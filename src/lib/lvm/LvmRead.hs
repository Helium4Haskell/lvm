{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module LvmRead( lvmReadFile, lvmRead ) where

import Prelude hiding (Read)
import Array
import PPrint   ( putDoc )
import Standard ( assert, foldlStrict, getLvmPath, searchPath )
import Id       ( Id, stringFromId, idFromString, newNameSupply, freshId )
import IdMap

import Byte   hiding (readByteList)
import qualified Byte
import Lvm
import Instr
import InstrPretty ( instrPretty )
import ModulePretty( modulePretty )


  
{--------------------------------------------------------------
  Magic numbers
--------------------------------------------------------------}
magic, lvmMajor, lvmMinor :: Int
lvmMajor  = 9
lvmMinor  = 0
magic     = 0x4C564D58


data Record v   = RecDecl       (Decl v)
                
                | RecCode       ![Int]
                | RecBytes      !Bytes
                | RecName       Id
                | RecModule     Id !Int !Int
                | RecExternType !String
                
{--------------------------------------------------------------
  read an LVM file
--------------------------------------------------------------}
test src
  = do{ path    <- getLvmPath
      ; source  <- searchPath path ".lvm" src
      ; mod     <- lvmReadFile source
      ; putDoc (modulePretty instrPretty mod)
      }

lvmReadFile :: FilePath -> IO (Module v)
lvmReadFile fname
  = do{ bs <- Byte.readByteList fname
      ; ns <- newNameSupply
      ; return (lvmRead ns fname bs)
      }

lvmRead :: NameSupply -> FilePath -> [Byte] -> (Module v) 
lvmRead ns fname bs
  = runRead readModule ns fname bs

readModule :: Read v (Module v,[Record v])
readModule
  = do{ tag    <- readint
      ; readGuard (tag == magic) "readHeader" "magic number is incorrect"
      ; len    <- readint
      ; total  <- readint
      ; lvmmajor <- readint
      ; lvmminor <- readint
      ; readGuard (lvmmajor == lvmMajor && lvmminor >= lvmMinor) "readHeader" "invalid module version"
      ; major  <- readint
      ; minor  <- readint
      ; id     <- readNameIdx
      ; count  <- readint
      ; bcount <- readint
      ; recs   <- readRecords total [] 
      ; readGuard (count == length recs) "readModule" "incorrect record count"
      ; return (Module id major minor [d | RecDecl d <- filter isRecDecl recs, accessPublic (declAccess d)],recs)
      }
  where
    isRecDecl (RecDecl d) = True
    isRecDecl other       = False

readFooter :: Read v ()
readFooter
  = return ()

readRecords :: Int -> [Record v] -> Read v [Record v]
readRecords total acc
  = do{ tag   <- readint
      ; len   <- readint
      ; if (tag == (magic+1))
         then do{ total' <- readint
                ; readGuard (total==total') "readRecords" "footer doesn't match with header"
                ; return (reverse acc)
                }
         else do{ rec <- case tag of
                          0     -> readName len
                          1     -> readBytes len
                          2     -> readCode len
                          3     -> readValue len
                          4     -> readCon len
                          5     -> readImport len
                          6     -> readModuleRec len
                          7     -> readExtern len
                          8     -> readExternType len
                          other -> readCustom (toEnum tag) len
                ; readRecords total (rec:acc)
                }
      }


{--------------------------------------------------------------
  declarations
--------------------------------------------------------------}
readValue :: Int -> Read v (Record v)
readValue len
  = do{ id     <- readNameIdx
      ; acc    <- readAccess
      ; arity  <- readint
      ; enc    <- readEnclosing
      -- ; code   <- readCodeIdx
      ; readint
      ; customs<- readCustoms (len - 20)
      ; return (RecDecl (DeclAbstract id acc arity))
      }

readCon len
  = do{ id    <- readNameIdx
      ; acc   <- readAccess
      ; arity <- readint
      ; tag   <- readint
      ; customs <- readCustoms (len - 16)
      ; return (RecDecl (DeclCon id acc arity tag customs))
      }

readImport len
  = do{ id    <- readNameIdx
      ; flags <- readint
      ; ~(modid,major,minor) <- readModuleIdx
      ; impid <- readNameIdx
      ; kind  <- readint
      ; customs <- readCustoms (len - 20)
      ; return (RecDecl (DeclImport id (Imported (odd flags) modid impid (toEnum kind) major minor)))
      }

readModuleRec len
  = do{ id    <- readNameIdx
      ; major <- readint
      ; minor <- readint
      ; customs <- readCustoms (len - 12)
      ; return (RecModule id major minor)
      }

readCustom :: DeclKind -> Int -> Read v (Record v)
readCustom kind len
  = do{ id  <- readNameIdx
      ; acc <- readAccess
      ; skip (len - 8)
      ; return (RecDecl (DeclCustom id acc (kind) []))
      }

readExtern :: Int -> Read v (Record v)
readExtern len
  = do{ id    <- readNameIdx
      ; acc   <- readAccess
      ; arity <- readint
      ; tp    <- readExternTypeIdx
      ; libname <- readNameStringIdx
      ; idx   <- readint
      ; mode  <- readint
      ; link  <- readint
      ; call  <- readint
      ; customs <- readCustoms (len - 9*4)
      ; name  <- case mode of
                   1  -> fmap Decorate  (readNameString idx)
                   2  -> return (Ordinal idx)
                   _  -> fmap Plain     (readNameString idx)
      ; let linkMode = case link of
                         1 -> LinkDynamic
                         2 -> LinkRuntime
                         _ -> LinkStatic
            callMode = case call of
                         1 -> CallStd
                         2 -> CallInstr
                         _ -> CallC
      ; return (RecDecl (DeclExtern id acc arity tp linkMode callMode libname name customs))
      }
                   
{--------------------------------------------------------------
  constants
--------------------------------------------------------------}
readCode :: Int -> Read v (Record v)
readCode len
  = do{ ints   <- mapM (const readint) [1..div len 4]
      ; return (RecCode ints)
      }

readName :: Int -> Read v (Record v)
readName len
  = do{ bs <- readByteSeq len
      ; return (RecName (idFromString (stringFromByteList bs)))
      }

readBytes :: Int -> Read v (Record v)
readBytes len
  = do{ bs <- readByteSeq len
      ; return (RecBytes (bytesFromByteList bs))
      }

readExternType len
  = do{ bs <- readByteSeq len
      ; return (RecExternType (stringFromByteList bs))
      }

readByteSeq :: Int -> Read v [Byte]
readByteSeq len
  = do{ blen <- readint
      ; bs   <- readByteList blen
      ; skip (len - 4 - blen)      
      ; return bs
      }

readCustoms :: Int -> Read v [Custom]
readCustoms len
  = do{ ints <- mapM (const readint) [1..div len 4]
      ; return (map CustomInt ints)
      }

readAccess :: Read v Access
readAccess
  = do{ flags <- readint
      ; return (Defined (odd flags))
      }

{--------------------------------------------------------------
  indices
--------------------------------------------------------------}
readNameIdx :: Read v Id
readNameIdx
  = do{ idx <- readint
      ; if (idx == 0)
         then readFreshId
         else resolve idx (\rec -> case rec of 
                              RecName id  -> id
                              other       -> error "LvmRead.readName: invalid name index")
      }

readModuleIdx 
  = do{ idx <- readint
      ; resolve idx (\rec -> case rec of
                               RecModule modid major minor -> (modid,major,minor)
                               other -> error "LvmRead.readModule: invalid module index")
      }

readExternTypeIdx
  = do{ idx <- readint
      ; resolve idx (\rec -> case rec of
                               RecExternType tp -> tp
                               other  -> error "LvmRead.readExternType: invalid extern type index")
      }

readNameStringIdx
  = do{ idx <- readint
      ; readNameString idx
      }

readNameString idx
  = resolve idx (\rec -> case rec of
                           RecName id   -> stringFromId id
                           RecBytes bs  -> stringFromBytes bs
                           other  -> error "LvmRead.readNameString: invalid name index")
              

readCodeIdx
  = do{ idx <- readint
      ; resolve idx (\rec -> case rec of
                               RecCode code -> code
                               other        -> error "readCode" "invalid code index")
      }

readEnclosing
  = do{ idx  <- readint
      ; if (idx == 0) 
          then return Nothing
          else resolve idx (\rec -> case rec of
                                     RecDecl d  | isDeclValue d || isDeclAbstract d -> Just (declName d)
                                     other            -> error "readEnclosing" "invalid enclosing index"
                          )
      }


                   

{--------------------------------------------------------------
  Reader monad.
  Note the lazy recursive definition, where resolving
  and reading is done in a single pass (using delayed 
  computations).
--------------------------------------------------------------}
newtype Read v a  = Read (Env v -> State -> Result a)
type    Records v = Array Int (Record v)
data    Result a  = Result a !State
data    Env v     = Env   !FilePath (Records v)
data    State     = State ![Byte] !NameSupply

unRead (Read r)   = r

runRead :: Read v (a,[(Record v)]) -> NameSupply -> FilePath -> [Byte]-> a
runRead (Read r) ns fname bs
  = let (Result (x,rs) st) = r (Env fname (listArray (1,length rs) rs)) (State bs ns)
    in x

instance Functor (Read v) where
  fmap f (Read r) = Read (\env st -> case r env st of
                                       Result x st -> Result (f x) st)
instance Monad (Read v) where
  return x        = Read (\rs bs -> Result x bs)
  (Read r) >>= f  = Read (\rs bs -> case r rs bs of
                                      Result x bsx -> unRead (f x) rs bsx) 

readint :: Read v Int
readint 
  = Read (\env (State bs ns) -> case int32FromByteList bs of (i,cs) -> Result i (State cs ns))

readByteList :: Int -> Read v [Byte]
readByteList n
  = Read (\env (State bs ns) -> case splitAt n bs of (xs,cs) -> Result xs (State cs ns))

skip n
  = Read (\env (State bs ns) -> Result () (State (drop n bs) ns))

readFreshId :: Read v Id
readFreshId
  = Read (\env (State bs ns) -> let (id,ns') = freshId ns in Result id (State bs ns'))
  
readGuard :: Bool -> String -> String -> Read v ()
readGuard test fun msg
  = if (test) then return () else readError fun msg

readError :: String -> String -> Read v a
readError fun msg
  = Read (\(Env fname rs) st -> error ("LvmRead." ++ fun ++ ": \"" ++ fname ++ "\"\n  " ++ msg))

resolve :: Int -> (Record v -> a) -> Read v a
resolve idx f
  = Read (\(Env fname rs) st -> Result (f (rs ! idx)) st)