{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module LvmRead( lvmReadFile, lvmRead ) where

import Prelude hiding (Read, readInt)
import Array
import PPrint   ( putDoc )
import Standard ( trace, assert, foldlStrict, getLvmPath, searchPath )
import Id       ( Id, stringFromId )
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


data Record     = RecValue    Id !Int (DValue [Instr])               
                | RecAbstract Id DAbstract
                | RecImport   Id DImport
                | RecCon      Id DCon
                | RecExtern   Id DExtern
                | RecCustom   Id DCustom
                
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

readModule :: Read (Module v,[Record])
readModule
  = do{ tag    <- readInt
      ; readGuard (tag == magic) "readHeader" ("invalid LVM file: magic number is incorrect")
      ; len    <- readInt
      ; total  <- readInt
      ; lvmmajor <- readInt
      ; lvmminor <- readInt
      ; readGuard (lvmmajor == lvmMajor && lvmminor >= lvmMinor) "readHeader" "invalid module version"
      ; major  <- readInt
      ; minor  <- readInt
      ; id     <- readNameIdx
      ; count  <- readInt
      ; bcount <- readInt
      ; recs   <- readRecords total [] 
      ; readGuard (count == length recs) "readModule" "incorrect record count"
      ; let (abstracts,imports,cons,externs,customs) = foldlStrict insert (emptyMap,[],emptyMap,emptyMap,emptyMap) recs
      ; return (Module id major minor [] abstracts cons externs customs imports, recs)
      }
  where
    insert x@(abstracts,imports,cons,externs,customs) rec
      = case rec of
          RecCustom id custom     | isPublic (customAccess custom) 
                                  -> (abstracts,imports,cons,externs,insertMap id custom customs)
          RecAbstract id abstract | isPublic (abstractAccess abstract)
                                  -> (insertMap id abstract abstracts,imports,cons,externs,customs)
          RecImport id imp        | isPublic (importAccess imp)
                                  -> (abstracts,(id,imp):imports,cons,externs,customs)
          RecCon id con           | isPublic (conAccess con)
                                  -> (abstracts,imports,insertMap id con cons,externs,customs)
          RecExtern id extern     | isPublic (externAccess extern)
                                  -> (abstracts,imports,cons,insertMap id extern externs,customs)
          other                   -> x
         
    isPublic acc
      = case acc of
          Private -> True
          Public  -> True
          Import public _ _ _ _ -> public

readFooter :: Read ()
readFooter
  = return ()

readRecords :: Int -> [Record] -> Read [Record]
readRecords total acc
  = do{ tag   <- readInt
      ; len   <- readInt
      ; if (tag == (magic+1))
         then do{ total' <- readInt
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
                          other -> readCustom tag len
                ; readRecords total (rec:acc)
                }
      }


{--------------------------------------------------------------
  declarations
--------------------------------------------------------------}
readValue :: Int -> Read Record
readValue len
  = do{ id     <- readNameIdx
      ; acc    <- readAccess
      ; arity  <- readInt
      ; enc    <- readEnclosing
      -- ; code   <- readCodeIdx
      ; readInt
      ; customs<- readCustoms (len - 20)
      ; return (RecAbstract id (DAbstract acc arity))
      }

readCon len
  = do{ id    <- readNameIdx
      ; acc   <- readAccess
      ; arity <- readInt
      ; tag   <- readInt
      ; customs <- readCustoms (len - 16)
      ; return (RecCon id (DCon acc arity tag customs))
      }

readImport len
  = do{ id    <- readNameIdx
      ; flags <- readInt
      ; (modid,major,minor) <- readModuleIdx
      ; impid <- readNameIdx
      ; kind  <- readInt
      ; customs <- readCustoms (len - 20)
      ; return (RecImport id (DImport (Import (odd flags) modid impid major minor) kind))
      }

readModuleRec len
  = do{ id    <- readNameIdx
      ; major <- readInt
      ; minor <- readInt
      ; customs <- readCustoms (len - 12)
      ; return (RecModule id major minor)
      }

readCustom :: DeclKind -> Int -> Read Record
readCustom kind len
  = do{ id  <- readNameIdx
      ; acc <- readAccess
      ; skip (len - 8)
      ; return (RecCustom id (DCustom acc kind (map CtmInt [])))
      }

readExtern :: Int -> Read Record
readExtern len
  = do{ id    <- readNameIdx
      ; acc   <- readAccess
      ; arity <- readInt
      ; tp    <- readExternTypeIdx
      ; libname <- readNameStringIdx
      ; idx   <- readInt
      ; mode  <- readInt
      ; link  <- readInt
      ; call  <- readInt
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
      ; return (RecExtern id (DExtern acc arity tp linkMode callMode libname name customs))
      }
                   
{--------------------------------------------------------------
  constants
--------------------------------------------------------------}
readCode :: Int -> Read Record
readCode len
  = do{ ints   <- mapM (const readInt) [1..div len 4]
      ; return (RecCode ints)
      }

readName :: Int -> Read Record
readName len
  = do{ bs <- readByteSeq len
      ; return (RecName (idFromString (stringFromByteList bs)))
      }

readBytes :: Int -> Read Record
readBytes len
  = do{ bs <- readByteSeq len
      ; return (RecBytes (bytesFromByteList bs))
      }

readExternType len
  = do{ bs <- readByteSeq len
      ; return (RecExternType (stringFromByteList bs))
      }

readByteSeq :: Int -> Read [Byte]
readByteSeq len
  = do{ blen <- readInt
      ; bs   <- readByteList blen
      ; skip (len - 4 - blen)      
      ; return bs
      }

readCustoms :: Int -> Read [Custom]
readCustoms len
  = do{ ints <- mapM (const readInt) [1..div len 4]
      ; return (map CtmInt ints)
      }

readAccess :: Read Access
readAccess
  = do{ flags <- readInt
      ; if (odd flags)
         then return Public
         else return Private
      }

{--------------------------------------------------------------
  indices
--------------------------------------------------------------}
readNameIdx :: Read Id
readNameIdx
  = do{ idx <- readInt
      ; if (idx == 0)
         then readFreshId
         else resolve idx (\rec -> case rec of 
                              RecName id  -> id
                              other       -> error "LvmRead.readName: invalid name index")
      }

readModuleIdx 
  = do{ idx <- readInt
      ; resolve idx (\rec -> case rec of
                               RecModule modid major minor -> (modid,major,minor)
                               other -> error "LvmRead.readModule: invalid module index")
      }

readExternTypeIdx
  = do{ idx <- readInt
      ; resolve idx (\rec -> case rec of
                               RecExternType tp -> tp
                               other  -> error "LvmRead.readExternType: invalid extern type index")
      }

readNameStringIdx
  = do{ idx <- readInt
      ; readNameString idx
      }

readNameString idx
  = resolve idx (\rec -> case rec of
                           RecName id   -> stringFromId id
                           RecBytes bs  -> stringFromBytes bs
                           other  -> error "LvmRead.readNameString: invalid name index")
              

readCodeIdx
  = do{ idx <- readInt
      ; resolve idx (\rec -> case rec of
                               RecCode code -> code
                               other        -> error "readCode" "invalid code index")
      }

readEnclosing
  = do{ idx  <- readInt
      ; if (idx == 0) 
          then return Nothing
          else resolve idx (\rec -> case rec of
                                     RecValue id _ _ -> Just id
                                     other           -> error "readEnclosing" "invalid enclosing index"
                          )
      }


                   

{--------------------------------------------------------------
  reader monad
--------------------------------------------------------------}
newtype Read a    = Read (Env -> State -> Result a)
type    Records   = Array Int Record
data    Result a  = Result a !State
data    Env       = Env   !FilePath Records
data    State     = State ![Byte] !NameSupply

unRead (Read r)   = r

runRead :: Read (a,[Record]) -> NameSupply -> FilePath -> [Byte]-> a
runRead (Read r) ns fname bs
  = let (Result (x,rs) st) = r (Env fname (listArray (1,length rs) rs)) (State bs ns)
    in x

instance Functor Read where
  fmap f (Read r) = Read (\env st -> case r env st of
                                       Result x st -> Result (f x) st)
instance Monad Read where
  return x        = Read (\rs bs -> Result x bs)
  (Read r) >>= f  = Read (\rs bs -> case r rs bs of
                                      Result x bsx -> unRead (f x) rs bsx) 

readTrace msg
  = trace (msg ++ "\n") (return ())

readInt :: Read Int
readInt 
  = Read (\env (State bs ns) -> case int32FromByteList bs of (i,cs) -> Result i (State cs ns))

readByteList :: Int -> Read [Byte]
readByteList n
  = Read (\env (State bs ns) -> case splitAt n bs of (xs,cs) -> Result xs (State cs ns))

skip n
  = Read (\env (State bs ns) -> Result () (State (drop n bs) ns))

readFreshId :: Read Id
readFreshId
  = Read (\env (State bs ns) -> let (id,ns') = freshId ns in Result id (State bs ns'))
  
readGuard :: Bool -> String -> String -> Read ()
readGuard test fun msg
  = if (test) then return () else readError fun msg

readError :: String -> String -> Read a
readError fun msg
  = Read (\(Env fname rs) st -> error ("LvmRead." ++ fun ++ ": \"" ++ fname ++ "\"\n  " ++ msg))

resolve :: Int -> (Record -> a) -> Read a
resolve idx f
  = Read (\(Env fname rs) st -> Result (f (rs ! idx)) st)