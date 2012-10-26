--------------------------------------------------------------------------------
-- Copyright 2001-2012, Bastiaan Heeren, Jurriaan Hage, Daan Leijen. This file 
-- is distributed under the terms of the GNU General Public License. For more 
-- information, see the file "LICENSE.txt", which is included in the 
-- distribution.
--------------------------------------------------------------------------------
--  $Id$

module Lvm.Read (lvmReadFile, lvmRead) where

import Control.Monad
import Data.Array
import Lvm.Common.Byte hiding (readByteList)
import Lvm.Common.Id
import Lvm.Data
import Lvm.Instr.Data
import Prelude hiding (Read)
import qualified Lvm.Common.Byte as Byte

{--------------------------------------------------------------
  Magic numbers
--------------------------------------------------------------}
lvmMajor, lvmMinor :: Int
lvmMajor  = 15
lvmMinor  = 0

data Record v   = RecDecl       (Decl v)
                
                | RecName       Id
                | RecKind       Id
                | RecBytes      !Bytes
                | RecCode       ![Int]
                | RecModule     Id !Int !Int [Custom]
                | RecExternType !String
                | RecAnon       !DeclKind [Custom]
                
{--------------------------------------------------------------
  read an LVM file
--------------------------------------------------------------}

{-
test src
  = do{ path    <- getLvmPath
      ; source  <- searchPath path ".lvm" src
      ; mod     <- lvmReadFile source
      ; putDoc (modulePretty instrPretty mod)
      }  -}

lvmReadFile :: FilePath -> IO (Module v)
lvmReadFile fname
  = do{ bs <- Byte.readByteList fname
      ; ns <- newNameSupply
      ; return (lvmRead ns fname bs)
      }

lvmRead :: NameSupply -> FilePath -> [Byte] -> Module v
lvmRead = runRead readModule 

readModule :: Read v (Module v,[Record v])
readModule
  = do{ tag    <- readRaw
      ; readGuard (tag == recHeader) "readHeader" "magic number is incorrect"
      ; _len     <- readint
      ; total    <- readint
      ; lvmmajor <- readint
      ; lvmminor <- readint
      ; readGuard (lvmmajor == lvmMajor && lvmminor >= lvmMinor) "readHeader" ("incompatible lvm version " ++ show lvmmajor ++ "." ++ show lvmminor)
      ; count    <- readint
      ; _bcount  <- readint
      ; ~(x,major,minor)  <- readModuleIdx 
      ; recs   <- readRecords total [] 
      ; readGuard (count == length recs) "readModule" "incorrect record count"
      ; return (Module x major minor [d | RecDecl d <- filter isRecDecl recs],recs)
      }
  where
    isRecDecl (RecDecl _) = True
    isRecDecl _           = False

readRecords :: Int -> [Record v] -> Read v [Record v]
readRecords total acc
  = do{ x     <- readRaw
      ; len   <- readint
      ; if x == recFooter
         then do{ total' <- readint
                ; readGuard (total==total') "readRecords" "footer doesn't match with header"
                ; return (reverse acc)
                }
        else if isInt x
         then do{ let tag = decodeInt x 
                ; rec_ <- case tag of
                          0     -> readName len
                          1     -> readKind len
                          2     -> readBytes len
                          3     -> readCode len
                          4     -> readValue len
                          5     -> readCon len
                          6     -> readImport len
                          7     -> readModuleRec len
                          8     -> readExtern len
                          9     -> readExternType len
                          _ -> readError "readRecords" ("unknown standard record kind (" ++ show tag ++ ")")
                ; readRecords total (rec_:acc)
                }
         else do{ let idx = decodeIdx x
                ; rec_ <- readDeclCustom idx len 
                ; readRecords total (rec_:acc)
                }
      }


{--------------------------------------------------------------
  declarations
--------------------------------------------------------------}
readValue :: Int -> Read v (Record v)
readValue len
  = do{ x      <- readNameIdx "value"
      ; acc    <- readAccess
      ; arity  <- readint
      ; _      <- readEnclosing
      ; _      <- readIdx "code"
      ; customs<- readCustoms (len - 20)
      ; return (RecDecl (DeclAbstract x acc arity customs))
      }

readCon :: Int -> Read v (Record a)
readCon len
  = do{ x     <- readNameIdx "constructor"
      ; acc   <- readAccess
      ; arity <- readint
      ; tag   <- readint
      ; customs <- readCustoms (len - 16)
      ; return (RecDecl (DeclCon x acc arity tag customs))
      }

readImport :: Int -> Read v (Record v1)
readImport len
  = do{ x     <- readNameIdx "import"
      ; flags <- readint
      ; ~(modid,major,minor) <- readModuleIdx
      ; impid <- readNameIdx "imported"
      ; kind  <- readKindIdx
      ; customs <- readCustoms (len - 20)
      ; return (RecDecl (DeclImport x (Imported (odd flags) modid impid kind major minor) customs))
      }

readKindIdx :: Read v DeclKind
readKindIdx
  = do{ xkind <- readRaw
      ; if isInt xkind
         then return (toEnum (decodeInt xkind))
         else do{ kindid <- resolveKindIdx (decodeIdx xkind)
                ; return (DeclKindCustom kindid)
                }
      }
readModuleRec :: Int -> Read v (Record a)
readModuleRec len
  = do{ x     <- readNameIdx "module"
      ; major <- readint
      ; minor <- readint
      ; customs <- readCustoms (len - 12)
      ; return (RecModule x major minor customs)
      }

readDeclCustom :: Index -> Int -> Read v (Record v)
readDeclCustom kindIdx len
  = do{ kindid  <- resolveKindIdx kindIdx
      ; mbId    <- readCustomNameIdx
      ; case mbId of
          Just x   -> do{ acc     <- readAccess
                        ; customs <- readCustoms (len-8)
                        ; return (RecDecl (DeclCustom x acc (DeclKindCustom kindid) customs))
                        }
          Nothing  -> do{ customs <- readCustoms (len-4)
                        ; return (RecAnon (DeclKindCustom kindid) customs)
                        }
      }

readExtern :: Int -> Read v (Record v)
readExtern len
  = do{ x     <- readNameIdx "extern"
      ; acc   <- readAccess
      ; arity <- readint
      ; tp    <- readExternTypeIdx
      ; libname <- readNameStringIdx
      ; xname <- readRaw 
      ; mode  <- readint
      ; link  <- readint
      ; call  <- readint
      ; customs <- readCustoms (len - 9*4)
      ; name  <- case mode of
                   1  -> fmap Decorate  (readNameString (decodeIdx xname))
                   2  -> return (Ordinal (decodeInt xname))
                   _  -> fmap Plain     (readNameString (decodeIdx xname))
      ; let linkMode = case link of
                         1 -> LinkDynamic
                         2 -> LinkRuntime
                         _ -> LinkStatic
            callMode = case call of
                         1 -> CallStd
                         2 -> CallInstr
                         _ -> CallC
      ; return (RecDecl (DeclExtern x acc arity tp linkMode callMode libname name customs))
      }
                   
{--------------------------------------------------------------
  constants
--------------------------------------------------------------}
readCode :: Int -> Read v (Record v)
readCode len
  = do{ ints   <- mapM (const readRaw) [1..div len 4]
      ; return (RecCode ints)
      }

readName :: Int -> Read v (Record v)
readName len
  = do{ bs <- readByteSeq len
      ; return (RecName (idFromString (stringFromByteList bs)))
      }

readKind :: Int -> Read v (Record v)
readKind len
  = do{ bs <- readByteSeq len
      ; return (RecKind (idFromString (stringFromByteList bs)))
      }

readBytes :: Int -> Read v (Record v)
readBytes len
  = do{ bs <- readByteSeq len
      ; return (RecBytes (bytesFromByteList bs))
      }

readExternType :: Int -> Read v (Record a)
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
  = mapM (const readCustom) [1..div len 4]
    
readAccess :: Read v Access
readAccess
  = do{ flags <- readint
      ; return (Defined (odd flags))
      }

readCustom :: Read v Custom
readCustom
  = do{ x <- readRaw
      ; if isInt x
         then return (CustomInt (decodeInt x))
        else if decodeIdx x == 0
         then return CustomNothing
         else resolve (decodeIdx x) recToCustom
      }
  where
    recToCustom rec_
      = case rec_ of
          RecName x         -> CustomName x
          RecBytes bs       -> CustomBytes bs
          RecDecl d         -> CustomLink (declName d) (declKindFromDecl d)
          RecAnon kind cs   -> CustomDecl kind cs
          _                 -> error "LvmRead.readCustom: invalid link"


{--------------------------------------------------------------
  indices
--------------------------------------------------------------}
readNameIdx :: String -> Read v Id
readNameIdx parent
  = do{ idx <- readIdx (parent ++ ".name")
      ; if idx == 0
         then readFreshId
         else resolve idx (\rec_ -> case rec_ of 
                              RecName x  -> x
                              _          -> error "LvmRead.readName: invalid name index")
      }

readCustomNameIdx :: Read v (Maybe Id)
readCustomNameIdx
  = do{ idx <- readIdx "custom name"
      ; if idx==0
         then return Nothing
         else do{ x1 <- resolve idx (\rec_ -> case rec_ of 
                                               RecName x2  -> x2
                                               _       -> error "LvmRead.readCustomNameIdx: invalid name index")
                ; return (Just x1)
                }
      }

resolveKindIdx :: Index -> Read v Id
resolveKindIdx idx
  = resolve idx (\rec_ -> case rec_ of 
                          RecKind x  -> x
                          _       -> error "LvmRead.resolveKindIdx: invalid kind index")

readModuleIdx :: Read v (Id, Int, Int)
readModuleIdx 
  = do{ idx <- readIdx "module descriptor"
      ; resolve idx (\rec_ -> case rec_ of
                               RecModule modid major minor _ -> (modid,major,minor)
                               _ -> error "LvmRead.readModule: invalid module index")
      }

readExternTypeIdx :: Read v String
readExternTypeIdx
  = do{ idx <- readIdx "extern type"
      ; resolve idx (\rec_ -> case rec_ of
                               RecExternType tp -> tp
                               _  -> error "LvmRead.readExternType: invalid extern type index")
      }

readNameStringIdx :: Read v String
readNameStringIdx
  = do{ idx <- readIdx "name string"
      ; readNameString idx
      }

readNameString :: Int -> Read v String
readNameString idx 
  = resolve idx (\rec_ -> case rec_ of
                           RecName x    -> stringFromId x
                           RecBytes bs  -> stringFromBytes bs
                           _  -> error "LvmRead.readNameString: invalid name index")

readEnclosing :: Read a (Maybe Id)
readEnclosing
  = do{ idx  <- readIdx "enclosing"
      ; if idx == 0
          then return Nothing
          else resolve idx (\rec_ -> case rec_ of
                                     RecDecl d  | isDeclValue d || isDeclAbstract d -> Just (declName d)
                                     _            -> error "readEnclosing" "invalid enclosing index"
                          )
      }



readint :: Read v Int
readint 
  = do{ i <- readRaw
      ; readGuard (isInt i) "readint" "expecting integer but found index"
      ; return (decodeInt i)
      }
readIdx :: String -> Read v Int
readIdx name
  = do{ i <- readRaw
      ; readGuard (isIdx i) "readIdx" ("expecting index but found integer (" ++ name ++ ")")
      ; return (decodeIdx i)
      }

isInt, isIdx :: Int -> Bool
isInt = odd
isIdx = even

decodeInt, decodeIdx :: Int -> Int
decodeInt i = (i-1) `div` 2
decodeIdx i = i `div` 2

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

unRead :: Read a b -> Env a -> State -> Result b
unRead (Read r)   = r

runRead :: Read v (a,[Record v]) -> NameSupply -> FilePath -> [Byte]-> a
runRead (Read r) ns fname bs
  = let (Result (x,rs) _) = r (Env fname (listArray (1,length rs) rs)) (State bs ns)
    in x

instance Functor (Read v) where
  fmap f (Read r) = Read (\env st1 -> case r env st1 of
                                        Result x st2 -> Result (f x) st2)
instance Monad (Read v) where
  return x        = Read (\_  bs -> Result x bs)
  (Read r) >>= f  = Read (\rs bs -> case r rs bs of
                                      Result x bsx -> unRead (f x) rs bsx) 


readRaw :: Read v Int
readRaw 
  = Read (\_ (State bs ns) -> case int32FromByteList bs of (i,cs) -> Result i (State cs ns))

readByteList :: Int -> Read v [Byte]
readByteList n
  = Read (\_ (State bs ns) -> case splitAt n bs of (xs,cs) -> Result xs (State cs ns))

skip :: Int -> Read v ()
skip n
  = Read (\_ (State bs ns) -> Result () (State (drop n bs) ns))

readFreshId :: Read v Id
readFreshId
  = Read (\_ (State bs ns) -> let (x,ns') = freshId ns in Result x (State bs ns'))
  
readGuard :: Bool -> String -> String -> Read v ()
readGuard test fun = unless test . readError fun

readError :: String -> String -> Read v a
readError fun msg
  = Read (\(Env fname _) _ -> error ("LvmRead." ++ fun ++ ": \"" ++ fname ++ "\"\n  " ++ msg))

resolve :: Int -> (Record v -> a) -> Read v a
resolve idx f
  = Read (\(Env _ rs) st -> Result (f (rs ! idx)) st)