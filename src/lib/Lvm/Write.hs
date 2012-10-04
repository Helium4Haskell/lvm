{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

module Lvm.Write (lvmWriteFile, lvmToBytes) where

import qualified Control.Exception as CE (assert, catch, IOException) 
import Control.Monad
import Data.Maybe
import Lvm.Common.Byte
import Lvm.Common.Id 
import Lvm.Common.IdMap
import Lvm.Data
import Lvm.Instr.Data
import System.Exit 

{--------------------------------------------------------------
  Magic numbers
--------------------------------------------------------------}
lvmMajorVersion,lvmMinorVersion :: Int
lvmMajorVersion  = 15
lvmMinorVersion  = 0

{--------------------------------------------------------------
  Emit an LVM file
--------------------------------------------------------------}
lvmWriteFile :: FilePath -> LvmModule -> IO ()
lvmWriteFile path lvm
  = let bytes = lvmToBytes lvm
    in seq bytes $
        writeBytes path bytes `CE.catch` (\exception ->
            let message = show (exception :: CE.IOException) ++ "\n\nUnable to write to file " ++ show path
            in do { putStrLn message; exitWith (ExitFailure 1) })

lvmToBytes :: LvmModule -> Bytes
lvmToBytes m
  = let (idxInfo,recs) = bytesFromModule m
        headerlen = 24
        header    = block
                    [ recHeader
                    , encodeInt headerlen
                    , encodeInt totallen
                    , encodeInt lvmMajorVersion
                    , encodeInt lvmMinorVersion
                    , encodeInt (length recs)
                    , encodeInt (bytesLength brecs)
                    , encodeIdx idxInfo
                    ]

        footerlen = 4
        footer    = block [ recFooter, encodeInt footerlen, encodeInt totallen ]

        brecs     = mconcat recs 
        totallen  = bytesLength brecs + headerlen + 8 + footerlen + 8
        total     = mconcat [header,brecs,footer]
    in seq totallen total

bytesFromModule :: LvmModule -> (Index,[Bytes])
bytesFromModule = runEmit . emitLvmModule


emitLvmModule :: LvmModule -> Emit Index
emitLvmModule m
  = do{ idxInfo <- emitModule (moduleName m) (moduleMajorVer m) (moduleMinorVer m)
      ; mapM_ emitDecl (moduleDecls m)
      ; return idxInfo
      }

{--------------------------------------------------------------
  emit declarations
  TODO: emit  assumes some canonical order: We should do
  allocation of named blocks first and than emit to fix this.
--------------------------------------------------------------}
flags :: Access -> Int
flags access = if accessPublic access then 1 else 0

isImported :: Decl v -> Bool
isImported decl
  = case declAccess decl of
      Imported{}  -> True
      _           -> False

emitDecl :: Decl [Instr] -> Emit Index
emitDecl DeclExtern{ externCall = CallInstr }  
  = return 0
emitDecl decl
  | isImported decl   = emitImport (declName decl) (declKindFromDecl decl) (declAccess decl) []
emitDecl decl
  = case decl of
      DeclValue{}     -> emitDValue decl
      DeclAbstract{}  -> emitDAbstract decl
      DeclCon{}       -> emitDCon decl
      DeclExtern{}    -> emitDExtern decl
      DeclCustom{}    -> emitDCustom decl
      _               -> error "LvmWrite.emitDecl: invalid declaration at this phase"

emitDValue :: Decl [Instr] -> Emit Index
emitDValue (DeclValue x access mbEnc instrs custom)
  = do{ idxEnc  <- maybe (return 0) (findIndex DeclKindValue) mbEnc
      ; idxCode <- emitInstrs instrs
      ; emitNamedBlock x DeclKindValue [encodeInt (flags access), encodeInt arity
                                        ,encodeIdx idxEnc, encodeIdx idxCode] custom
      }
  where
    arity = case instrs of
              (ARGCHK n:_    ) -> n
              _                -> error ("LvmWrite.emitDecl: instructions do not start with an argument check: " ++ show x)
emitDValue _ = error "Lvm.Write"

emitDCon :: Decl a -> Emit Index
emitDCon (DeclCon x access arity tag custom)
  = emitNamedBlock x DeclKindCon [encodeInt (flags access),encodeInt arity,encodeInt tag] custom
emitDCon _ = error "Lvm.Write"

emitDCustom :: Decl a -> Emit Index
emitDCustom (DeclCustom x access kind custom)
  = emitNamedBlock x kind [encodeInt (flags access)] custom
emitDCustom _ = error "Lvm.Write"

emitDExtern :: Decl a -> Emit Index
emitDExtern (DeclExtern x access arity tp linkconv callconv libname externname custom)
  = do{ idxType            <- emitExternType tp
      ; idxLibName         <- emitNameString libname
      ; (nameFlag,idxName) <- emitNameExtern
      ; idxId              <- emitName x
      ; emitBlockEx (Just x) DeclKindValue DeclKindExtern  
            (block [encodeIdx idxId, encodeInt (flags access), encodeInt arity
                    ,encodeIdx idxType
                    ,encodeIdx idxLibName
                    ,idxName      -- already encoded
                    ,encodeInt nameFlag
                    ,encodeInt (fromEnum linkconv)
                    ,encodeInt (fromEnum callconv)
                    ]) custom
      }
  where
    emitNameExtern  = case externname of
                        Plain s    -> do{ idx <- emitNameString s; return (0,encodeIdx idx) }
                        Decorate s -> do{ idx <- emitNameString s; return (1,encodeIdx idx) }
                        Ordinal i  -> return (2,encodeInt i)
emitDExtern _ = error "Lvm.Write"

emitDAbstract :: Decl a -> b
emitDAbstract _ = error "LvmWrite.emitDAbstract: abstract values should be imported"

emitImport :: Id -> DeclKind -> Access -> [Custom] -> Emit Index
emitImport x declkind access@(Imported _ modName impName kind majorVer minorVer) customs
  = CE.assert (declkind==kind) $ -- LvmWrite.emitImport: kinds don't match
    do{ idxModule <- emitModule modName majorVer minorVer
      ; idxName   <- emitName impName
      ; idxId     <- emitName x
      ; kindenc   <- encodeKind declkind
      ; emitBlockEx (Just x) declkind DeclKindImport 
          (block [encodeIdx idxId, encodeInt (flags access), encodeIdx idxModule
                 , encodeIdx idxName, kindenc]) customs
      }
emitImport _ _ _ _ = error "LvmWrite.emitImport: unknown case"

emitModule :: Id -> Int -> Int -> Emit Index
emitModule name major minor
  = do{ idxName <- emitName name
      ; emitBlock Nothing DeclKindModule (block [encodeIdx idxName,encodeInt major,encodeInt minor]) []
      }

{--------------------------------------------------------------
  emit instructions
--------------------------------------------------------------}
emitInstrs :: [Instr] -> Emit Index
emitInstrs instrs
  = do{ rinstrs <- mapM resolve instrs
      ; let codes = concatMap emit rinstrs
      ; emitBlock Nothing DeclKindCode (block codes) []
      }
{-
emitCode :: (Id,LvmValue) -> Emit Index
emitCode (id,DValue access mbEnc instrs custom)
  = do{ idx     <- findIndex id
      ; rinstrs <- mapM resolve instrs
      ; let codes = concatMap emit rinstrs
      ; emitBlock Nothing recCode (block (idx:codes)) []
      }

emitCode other
  = return 0
-}

{--------------------------------------------------------------
  emit an instruction
--------------------------------------------------------------}

emits :: [Instr] -> [Int]
emits = concatMap emit

emit :: Instr -> [Int]
emit instr
  = let opcode   = opcodeFromInstr instr
        illegal  = error ("LvmWrite.emit: illegal instruction at this phase: " ++ show (nameFromInstr instr))
        todo     = error ("LvmWrite.emit: todo: " ++ show (nameFromInstr instr))
    in case instr of
      -- pseudo instructions
      VAR _                   -> []
      PARAM _                 -> []
      USE _                   -> []
      NOP                     -> illegal
      RESULT _                -> illegal

      -- structured instructions
      MATCH alts              -> opcode : emitMatch 3 alts
      MATCHCON alts           -> opcode : emitMatch 2 alts
      MATCHINT alts           -> opcode : emitMatch 2 alts
      SWITCHCON _             -> todo

      EVAL _ is               -> let scrut = emits is
                                 in  emit (PUSHCONT (length scrut)) ++ scrut

      -- push instructions
      PUSHVAR     var         -> [opcode, offsetFromVar var]
      PUSHINT     n           -> [opcode, n]
      PUSHBYTES   _ c         -> [opcode, c]
      PUSHFLOAT   _           -> todo
      PUSHCODE    global      -> [opcode, indexFromGlobal global]
      PUSHCONT    ofs         -> [opcode, ofs]

      -- stack instructions
      ARGCHK      n           -> [opcode, n]
      SLIDE       n m _       -> [opcode, n, m]
      STUB        var         -> [opcode, offsetFromVar var]

      -- control
      ENTER                   -> [opcode]
      PUSHCATCH               -> [opcode]
      RAISE                   -> [opcode]
      CALL        global      -> [opcode, indexFromGlobal global, arityFromGlobal global]

      ENTERCODE   global      -> [opcode, indexFromGlobal global]
      EVALVAR     var         -> [opcode, offsetFromVar var]

      RETURN                  -> [opcode]
      RETURNCON   con         -> [opcode, indexFromCon con, arityFromCon con]
      RETURNINT   i           -> [opcode, i]

      -- applications
      ALLOCAP     arity       -> [opcode, arity]
      PACKAP      var arity   -> [opcode, offsetFromVar var, arity]
      PACKNAP     var arity   -> [opcode, offsetFromVar var, arity]
      NEWAP       arity       -> [opcode, arity]
      NEWNAP      arity       -> [opcode, arity]

      -- constructors
      ALLOCCON    con         -> [opcode, indexFromCon con, arityFromCon con]
      PACKCON     con var     -> [opcode, offsetFromVar var, arityFromCon con] --TODO: constant instead of arity

      NEWCON      con         -> [opcode, indexFromCon con, arityFromCon con]                                 

      NEW         arity       -> [opcode, arity]
      PACK        arity v     -> [opcode, arity, offsetFromVar v]
      UNPACK      arity       -> [opcode, arity]

      -- optimized instructions
      PUSHVARS2   v w         -> [opcode, offsetFromVar v, offsetFromVar w]

      NEWCON0 con             -> [opcode, indexFromCon con]
      NEWCON1 con             -> [opcode, indexFromCon con]
      NEWCON2 con             -> [opcode, indexFromCon con]
      NEWCON3 con             -> [opcode, indexFromCon con]

      RETURNCON0 con          -> [opcode, indexFromCon con]

      -- single opcode instructions
      _                       -> [opcode]

emitMatch :: Int -> [Alt] -> [Int]
emitMatch entrySize alts
  = CE.assert (normalizedAlts alts) $ -- "LvmWrite.emitMatch: unnormalized alternatives"
    let (pats,iss) = unzipAlts alts
        altis      = map emits iss
        start      = 2 + entrySize*(length alts -1)
    in [length alts-1]
       ++ matches start (zip pats (map length altis))
       ++ concat altis       
    where
      matches _ [] = []
      matches top ((pat,n):xs)
        = (case pat of
             PatCon con -> [indexFromCon con]
             PatInt i   -> [i]
             PatTag t a -> [t,a]
             PatDefault -> [])
          ++ [if n==0 then 0 else top] ++ matches (top+n) xs


normalizedAlts :: [Alt] -> Bool
normalizedAlts alts
  = case alts of
      Alt PatDefault _:_ -> True
      _                  -> False

unzipAlts :: [Alt] -> ([Pat], [[Instr]])
unzipAlts alts
  = unzip (map (\(Alt pat expr) -> (pat,expr)) alts)


{--------------------------------------------------------------
  resolve instructions
--------------------------------------------------------------}

resolves :: ([Instr] -> a) -> [Instr] -> Emit a
resolves f is
  = do{ ris <- mapM resolve is
      ; return (f ris)
      }

resolve :: Instr -> Emit Instr
resolve instr
  = case instr of
      EVAL d is       -> resolves (EVAL d) is
      RESULT is       -> resolves RESULT is
      MATCH    alts   -> resolveAlts MATCH alts
      MATCHCON alts   -> resolveAlts MATCHCON alts
      MATCHINT alts   -> resolveAlts MATCHINT alts
      SWITCHCON alts  -> resolveAlts SWITCHCON alts

      PUSHCODE global -> resolveGlobal PUSHCODE global
      ENTERCODE global-> resolveGlobal ENTERCODE global
      CALL global     -> resolveGlobal CALL global

      RETURNCON con   -> resolveCon RETURNCON con
      ALLOCCON con    -> resolveCon ALLOCCON con
      NEWCON con      -> resolveCon NEWCON con
      PACKCON con var -> resolveCon (`PACKCON` var) con

      PUSHBYTES bs _  -> resolveBytes (PUSHBYTES bs) bs

      -- optimized instructions
      RETURNCON0 con  -> resolveCon RETURNCON0 con
      NEWCON0 con     -> resolveCon NEWCON0 con
      NEWCON1 con     -> resolveCon NEWCON1 con
      NEWCON2 con     -> resolveCon NEWCON2 con
      NEWCON3 con     -> resolveCon NEWCON3 con

      _               -> return instr

resolveAlts :: ([Alt] -> a) -> [Alt] -> Emit a
resolveAlts f = liftM f . mapM resolveAlt

resolveAlt :: Alt -> Emit Alt
resolveAlt (Alt pat is)
  = do{ pat' <- resolvePat pat
      ; resolves (Alt pat') is
      }

resolvePat :: Pat -> Emit Pat
resolvePat pat
  = case pat of
      PatCon con  -> resolveCon PatCon con
      _           -> return pat

resolveGlobal :: (Global -> a) -> Global -> Emit a
resolveGlobal f (Global x _ arity)
  = do{ idx <- findIndex DeclKindValue x
      ; return (f (Global x idx arity))
      }

resolveCon :: (Con -> a) -> Con -> Emit a
resolveCon f (Con x _ arity tag)
  = do{ idx <- findIndex DeclKindCon x
      ; return (f (Con x idx arity tag))
      }

resolveBytes :: (Index -> a) -> Bytes -> Emit a
resolveBytes f bs
  = do{ idx <- emitBytes bs
      ; return (f idx)
      }
      
{--------------------------------------------------------------
  basic entities
--------------------------------------------------------------}
emitNamedBlock :: Id -> DeclKind -> [Int] -> [Custom] -> Emit Index
emitNamedBlock x kind is custom
  = do{ idxName <- emitName x
      ; emitBlock (Just x) kind (block (encodeIdx idxName:is)) custom
      }

emitName :: Id -> Emit Index
emitName x
  = emitNameString (stringFromId x)

emitNameString :: String -> Emit Index
emitNameString s
  = emitBlock Nothing DeclKindName (blockString s) []

emitExternType :: String -> Emit Index
emitExternType tp
  = emitBlock Nothing DeclKindExternType (blockString tp) []

emitKind :: Id -> Emit Index
emitKind x
  = emitBlock Nothing DeclKindKind (blockString (stringFromId x)) []

emitBytes :: Bytes -> Emit Index
emitBytes bs
  = emitBlock Nothing DeclKindBytes (blockBytes bs) []

emitBlock :: Maybe Id -> DeclKind -> Bytes -> [Custom] -> Emit Index
emitBlock mbId kind = emitBlockEx mbId kind kind

emitBlockEx :: Maybe Id -> DeclKind -> DeclKind -> Bytes -> [Custom] -> Emit Index
emitBlockEx mbId kindId kind bs custom
  = do{ bcustom <- emitCustoms custom
      ; kindenc <- encodeKind kind
      ; let bytes = mappend bs bcustom
            total = mappend (block [kindenc,encodeInt (bytesLength bytes)]) bytes
      ; CE.assert ((bytesLength bytes `mod` 4) == 0) $ -- "LvmWrite.emitBlock: unaligned size"
        emitPrimBlock (maybe Nothing (\x -> Just (x,kindId)) mbId) kind total
      }

encodeKind :: DeclKind -> Emit Int
encodeKind (DeclKindCustom x)
  = do{ idx <- emitKind x
      ; return (encodeIdx idx)
      }

encodeKind kind
  = return (encodeInt (fromEnum kind))


{--------------------------------------------------------------
  custom fields
--------------------------------------------------------------}
emitCustoms :: [Custom] -> Emit Bytes
emitCustoms decls
  = do{ is <- mapM emitCustom decls
      ; return (block is)
      }

emitCustom :: Custom -> Emit Int
emitCustom custom
  = case custom of
      CustomInt i         -> return (encodeInt i)
      CustomNothing       -> return (encodeIdx 0)
      CustomBytes bs      -> do{ idx <- emitBytes bs; return (encodeIdx idx) }
      CustomName x        -> do{ idx <- emitName x; return (encodeIdx idx) }
      CustomLink x kind   -> do{ idx <- findIndex kind x; return (encodeIdx idx) }
      CustomDecl kind cs  -> do{ idx <- emitAnonymousCustom kind cs; return (encodeIdx idx) }

emitAnonymousCustom :: DeclKind -> [Custom] -> Emit Index
emitAnonymousCustom kind
  = emitBlock Nothing kind (block [encodeIdx 0])


{--------------------------------------------------------------
  Emit Monad
--------------------------------------------------------------}
newtype Emit a  = Emit (Env -> State -> (a,State))
data State      = State !Int Env [Bytes]
type Env        = IdMap [(DeclKind,Index)]

instance Functor Emit where
  fmap f (Emit e)   = Emit (\env st -> case e env st of
                                         (x,stx) -> (f x,stx))

instance Monad Emit where
  return x          = Emit (\_   st -> (x,st))
  (Emit e) >>= f    = Emit (\env st -> case e env st of
                                         (x,stx) -> case f x of
                                                      Emit ef -> ef env stx)

runEmit :: Emit a -> (a,[Bytes])
runEmit (Emit e)
  = let (x,State _ env bbs) = e env (State 0 emptyMap [])  -- yes, a recursive, lazy, env :-)
    in (x,reverse bbs)

emitPrimBlock :: Maybe (Id,DeclKind) -> DeclKind -> Bytes -> Emit Index
emitPrimBlock mpair kind bs
  = Emit (\_ (State count m1 bbs) ->
            let (index,count2,bbs2) | sharable kind = case find count bs bbs of   --try to share records
                                                        Nothing  -> (count+1,count+1,bs:bbs)
                                                        Just idx -> (idx,count,bbs)
                                    | otherwise     = (count+1,count+1,bs:bbs)
                m2                = case mpair of
                                        Just (x,kindid) -> insertMapWith x [(kindid,index)] ((kindid,index):) m1
                                        Nothing         -> m1
            in (index, State count2 m2 bbs2)
         )

sharable :: DeclKind -> Bool
sharable _ = False
{-
  = case kind of
      DeclKindBytes       -> True
      DeclKindName        -> True
      DeclKindKind        -> True
      DeclKindExternType  -> True
      DeclKindModule      -> True   -- dubious when custom values are present!
      other               -> False
-}

find :: Eq a => Int -> a -> [a] -> Maybe Int
find n _ []       = CE.assert (n==0) Nothing -- "LvmWrite.find: count too large"
find n x (y:ys)   | x==y       = Just n
                  | otherwise  = (find $! (n-1)) x ys

-- a nice lazy formulation, we can calculate all indices before writing the bytes.
findIndex :: DeclKind -> Id -> Emit Index
findIndex kind x 
  = Emit (\env st ->
          (case lookupMap x env of
             Nothing  -> error ("LvmWrite.findIndex: undeclared identifier: " ++ show (stringFromId x))
             Just xs  -> fromMaybe (error msg) (lookup kind xs)
          , st))
 where
   msg = "LvmWrite.findIndex: undeclared identifier (with the right declaration kind): " ++ show (stringFromId x)



{--------------------------------------------------------------
  block
--------------------------------------------------------------}
block :: [Int] -> Bytes
block is
  = mconcat (map bytesFromInt32 is)

blockString :: String -> Bytes
blockString s
  = blockBytes (bytesFromString s)

blockBytes :: Bytes -> Bytes
blockBytes bs
  = let len = bytesLength bs
    in mconcat [bytesFromInt32 (encodeInt len), bs, padding len]

padding :: Int -> Bytes
padding n
  = let m = div (n + 3) 4 * 4
    in bytesFromList (replicate (m - n) (byteFromInt8 0))

encodeInt, encodeIdx :: Int -> Int
encodeInt i = (2*i)+1
encodeIdx i = 2*i
