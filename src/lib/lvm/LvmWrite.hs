{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

-- TODO: the single IdMap can lead to name clashes, use proper indices 
-- where every id belongs to the namespace of declkind

module LvmWrite( lvmWriteFile, lvmToBytes ) where

import Standard ( assert)
import Id       ( Id, stringFromId )
import IdMap    ( IdMap, emptyMap, insertMap, lookupMap, listFromMap )
import Byte
import Instr
import Lvm


{--------------------------------------------------------------
  Magic numbers
--------------------------------------------------------------}
lvmMajorVersion,lvmMinorVersion,magic :: Int
lvmMajorVersion  = 9
lvmMinorVersion  = 0
magic            = 0x4C564D58

{--------------------------------------------------------------
  Emit an LVM file
--------------------------------------------------------------}
lvmWriteFile :: FilePath -> LvmModule -> IO ()
lvmWriteFile path lvm
  = writeBytes path (lvmToBytes lvm)


lvmToBytes :: LvmModule -> Bytes
lvmToBytes mod
  = let (idxName,recs) = bytesFromModule mod
        headerlen = 32
        header    = block
                    [ recHeader
                    , headerlen
                    , totallen
                    , lvmMajorVersion
                    , lvmMinorVersion
                    , moduleMajorVer mod
                    , moduleMinorVer mod
                    , idxName         
                    , length recs
                    , bytesLength brecs
                    ]

        footerlen = 4
        footer    = block
                    [ recFooter, footerlen, totallen ]

        brecs     = cats recs 
        totallen  = bytesLength brecs + headerlen + 8 + footerlen + 8
        total     = cats [header,brecs,footer]
    in seq totallen total

bytesFromModule :: LvmModule -> (Index,[Bytes])
bytesFromModule mod
  = runEmit (emitLvmModule mod) 


emitLvmModule :: LvmModule -> Emit Index
emitLvmModule mod
  = do{ idxName <- emitName (moduleName mod)
      ; mapM_ emitDecl (moduleDecls mod)
      ; return idxName
      }

{--------------------------------------------------------------
  emit declarations
  TODO: emit  assumes some canonical order: We should do
  allocation of named blocks first and than emit to fix this.
--------------------------------------------------------------}
flags :: Access -> Int
flags access    = if (accessPublic access) then 1 else 0

isImported decl
  = case declAccess decl of
      Imported{}  -> True
      other       -> False

emitDecl DeclExtern{ externCall = CallInstr }  
  = return 0
emitDecl decl
  | isImported decl   = emitImport (declName decl) (declKindFromDecl decl) (declAccess decl)
emitDecl decl
  = case decl of
      DeclValue{}     -> emitDValue decl
      DeclAbstract{}  -> emitDAbstract decl
      DeclCon{}       -> emitDCon decl
      DeclExtern{}    -> emitDExtern decl
      DeclCustom{}    -> emitDCustom decl
      other           -> error "LvmWrite.emitDecl: invalid declaration at this phase"

emitDValue (DeclValue id access mbEnc instrs custom)
  = do{ idxEnc <- maybe (return 0) (\(Link id declkind) -> findIndex id) mbEnc
      ; idxCode <- emitInstrs instrs
      ; emitNamedBlock id DeclKindValue [flags access,arity,idxEnc,idxCode] custom
      }
  where
    arity = case instrs of
              (ARGCHK n:other) -> n
              otherwise        -> error ("LvmWrite.emitDecl: instructions do not start with an argument check: " ++ show id)

emitDCon (DeclCon id access arity tag custom)
  = emitNamedBlock id DeclKindCon [flags access,arity,tag] custom

emitDCustom (DeclCustom id access kind custom)
  = emitBlock (Just id) kind nil custom

emitDExtern (DeclExtern id access arity tp linkconv callconv libname externname custom)
  = do{ idxType            <- emitExternType tp
      ; idxLibName         <- emitNameString libname
      ; (nameFlag,idxName) <- emitNameExtern
      ; emitNamedBlock id DeclKindExtern  [flags access,arity,idxType
                                          ,idxLibName,idxName,nameFlag
                                          ,fromEnum linkconv, fromEnum callconv
                                          ] []
      }
  where
    emitNameExtern  = case externname of
                        Plain s    -> do{ idx <- emitNameString s; return (0,idx) }
                        Decorate s -> do{ idx <- emitNameString s; return (1,idx) }
                        Ordinal i  -> return (2,i)


emitDAbstract (DeclAbstract id access arity)
  = error ("LvmWrite.emitDAbstract: abstract values should be imported: " ++ show id)


emitImport id declkind access@(Imported public moduleName importName kind majorVer minorVer)
  = assert (declkind==kind) "LvmWrite.emitImport: kinds don't match" $
    do{ idxModule <- emitModule moduleName majorVer minorVer
      ; idxName   <- emitName importName
      ; emitNamedBlock id DeclKindImport [flags access,idxModule,idxName,fromEnum declkind] []
      }

emitModule name major minor
  = do{ idxName <- emitName name
      ; emitBlock Nothing DeclKindModule (block [idxName,major,minor]) []
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
emits instrs
  = concatMap emit instrs

emit :: Instr -> [Int]
emit instr
  = let opcode   = opcodeFromInstr instr
        illegal  = error ("LvmWrite.emit: illegal instruction at this phase: " ++ show (nameFromInstr instr))
        todo     = error ("LvmWrite.emit: todo: " ++ show (nameFromInstr instr))
    in case instr of
      -- pseudo instructions
      VAR id                  -> []
      PARAM id                -> []
      USE id                  -> []
      NOP                     -> illegal
      RESULT is               -> illegal

      -- structured instructions
      MATCHCON alts           -> [opcode] ++ emitMatch alts
      MATCHINT alts           -> [opcode] ++ emitMatch alts
      SWITCHCON alts          -> todo

      EVAL d is               -> let scrut = emits is
                                 in  emit (PUSHCONT (length scrut)) ++ scrut

      -- push instructions
      PUSHVAR     var         -> [opcode, offsetFromVar var]
      PUSHINT     n           -> [opcode, n]
      PUSHBYTES   bs c        -> [opcode, c]
      PUSHFLOAT   d           -> todo
      PUSHCODE    global      -> [opcode, indexFromGlobal global]
      PUSHCONT    ofs         -> [opcode, ofs]

      -- stack instructions
      ARGCHK      n           -> [opcode, n]
      SLIDE       n m depth   -> [opcode, n, m]
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
      PACK        arity       -> [opcode, arity]
      UNPACK      arity       -> [opcode, arity]

      -- optimized instructions
      PUSHVARS2   v w         -> [opcode, offsetFromVar v, offsetFromVar w]

      NEWCON0 con             -> [opcode, indexFromCon con]
      NEWCON1 con             -> [opcode, indexFromCon con]
      NEWCON2 con             -> [opcode, indexFromCon con]
      NEWCON3 con             -> [opcode, indexFromCon con]

      RETURNCON0 con          -> [opcode, indexFromCon con]

      -- single opcode instructions
      other                   -> [opcode]


emitMatch alts
  = assert (normalizedAlts alts) "LvmWrite.emitMatch: unnormalized alternatives" $
    let (pats,iss) = unzipAlts alts
        altis      = map emits iss
        start      = 2 + 2*(length alts -1)
    in [length alts-1]
       ++ matches start (zip pats (map length altis))
       ++ concat altis       
    where
      matches top []           = []
      matches top ((pat,n):xs)
        = (case pat of
             PatCon con -> [indexFromCon con]
             PatInt i   -> [i]
             PatDefault -> [])
          ++ [if (n==0) then 0 else top] ++ matches (top+n) xs


normalizedAlts alts
  = case alts of
      (Alt PatDefault is:rest) -> True
      other                    -> False

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
      MATCHCON alts   -> resolveAlts MATCHCON alts
      MATCHINT alts   -> resolveAlts MATCHINT alts
      SWITCHCON alts  -> resolveAlts SWITCHCON alts

      PUSHCODE global -> resolveGlobal PUSHCODE global
      ENTERCODE global-> resolveGlobal ENTERCODE global
      CALL global     -> resolveGlobal CALL global

      RETURNCON con   -> resolveCon RETURNCON con
      ALLOCCON con    -> resolveCon ALLOCCON con
      NEWCON con      -> resolveCon NEWCON con
      PACKCON con var -> resolveCon (\c -> PACKCON c var) con

      PUSHBYTES bs _  -> resolveBytes (PUSHBYTES bs) bs

      -- optimized instructions
      RETURNCON0 con  -> resolveCon RETURNCON0 con
      NEWCON0 con     -> resolveCon NEWCON0 con
      NEWCON1 con     -> resolveCon NEWCON1 con
      NEWCON2 con     -> resolveCon NEWCON2 con
      NEWCON3 con     -> resolveCon NEWCON3 con

      other           -> return instr

resolveAlts f alts
  = do{ alts' <- sequence (map resolveAlt alts); return (f alts') }

resolveAlt :: Alt -> Emit Alt
resolveAlt (Alt pat is)
  = do{ pat' <- resolvePat pat
      ; resolves (Alt pat') is
      }

resolvePat pat
  = case pat of
      PatCon con  -> resolveCon PatCon con
      other       -> return pat

resolveGlobal f (Global id _ arity)
  = do{ idx <- findIndex id
      ; return (f (Global id idx arity))
      }

resolveCon f (Con id _ arity tag)
  = do{ idx <- findIndex id
      ; return (f (Con id idx arity tag))
      }

resolveBytes f bs
  = do{ idx <- emitBytes bs
      ; return (f idx)
      }



{--------------------------------------------------------------
  basic entities
--------------------------------------------------------------}
emitNamedBlock :: Id -> DeclKind -> [Int] -> [Custom] -> Emit Index
emitNamedBlock id kind is custom
  = do{ idxName <- emitName id
      ; emitBlock (Just id) kind (block (idxName:is)) custom
      }

emitName :: Id -> Emit Index
emitName id
  = emitNameString (stringFromId id)

emitNameString :: String -> Emit Index
emitNameString s
  = emitBlock Nothing DeclKindName (blockString s) []

emitExternType tp
  = emitBlock Nothing DeclKindExternType (blockString tp) []

emitBytes bs
  = emitBlock Nothing DeclKindBytes (blockBytes bs) []

emitBlock mbId kind bs custom
  = do{ bcustom <- emitCustoms custom
      ; let bytes = cat bs bcustom
            total = cat (block [fromEnum kind,bytesLength bytes]) bytes
      ; assert ((bytesLength bytes `mod` 4) == 0) "LvmWrite.emitBlock: unaligned size" $
        emitPrimBlock mbId total
      }

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
      CustomInt i      -> return i
      CustomBytes bs   -> emitBytes bs
      CustomName id    -> emitName id
      CustomDecl (Link id kind) 
        -> findIndex id

{--------------------------------------------------------------
  Emit Monad
--------------------------------------------------------------}
newtype Emit a  = Emit (Env -> State -> (a,State))
data State      = State !Int Env [Bytes]
type Env        = IdMap Index

instance Functor Emit where
  fmap f (Emit e)   = Emit (\env st -> case e env st of
                                         (x,stx) -> (f x,stx))

instance Monad Emit where
  return x          = Emit (\env st -> (x,st))
  (Emit e) >>= f    = Emit (\env st -> case e env st of
                                         (x,stx) -> case f x of
                                                      Emit ef -> ef env stx)

runEmit :: Emit a -> (a,[Bytes])
runEmit (Emit e)
  = let (x,State _ env bbs) = e env (State 0 emptyMap [])  -- yes, a recursive, lazy, env :-)
    in (x,reverse bbs)

emitPrimBlock :: Maybe Id -> Bytes -> Emit Index
emitPrimBlock mid bs
  = Emit (\env (State count map bbs) ->
            let index = count+1
            in (index, State index (maybe map (\id -> insertMap id index map) mid) (bs:bbs)))

-- a nice lazy formulation, we can calculate all indices before writing the bytes.
findIndex :: Id -> Emit Index
findIndex id
  = Emit (\env st ->
          (case lookupMap id env of
             Nothing  -> (error ("LvmWrite.findIndex: undeclared identifier: " ++ show (stringFromId id)))
             Just idx -> (idx)
          , st))



{--------------------------------------------------------------
  block
--------------------------------------------------------------}
block :: [Int] -> Bytes
block is
  = cats (map bytesFromInt32 is)

blockString :: String -> Bytes
blockString s
  = blockBytes (bytesFromString s)

blockBytes bs
  = let len = bytesLength bs
    in cat (bytesFromInt32 len) (cat bs (padding len))

padding n
  = let m = (div (n + 3) 4) * 4
    in bytesFromList (replicate (m - n) (byteFromInt8 0))
