{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module LvmWrite( lvmWriteFile, lvmToBytes ) where

import Standard ( assert )
import Id       ( Id, stringFromId )
import IdMap    ( IdMap, emptyMap, insertMap, lookupMap, listFromMap )
import Byte
import Instr
import Lvm

{--------------------------------------------------------------
  Magic numbers
--------------------------------------------------------------}
lvmMajorVersion,lvmMinorVersion,magic :: Int
lvmMajorVersion  = 5
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
  = let (idxName,bdecls,binstrs)  = bytesFromModule mod
        header    = block
                    [ 36              -- header length
                    , lvmMajorVersion
                    , lvmMinorVersion
                    , versionMajor mod
                    , versionMinor mod
                    , idxName         -- index module name
                    , length bdecls
                    , sum (map bytesLength bdecls)
                    , length binstrs
                    , sum (map bytesLength binstrs)
                    ]

        total     = cats ([header] ++ bdecls ++ binstrs)
        totallen  = bytesLength total + 12 -- 2*sizeof(total length) + magic number
        xs        = cats [block [magic, totallen],total,block [totallen]]
    in seq totallen xs

bytesFromModule :: LvmModule -> (Index,[Bytes],[Bytes])
bytesFromModule mod
  = case runEmit (emitLvmModule mod) of
      ((idxName,binstrs),bdecls)  -> (idxName,bdecls,binstrs)


emitLvmModule :: LvmModule -> Emit (Index,[Bytes])
emitLvmModule mod
  = do{ idxName <- emitName (moduleName mod)
      ; mapM_ emitDCon     (listFromMap (constructors mod))
      ; mapM_ emitDExtern  (listFromMap (externs mod))
      ; mapM_ emitDImport  (listFromMap (imports mod))
      ; mapM_ emitDValue   (values mod)
      ; mapM_ emitDCustom  (listFromMap (customs mod))
      ; binstrs <- mapM emitInstrs (values mod)
      ; return (idxName, filter (not.isNil) binstrs)
      }

{--------------------------------------------------------------
  emit declarations
  TODO: emit  assumes some canonical order: We should do
  allocation of named blocks first and than emit to fix this.
--------------------------------------------------------------}
flags :: Access -> Int
flags Public    = 1
flags Private   = 0
flags other     = error "LvmWrite.flags: invalid access mode"

emitDValue (id,DValue access mbEnc instrs custom)
  | isImport access = emitImport id declValue access
  | otherwise       = do{ idxEnc <- maybe (return 0) (findIndex) mbEnc
                        ; emitNamedBlock id declValue [0,flags access,0,arity,idxEnc] custom
                        }
  where
    arity = case instrs of
              (ARGCHK n:other) -> n
              otherwise        -> error ("LvmWrite.emitDecl: instructions do not start with an argument check: " ++ show id)

emitDCon (id,DCon access arity tag custom)
  | isImport access = emitImport id declCon access
  | otherwise       = emitNamedBlock id declCon [0,flags access,0,arity,tag] custom

emitDCustom (id,DCustom access kind custom)
  | isImport access = emitImport id declCon access
  | otherwise       = emitBlock (Just id) kind nil custom

emitDExtern (id,DExtern access arity tp linkconv callconv libname externname custom)
  | callconv == CallInstr = return 0
  | isImport access = emitImport id declExtern access
  | otherwise       = do{ idxType            <- emitTypeString tp
                        ; idxLibName         <- emitNameString libname
                        ; (nameFlag,idxName) <- emitNameExtern
                        ; emitNamedBlock id declExtern [idxType,flags access,0,arity
                                                       ,idxLibName,idxName,nameFlag
                                                       ,fromEnum linkconv, fromEnum callconv
                                                       ] []
                        }
  where
    emitNameExtern  = case externname of
                        Plain s    -> do{ idx <- emitNameString s; return (0,idx) }
                        Decorate s -> do{ idx <- emitNameString s; return (1,idx) }
                        Ordinal i  -> return (2,i)


emitDImport (id,DImport access arity)
  | isImport access = emitImport id declValue access
  | otherwise       = error ("LvmWrite.emitImport: should be imported: " ++ show id)


emitImport id declKind (Import public moduleName importName majorVer minorVer)
  = do{ idxModule <- emitModule moduleName majorVer minorVer
      ; idxName   <- emitName importName
      ; emitNamedBlock id declImport [0,if public then 1 else 0,idxModule,idxName,declKind] []
      }

emitModule name major minor
  = do{ idxName <- emitName name
      ; emitBlock Nothing declModule (block [idxName,major,minor]) []
      }

{--------------------------------------------------------------
  emit instructions
--------------------------------------------------------------}
emitInstrs :: (Id,LvmValue) -> Emit Bytes
emitInstrs (id,DValue access mbEnc instrs custom)
  = do{ idx      <- findIndex id
      ; rinstrs  <- mapM resolve instrs
      ; let codes = concatMap emit rinstrs
      ; return (block ([idx,4 + length codes*4,0] ++ codes))
      }

emitInstrs other
  = return nil


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

      EVAL is                 -> let scrut = emits is
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
      EVAL is         -> resolves EVAL is
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
  = do{ idx <- emitBlock Nothing declBytes bs []
      ; return (f idx)
      }



{--------------------------------------------------------------
  basic entities
--------------------------------------------------------------}
emitNamedBlock :: Id -> Int -> [Int] -> [Custom] -> Emit Index
emitNamedBlock id kind is custom
  = do{ idxName <- emitName id
      ; emitBlock (Just id) kind (block (idxName:is)) custom
      }

emitName :: Id -> Emit Index
emitName id
  = emitNameString (stringFromId id)

emitNameString :: String -> Emit Index
emitNameString s
  = emitBlock Nothing declName (blockString s) []

emitTypeString tp
  = emitBlock Nothing declType (blockString tp) []

emitBlock mbId kind bs custom
  = do{ bcustom <- emitCustoms custom
      ; let bytes = cat bs bcustom
            total = cat (block [kind,bytesLength bytes]) bytes
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
      CtmInt i      -> return i
      CtmIndex id   -> findIndex id
      CtmBytes bs   -> emitBlock Nothing declBytes bs []
      CtmName id    -> emitName id

{--------------------------------------------------------------
  Emit Monad
--------------------------------------------------------------}
newtype Emit a  = Emit (State -> (a,State))
data State      = State !Int (IdMap Index) [Bytes]

instance Functor Emit where
  fmap f (Emit e)   = Emit (\st -> case e st of
                                     (x,stx) -> (f x,stx))

instance Monad Emit where
  return x          = Emit (\st -> (x,st))
  (Emit e) >>= f    = Emit (\st -> case e st of
                                     (x,stx) -> case f x of
                                                  Emit ef -> ef stx)

runEmit :: Emit a -> (a,[Bytes])
runEmit (Emit e)
  = case e (State 0 emptyMap []) of
      (x,State _ _ bbs) -> (x,reverse bbs)

emitPrimBlock :: Maybe Id -> Bytes -> Emit Index
emitPrimBlock mid bs
  = Emit (\(State count map bbs) ->
            let index = count+1
            in (index, State index (maybe map (\id -> insertMap id index map) mid) (bs:bbs)))

findIndex :: Id -> Emit Index
findIndex id
  = Emit (\st@(State count map bbs) ->
          case lookupMap id map of
            Nothing  -> error ("LvmWrite.findIndex: undeclared identifier: " ++ show (stringFromId id))
            Just idx -> (idx,st))

{--------------------------------------------------------------
  block
--------------------------------------------------------------}
block :: [Int] -> Bytes
block is
  = cats (map bytesFromInt32 is)

blockString :: String -> Bytes
blockString s
  = let bs  = bytesFromString s
        len = bytesLength bs
    in cat (bytesFromInt32 len) (cat bs (padding len))

padding n
  = let m = (div (n + 3) 4) * 4
    in bytesFromList (replicate (m - n) (byteFromInt8 0))
