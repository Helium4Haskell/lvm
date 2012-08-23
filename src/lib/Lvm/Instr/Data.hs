{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

module Lvm.Instr.Data ( Instr(..)
            , Var(..), Con(..), Global(..)
            , Alt(..), Pat(..)

            , Offset, Depth, Index, Tag, Arity

            , arityFromCon,    tagFromCon, indexFromCon
            , arityFromGlobal, indexFromGlobal
            , offsetFromVar,   idFromVar, depthFromVar

            , opcodeFromInstr, instrFromOpcode
            , instrFromName, nameFromInstr, isCATCH
            , instrHasStrictResult
            ) where

import Data.Char     ( toUpper )
import Lvm.Common.Standard ( strict )
import Lvm.Common.Id       ( Id, dummyId, stringFromId )
import Lvm.Common.Byte     ( Bytes, nil, stringFromBytes )
import Text.PrettyPrint.Leijen

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
type Offset   = Int
type Depth    = Int
type Index    = Int
type Tag      = Int
type Arity    = Int

data Global   = Global !Id Index !Arity
              deriving Show
data Con      = Con    !Id Index !Arity !Tag
              deriving Show
data Var      = Var    !Id !Offset !Depth
              deriving Show

arityFromCon    (Con _ _ arity _  ) = arity
tagFromCon      (Con _ _ _     tag) = tag
indexFromCon    (Con _ c _     _  ) = c

arityFromGlobal (Global _ _ arity)  = arity
indexFromGlobal (Global _ c _    )  = c

offsetFromVar   (Var _  offset _    )  = offset
depthFromVar    (Var _  _      depth)  = depth
idFromVar       (Var id _      _    )  = id

----------------------------------------------------------------
-- The instructions
----------------------------------------------------------------
data Pat      = PatCon !Con
              | PatInt !Int
              | PatTag !Tag !Arity
              | PatDefault
              deriving Show

data Alt      = Alt !Pat ![Instr]
              deriving Show

data Instr    =
              -- pseudo instructions
                VAR         !Id
              | PARAM       !Id
              | USE         !Id
              | NOP
              | ATOM        ![Instr]
              | INIT        ![Instr]


              -- structured instructions
              | CATCH       ![Instr]               -- for generating PUSHCATCH
              | EVAL        !Depth ![Instr]        -- for generating PUSHCONT
              | RESULT      ![Instr]               -- for generating SLIDE

              | MATCH       ![Alt]
              | MATCHCON    ![Alt]
              | SWITCHCON   ![Alt]
              | MATCHINT    ![Alt]

              -- push instructions
              | PUSHVAR     !Var
              | PUSHINT     !Int
              | PUSHBYTES   !Bytes !Index
              | PUSHFLOAT   !Double
              | PUSHCODE    !Global
              | PUSHCONT    !Offset
              | PUSHCATCH

              -- stack instructions
              | ARGCHK      !Arity
              | SLIDE       !Int !Int !Depth
              | STUB        !Var

              -- control
              | ENTER
              | RAISE
              | CALL        !Global

              | ENTERCODE   !Global
              | EVALVAR     !Var

              | RETURN
              | RETURNCON   !Con
              | RETURNINT   !Int

              -- applications
              | ALLOCAP     !Arity
              | PACKAP      !Var !Arity
              | PACKNAP     !Var !Arity
              | NEWAP       !Arity
              | NEWNAP      !Arity

              -- constructors
              | ALLOCCON    !Con
              | PACKCON     !Con !Var
              | NEWCON      !Con

              | ALLOC
              | NEW         !Arity
              | PACK        !Arity  !Var
              | UNPACK      !Arity
              | GETFIELD
              | SETFIELD
              | GETTAG
              | GETSIZE
              | UPDFIELD

              -- INT operations
              | ADDINT
              | SUBINT
              | MULINT
              | DIVINT
              | MODINT
              | QUOTINT
              | REMINT
              | ANDINT
              | XORINT
              | ORINT
              | SHRINT
              | SHLINT
              | SHRNAT
              | NEGINT

              | EQINT
              | NEINT
              | LTINT
              | GTINT
              | LEINT
              | GEINT
             
              -- FLOAT operations
              | ADDFLOAT
              | SUBFLOAT
              | MULFLOAT
              | DIVFLOAT
              | NEGFLOAT

              | EQFLOAT
              | NEFLOAT
              | LTFLOAT
              | GTFLOAT
              | LEFLOAT
              | GEFLOAT

              -- optimized VAR
              | PUSHVAR0
              | PUSHVAR1
              | PUSHVAR2
              | PUSHVAR3
              | PUSHVAR4

              | PUSHVARS2 !Var !Var

              -- optimized AP
              | NEWAP2
              | NEWAP3
              | NEWAP4

              | NEWNAP2
              | NEWNAP3
              | NEWNAP4

              -- optimized NEWCON
              | NEWCON0 !Con
              | NEWCON1 !Con
              | NEWCON2 !Con
              | NEWCON3 !Con

              | RETURNCON0 !Con
              deriving Show

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

instance Pretty Instr where
   pretty     = ppInstr
   prettyList = instrPretty

----------------------------------------------------------------
-- instrPretty
----------------------------------------------------------------
instrPretty :: [Instr] -> Doc
instrPretty instrs
  = ppInstrs instrs


----------------------------------------------------------------
-- Pretty
----------------------------------------------------------------
ppInstrs :: [Instr] -> Doc
ppInstrs instrs
  = align (vcat (map (ppInstr ) instrs))

ppInstr instr
  = let name = nameFromInstr instr
    in case instr of
      -- pseudo instructions
      VAR         id          -> text name <+> ppId id
      PARAM       id          -> text name <+> ppId id
      USE         id          -> text name <+> ppId id
      NOP                     -> text name
      
      ATOM        is          -> nest 2 (text name <$> ppInstrs is)
      INIT        is          -> nest 2 (text name <$> ppInstrs is)

    -- structured instructions
      CATCH instrs            -> nest 2 (text name <$> ppInstrs instrs)
      EVAL d instrs           -> nest 2 (text name <+> pretty d <$> ppInstrs instrs)
      RESULT instrs           -> nest 2 (text name <$> ppInstrs instrs)

      SWITCHCON alts          -> nest 2 (text name <$> ppAlts alts)
      MATCHCON alts           -> nest 2 (text name <$> ppAlts alts)
      MATCHINT alts           -> nest 2 (text name <$> ppAlts alts)
      MATCH alts              -> nest 2 (text name <$> ppAlts alts)


    -- push instructions
      PUSHVAR     var         -> text name <+> ppVar var
      PUSHINT     n           -> text name <+> pretty n
      PUSHBYTES   bs c        -> text name <+> ppBytes bs
      PUSHFLOAT   d           -> text name <+> pretty d
      PUSHCODE    global      -> text name <+> ppGlobal global
      PUSHCONT    ofs         -> text name <+> pretty ofs

    -- stack instructions
      ARGCHK      n           -> text name  <+> pretty n
      SLIDE       n m depth   -> text name  <+> pretty n <+> pretty m <+> pretty depth
      STUB        var         -> text name <+> ppVar var

    -- control
      ENTER                   -> text name
      RAISE                   -> text name
      CALL        global      -> text name <+> ppGlobal global

      ENTERCODE   global      -> text name <+> ppGlobal global
      EVALVAR     var         -> text name <+> ppVar var

      RETURN                  -> text name
      RETURNCON   con         -> text name <+> ppCon con
      RETURNINT   n           -> text name <+> pretty n

    -- applications
      ALLOCAP     arity       -> text name <+> pretty arity
      PACKAP      var arity   -> text name  <+> ppVar var <+> pretty arity
      PACKNAP     var arity   -> text name <+> ppVar var <+> pretty arity
      NEWAP       arity       -> text name   <+> pretty arity
      NEWNAP      arity       -> text name  <+> pretty arity

    -- constructors
      ALLOCCON    con         -> text name <+> ppCon con
      PACKCON     con var     -> text name  <+> ppVar var <+> ppCon con
      NEWCON      con         -> text name   <+> ppCon con
      
      NEW arity               -> text name <+> pretty arity
      PACK arity var          -> text name <+> pretty arity <+> ppVar var
      UNPACK arity            -> text name <+> pretty arity

    -- optimized instructions
      PUSHVARS2  v w          -> text name <+> ppVar v <+> ppVar w

      NEWCON0 con             -> text name <+> ppCon con
      NEWCON1 con             -> text name <+> ppCon con
      NEWCON2 con             -> text name <+> ppCon con
      NEWCON3 con             -> text name <+> ppCon con

      RETURNCON0 con          -> text name <+> ppCon con

    -- others
      other                   -> text name

ppAlts alts
  = vcat (map ppAlt alts)

ppAlt (Alt pat is)
  = nest 2 (ppPat pat <> text ":" <$> ppInstrs is)

ppPat pat
  = case pat of
      PatCon con  -> ppCon con
      PatInt i    -> pretty i
      PatTag t a  -> text "(@" <> pretty t <> char ',' <> pretty a <> text ")"
      PatDefault  -> text "<default>"

ppCon (Con id c arity tag)
  = ppId id <+> pretty arity

ppGlobal (Global id c arity)
  = ppId id <+> pretty arity

ppVar (Var id ofs depth)
  = ppId id <+> parens( pretty ofs <> comma <+> pretty depth )

ppId id
  = text (stringFromId id)

ppBytes bs
  = dquotes (string (stringFromBytes bs))

----------------------------------------------------------------
-- Instruction instances
----------------------------------------------------------------
instance Enum Instr where
  fromEnum  instr     = enumFromInstr instr
  toEnum _            = error "Code.toEnum: undefined for instructions"

instance Eq Instr where
  instr1 == instr2    = (fromEnum instr1 == fromEnum instr2)

instance Ord Instr where
  compare instr1 instr2  = compare (fromEnum instr1) (fromEnum instr2)


----------------------------------------------------------------
-- Instruction names
----------------------------------------------------------------
instrFromName :: String -> Instr
instrFromName name
  = case lookup (map toUpper name) instrNames of
      Nothing -> error ("Code.instrFromName: unknown instruction name: " ++ name)
      Just i  -> i
  where
    instrNames
      = [ ("CATCH", CATCH [])
        , ("RAISE", RAISE)

        , ("ADDINT", ADDINT)
        , ("SUBINT", SUBINT)
        , ("MULINT", MULINT)
        , ("DIVINT", DIVINT)
        , ("MODINT", MODINT)
        , ("QUOTINT",QUOTINT)
        , ("REMINT", REMINT)
        , ("ANDINT", ANDINT)
        , ("XORINT", XORINT)
        , ("ORINT",  ORINT)
        , ("SHRINT", SHRINT)
        , ("SHLINT", SHLINT)
        , ("SHRNAT", SHRNAT)
        , ("NEGINT", NEGINT)

        , ("EQINT", EQINT)
        , ("NEINT", NEINT)
        , ("LTINT", LTINT)
        , ("GTINT", GTINT)
        , ("LEINT", LEINT)
        , ("GEINT", GEINT)

        , ("ADDFLOAT", ADDFLOAT)
        , ("SUBFLOAT", SUBFLOAT)
        , ("MULFLOAT", MULFLOAT)
        , ("DIVFLOAT", DIVFLOAT)
        , ("NEGFLOAT", NEGFLOAT)

        , ("EQFLOAT", EQFLOAT)
        , ("NEFLOAT", NEFLOAT)
        , ("LTFLOAT", LTFLOAT)
        , ("GTFLOAT", GTFLOAT)
        , ("LEFLOAT", LEFLOAT)
        , ("GEFLOAT", GEFLOAT)

        , ("ALLOC",   ALLOC)
        , ("GETFIELD",GETFIELD)
        , ("SETFIELD",SETFIELD)
        , ("GETTAG",  GETTAG)
        , ("GETSIZE", GETSIZE)
        , ("UPDFIELD",UPDFIELD)
        ]


{---------------------------------------------------------------
  instrHasStrictResult: returns [True] if an instruction returns
    a strict result, ie. a value that can be [match]ed or [RETURN]'ed.
---------------------------------------------------------------}
instrHasStrictResult instr
  = case instr of
      NEW _   -> True
      ALLOC   -> True

      ADDINT  -> True
      SUBINT  -> True
      MULINT  -> True
      DIVINT  -> True
      MODINT  -> True
      QUOTINT -> True
      REMINT  -> True
      ANDINT  -> True
      XORINT  -> True
      ORINT   -> True
      SHRINT  -> True
      SHLINT  -> True
      SHRNAT  -> True
      NEGINT  -> True

      EQINT   -> True
      NEINT   -> True
      LTINT   -> True
      GTINT   -> True
      LEINT   -> True
      GEINT   -> True

      ADDFLOAT  -> True
      SUBFLOAT  -> True
      MULFLOAT  -> True
      DIVFLOAT  -> True
      NEGFLOAT  -> True

      EQFLOAT   -> True
      NEFLOAT   -> True
      LTFLOAT   -> True
      GTFLOAT   -> True
      LEFLOAT   -> True
      GEFLOAT   -> True

      _         -> False


----------------------------------------------------------------
-- Instruction opcodes
----------------------------------------------------------------
instrFromOpcode :: Int -> Instr
instrFromOpcode i
  | i >= length instrTable  = error ("Instr.instrFromOpcode: illegal opcode")
  | otherwise               = instrTable !! i

opcodeFromInstr :: Instr -> Int
opcodeFromInstr instr
  = walk 0 instrTable
  where
    walk _   []     = error ("Instr.opcodeFromInstr: no opcode defined for this instruction")
    walk opc (i:is) | instr == i  = opc
                    | otherwise   = strict walk (opc+1) is

instrTable :: [Instr]
instrTable =
    [ ARGCHK 0, PUSHCODE global, PUSHCONT 0, PUSHVAR var
    , PUSHINT 0, PUSHFLOAT 0, PUSHBYTES nil 0, SLIDE 0 0 0, STUB var
    , ALLOCAP 0, PACKAP var 0, PACKNAP var 0, NEWAP 0, NEWNAP 0
    , ENTER, RETURN, PUSHCATCH, RAISE, CALL global
    , ALLOCCON con, PACKCON con var, NEWCON con
    --, UNPACKCON 0 , TESTCON id [], TESTINT 0 []
    , NOP, NOP, NOP
    , ADDINT, SUBINT, MULINT, DIVINT, MODINT, QUOTINT, REMINT
    , ANDINT, XORINT, ORINT, SHRINT, SHLINT, SHRNAT, NEGINT
    , EQINT, NEINT, LTINT, GTINT, LEINT, GEINT
    , ALLOC, NEW 0, GETFIELD, SETFIELD, GETTAG, GETSIZE, PACK 0 var, UNPACK 0
    , PUSHVAR0, PUSHVAR1, PUSHVAR2, PUSHVAR3, PUSHVAR4
    -- , PUSHVARS2 id 0 id 0, PUSHVARS3 id 0 id 0 id 0, PUSHVARS4 id 0 id 0 id 0 id 0
    , PUSHVARS2 var var, NOP, NOP
    , NOP, NEWAP2, NEWAP3, NEWAP4
    , NOP, NEWNAP2, NEWNAP3, NEWNAP4
    , NEWCON0 con, NEWCON1 con, NEWCON2 con, NEWCON3 con
    , ENTERCODE global, EVALVAR var, RETURNCON con, RETURNINT 0, RETURNCON0 con 
    , MATCHCON [], SWITCHCON [], MATCHINT [], NOP {- MATCHFLOAT -}, MATCH []
    , NOP {- ENTERFLOAT -}, ADDFLOAT, SUBFLOAT, MULFLOAT, DIVFLOAT, NEGFLOAT
    , EQFLOAT, NEFLOAT, LTFLOAT, GTFLOAT, LEFLOAT, GEFLOAT
    -- Additional experimental instructions (AD, 20040108)
    , UPDFIELD    
    ]
  where
    id     = dummyId
    var    = Var id 0 0
    global = Global id 0 0
    con    = Con id 0 0 0


----------------------------------------------------------------
-- Instruction enumeration
----------------------------------------------------------------

isCATCH :: Instr -> Bool
isCATCH (CATCH _) = True
isCATCH _         = False

enumFromInstr instr
  = case instr of
      -- pseudo instructions
      VAR         id          -> -1
      PARAM       id          -> -2
      USE         id          -> -3
      NOP                     -> -4
      ATOM        instrs      -> -5
      INIT        instrs      -> -6


    -- structured instructions
      CATCH       is          -> 0
      EVAL        d is        -> 1
      RESULT      is          -> 2

      MATCHCON    alts        -> 3
      SWITCHCON   alts        -> 4
      MATCHINT    alts        -> 5
      MATCH       alts        -> 6

    -- push instructions
      PUSHVAR     var         -> 10
      PUSHINT     n           -> 11
      PUSHBYTES   bs c        -> 12
      PUSHFLOAT   d           -> 13
      PUSHCODE    global      -> 14
      PUSHCONT    ofs         -> 15
      PUSHCATCH               -> 16

    -- stack instructions
      ARGCHK      n           -> 20
      SLIDE       n m depth   -> 21
      STUB        var         -> 22

    -- control
      ENTER                   -> 30
      RAISE                   -> 31
      CALL        global      -> 32

      ENTERCODE   global      -> 33
      EVALVAR     var         -> 34

      RETURN                  -> 35
      RETURNCON   con         -> 36
      RETURNINT   n           -> 37

    -- applications
      ALLOCAP     arity       -> 40
      PACKAP      var arity   -> 41
      PACKNAP     var arity   -> 42
      NEWAP       arity       -> 43
      NEWNAP      arity       -> 44

    -- constructors
      ALLOCCON    con         -> 47
      PACKCON     con var     -> 48
      NEWCON      con         -> 49

      ALLOC                   -> 50
      NEW arity               -> 51
      PACK arity var          -> 52
      UNPACK arity            -> 53
      GETFIELD                -> 54 
      SETFIELD                -> 55
      GETTAG                  -> 56
      GETSIZE                 -> 57
      UPDFIELD                -> 58

    -- INT operations
      ADDINT                  -> 60
      SUBINT                  -> 61
      MULINT                  -> 62
      DIVINT                  -> 63
      MODINT                  -> 64
      QUOTINT                 -> 65
      REMINT                  -> 66
      ANDINT                  -> 67
      XORINT                  -> 68
      ORINT                   -> 69
      SHRINT                  -> 70
      SHLINT                  -> 71
      SHRNAT                  -> 72
      NEGINT                  -> 73

      -- relative INT ops
      EQINT                   -> 80
      NEINT                   -> 81
      LTINT                   -> 82
      GTINT                   -> 83
      LEINT                   -> 84
      GEINT                   -> 85

      -- optimized instructions
      PUSHVAR0                -> 100
      PUSHVAR1                -> 101
      PUSHVAR2                -> 102
      PUSHVAR3                -> 103
      PUSHVAR4                -> 104
      PUSHVARS2 v w           -> 105

      -- optimizes AP
      NEWAP2                  -> 111
      NEWAP3                  -> 112
      NEWAP4                  -> 113

      NEWNAP2                 -> 114
      NEWNAP3                 -> 115
      NEWNAP4                 -> 116

      -- optimized NEWCON
      NEWCON0 con             -> 120
      NEWCON1 con             -> 121
      NEWCON2 con             -> 122
      NEWCON3 con             -> 123

      RETURNCON0 con          -> 124

      -- FLOAT operations
      ADDFLOAT                  -> 160
      SUBFLOAT                  -> 161
      MULFLOAT                  -> 162
      DIVFLOAT                  -> 163
      NEGFLOAT                  -> 164

      -- relative FLOAT ops
      EQFLOAT                   -> 180
      NEFLOAT                   -> 181
      LTFLOAT                   -> 182
      GTFLOAT                   -> 183
      LEFLOAT                   -> 184
      GEFLOAT                   -> 185

      other                   -> error "Code.enumFromInstr: unknown instruction"


----------------------------------------------------------------
-- Instruction names
----------------------------------------------------------------
nameFromInstr instr
  = case instr of
      -- pseudo instructions
      VAR         id          -> "VAR"
      PARAM       id          -> "PARAM"
      USE         id          -> "USE"
      NOP                     -> "NOP"
      ATOM        is          -> "ATOM"
      INIT        is          -> "INIT"


    -- structured instructions
      CATCH       is          -> "CATCH"
      EVAL        d is        -> "EVAL"
      RESULT      is          -> "RESULT"

      MATCHCON    alts        -> "MATCHCON"
      SWITCHCON   alts        -> "SWITCHCON"
      MATCHINT    alts        -> "MATCHINT"
      MATCH       alts        -> "MATCH"

    -- push instructions
      PUSHVAR     var         -> "PUSHVAR"
      PUSHINT     n           -> "PUSHINT"
      PUSHBYTES   bs c        -> "PUSHBYTES"
      PUSHFLOAT   d           -> "PUSHFLOAT"
      PUSHCODE    global      -> "PUSHCODE"
      PUSHCONT    ofs         -> "PUSHCONT"
      PUSHCATCH               -> "PUSHCATCH"

    -- stack instructions
      ARGCHK      n           -> "ARGCHK"
      SLIDE       n m depth   -> "SLIDE"
      STUB        var         -> "STUB"

    -- control
      ENTER                   -> "ENTER"
      RAISE                   -> "RAISE"
      CALL        global      -> "CALL"

      ENTERCODE   global      -> "ENTERCODE"
      EVALVAR     var         -> "EVALVAR"

      RETURN                  -> "RETURN"
      RETURNCON   con         -> "RETURNCON"
      RETURNINT   n           -> "RETURNINT"

    -- applications
      ALLOCAP     arity       -> "ALLOCAP"
      PACKAP      var arity   -> "PACKAP"
      PACKNAP     var arity   -> "PACKNAP"
      NEWAP       arity       -> "NEWAP"
      NEWNAP      arity       -> "NEWNAP"

    -- constructors
      ALLOCCON    con         -> "ALLOCCON"
      PACKCON     con var     -> "PACKCON"
      NEWCON      con         -> "NEWCON"

      ALLOC                   -> "ALLOC"
      NEW arity               -> "NEW"
      PACK arity var          -> "PACK"
      UNPACK arity            -> "UNPACK"
      GETFIELD                -> "GETFIELD" 
      SETFIELD                -> "SETFIELD"
      GETTAG                  -> "GETTAG"
      GETSIZE                 -> "GETSIZE"
      UPDFIELD                -> "UPDFIELD"

    -- INT operations
      ADDINT                  -> "ADDINT"
      SUBINT                  -> "SUBINT"
      MULINT                  -> "MULINT"
      DIVINT                  -> "DIVINT"
      MODINT                  -> "MODINT"
      QUOTINT                 -> "QUOTINT"
      REMINT                  -> "REMINT"
      ANDINT                  -> "ANDINT"
      XORINT                  -> "XORINT"
      ORINT                   -> "ORINT"
      SHRINT                  -> "SHRINT"
      SHLINT                  -> "SHLINT"
      SHRNAT                  -> "SHRNAT"
      NEGINT                  -> "NEGINT"

      -- relative INT ops
      EQINT                   -> "EQINT"
      NEINT                   -> "NEINT"
      LTINT                   -> "LTINT"
      GTINT                   -> "GTINT"
      LEINT                   -> "LEINT"
      GEINT                   -> "GEINT"

      -- optimized instructions
      PUSHVAR0                -> "PUSHVAR0"
      PUSHVAR1                -> "PUSHVAR1"
      PUSHVAR2                -> "PUSHVAR2"
      PUSHVAR3                -> "PUSHVAR3"
      PUSHVAR4                -> "PUSHVAR4"

      PUSHVARS2 v w           -> "PUSHVARS2"


      -- optimizes AP
      NEWAP2                  -> "NEWAP2"
      NEWAP3                  -> "NEWAP3"
      NEWAP4                  -> "NEWAP4"

      NEWNAP2                 -> "NEWNAP2"
      NEWNAP3                 -> "NEWNAP3"
      NEWNAP4                 -> "NEWNAP4"

      -- optimized NEWCON
      NEWCON0 con             -> "NEWCON0"
      NEWCON1 con             -> "NEWCON1"
      NEWCON2 con             -> "NEWCON2"
      NEWCON3 con             -> "NEWCON3"

      RETURNCON0 con          -> "RETURNCON0"

    -- FLOAT operations
      ADDFLOAT                  -> "ADDFLOAT"
      SUBFLOAT                  -> "SUBFLOAT"
      MULFLOAT                  -> "MULFLOAT"
      DIVFLOAT                  -> "DIVFLOAT"
      NEGFLOAT                  -> "NEGFLOAT"

    -- relative FLOAT ops
      EQFLOAT                   -> "EQFLOAT"
      NEFLOAT                   -> "NEFLOAT"
      LTFLOAT                   -> "LTFLOAT"
      GTFLOAT                   -> "GTFLOAT"
      LEFLOAT                   -> "LEFLOAT"
      GEFLOAT                   -> "GEFLOAT"

      other                   -> "<unknown>"
