{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module Instr( Instr(..)
            , Var(..), Con(..), Global(..)
            , Alt(..), Pat(..)

            , Offset, Depth, Index, Tag, Arity

            , arityFromCon,    tagFromCon, indexFromCon
            , arityFromGlobal, indexFromGlobal
            , offsetFromVar,   idFromVar

            , opcodeFromInstr, instrFromOpcode
            , instrFromName, nameFromInstr, isCATCH
            , instrHasStrictResult
            ) where

import Char     ( toUpper )
import Standard ( strict )
import Id       ( Id, dummyId )
import Byte     ( Bytes, nil )

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
type Offset   = Int
type Depth    = Int
type Index    = Int
type Tag      = Int
type Arity    = Int

data Global   = Global !Id !Index !Arity
data Con      = Con    !Id !Index !Arity !Tag
data Var      = Var    !Id !Offset !Depth

arityFromCon    (Con id c arity tag) = arity
tagFromCon      (Con id c arity tag) = tag
indexFromCon    (Con id c arity tag) = c

arityFromGlobal (Global id c arity)  = arity
indexFromGlobal (Global id c arity)  = c

offsetFromVar   (Var id offset depth)  = offset
idFromVar       (Var id offset depth)  = id

----------------------------------------------------------------
-- The instructions
----------------------------------------------------------------
data Pat      = PatCon !Con
              | PatInt !Int
              | PatDefault

data Alt      = Alt !Pat ![Instr]

data Instr    =
              -- pseudo instructions
                VAR         !Id
              | PARAM       !Id
              | USE         !Id
              | NOP


              -- structured instructions
              | CATCH       ![Instr]               -- for generating PUSHCATCH
              | EVAL        ![Instr]               -- for generating PUSHCONT
              | RESULT      ![Instr]               -- for generating SLIDE

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
              | LTNAT
              | GTNAT
              | LENAT
              | GENAT

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


----------------------------------------------------------------
-- Instruction instances
----------------------------------------------------------------
instance Enum Instr where
  fromEnum  instr     = enumFromInstr instr
  toEnum i            = error "Code.toEnum: undefined for instructions"

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
        ]


{---------------------------------------------------------------
  instrHasStrictResult: returns [True] if an instruction returns
    a strict result, ie. a value that can be [match]ed or [RETURN]'ed.
---------------------------------------------------------------}
instrHasStrictResult instr
  = case instr of
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
      LTNAT   -> True
      GTNAT   -> True
      LENAT   -> True
      GENAT   -> True

      other   -> False


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
    walk opc []     = error ("Instr.opcodeFromInstr: no opcode defined for this instruction")
    walk opc (i:is) | instr == i  = opc
                    | otherwise   = strict walk (opc+1) is

instrTable :: [Instr]
instrTable =
    [ ARGCHK 0, PUSHCODE global, PUSHCONT 0, PUSHVAR var
    , PUSHINT 0, PUSHFLOAT 0, PUSHBYTES nil 0, SLIDE 0 0 0, STUB var
    , ALLOCAP 0, PACKAP var 0, PACKNAP var 0, NEWAP 0, NEWNAP 0
    , ENTER, PUSHCATCH, RAISE, CALL global
    , ALLOCCON con, PACKCON con var, NEWCON con
    --, UNPACKCON 0 , TESTCON id [], TESTINT 0 []
    , NOP, NOP, NOP
    , ADDINT, SUBINT, MULINT, DIVINT, MODINT, QUOTINT, REMINT
    , ANDINT, XORINT, ORINT, SHRINT, SHLINT, SHRNAT, NEGINT
    , EQINT, NEINT, LTINT, GTINT, LEINT, GEINT
    -- , ALLOC, NEW, GETFIELD, SETFIELD, GETTAG, PACK, UNPACK
    , NOP, NOP, NOP, NOP, NOP, NOP, NOP
    , PUSHVAR0, PUSHVAR1, PUSHVAR2, PUSHVAR3, PUSHVAR4
    -- , PUSHVARS2 id 0 id 0, PUSHVARS3 id 0 id 0 id 0, PUSHVARS4 id 0 id 0 id 0 id 0
    , PUSHVARS2 var var, NOP, NOP
    , NOP, NEWAP2, NEWAP3, NEWAP4
    , NOP, NEWNAP2, NEWNAP3, NEWNAP4
    , NEWCON0 con, NEWCON1 con, NEWCON2 con, NEWCON3 con
    , ENTERCODE global, EVALVAR var, RETURNCON con, RETURNINT 0
    , MATCHCON [], SWITCHCON [], MATCHINT []
    -- ,PUSHEAGER 0, RETURNCON0 id, INCINT 0
    , NOP, RETURNCON0 con, NOP
    , RETURN
    ]
  where
    id     = dummyId
    var    = Var id 0 0
    global = Global id 0 0
    con    = Con id 0 0 0


----------------------------------------------------------------
-- Instruction enumeration
----------------------------------------------------------------
isCATCH instr
  = enumFromInstr instr == enumFromInstr (CATCH [])

enumFromInstr instr
  = case instr of
      -- pseudo instructions
      VAR         id          -> -1
      PARAM       id          -> -2
      USE         id          -> -3
      NOP                     -> -4


    -- structured instructions
      CATCH       is          -> 0
      EVAL        is          -> 1
      RESULT      is          -> 2

      MATCHCON    alts        -> 3
      SWITCHCON   alts        -> 4
      MATCHINT    alts        -> 5

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
      ALLOCCON    con         -> 50
      PACKCON     con var     -> 51
      NEWCON      con         -> 52

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
      LTNAT                   -> 86
      GTNAT                   -> 87
      LENAT                   -> 88
      GENAT                   -> 89

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


    -- structured instructions
      CATCH       is          -> "CATCH"
      EVAL        is          -> "EVAL"
      RESULT      is          -> "RESULT"

      MATCHCON    alts        -> "MATCHCON"
      SWITCHCON   alts        -> "SWITCHCON"
      MATCHINT    alts        -> "MATCHINT"

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
      LTNAT                   -> "LTNAT"
      GTNAT                   -> "GTNAT"
      LENAT                   -> "LENAT"
      GENAT                   -> "GENAT"


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

      other                   -> "<unknown>"
