{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module Module( Module(..)
             , Arity, Tag, DeclKind, Access(..)
             , DValue(..), DAbstract(..), DCon(..), DExtern(..)
             , DCustom(..), DImport(..)
             , Customs, Custom(..)
             , ExternName(..), CallConv(..), LinkConv(..)
             , declValue,declCon,declImport,declExtern

             , isImport, globals, mapValues, mapDValues
             ) where

import Byte    ( Bytes )
import Id      ( Id )
import IdMap   ( IdMap )
import IdSet   ( IdSet, unionSets, setFromList, setFromMap )
import Instr   ( Arity, Tag )

{---------------------------------------------------------------
  A general LVM module structure parameterised by the
  type of values (Core expression, Asm expression or [Instr])
---------------------------------------------------------------}
data Module v   = Module{ moduleName   :: Id
                        , versionMajor :: !Int
                        , versionMinor :: !Int

                        , values       :: [(Id,DValue v)]
                        , abstracts    :: IdMap DAbstract
                        , constructors :: IdMap DCon
                        , externs      :: IdMap DExtern
                        , customs      :: IdMap DCustom
                        , imports      :: [(Id,DImport)]
                        }

type DeclKind   = Int
data Access     = Private
                | Public
                | Import { importPublic :: !Bool
                         , importModule :: Id, importName :: Id
                         , importVerMajor :: !Int, importVerMinor :: !Int }
                deriving Show


data DValue v   = DValue    { valueAccess  :: !Access, valueEnc :: !(Maybe Id), valueValue :: v, valueCustoms :: !Customs }
                deriving Show

data DAbstract  = DAbstract { abstractAccess :: !Access, abstractArity :: !Arity }
                deriving Show

data DCon       = DCon      { conAccess    :: !Access, conArity    :: !Arity, conTag :: !Tag, conCustoms :: !Customs }
                deriving Show

data DExtern    = DExtern   { externAccess :: !Access, externArity :: !Arity
                            , externType   :: !String
                            , externLink   :: !LinkConv, externCall :: !CallConv
                            , externLib    :: !String, externName :: !ExternName, externCustoms :: !Customs }
                deriving Show

data DCustom    = DCustom   { customAccess :: !Access, customKind :: !DeclKind, customCustoms :: !Customs }
                deriving Show

data DImport    = DImport   { importAccess :: !Access, importKind :: !DeclKind }
                deriving Show

type Customs    = [Custom]
data Custom     = CtmInt   !Int
                | CtmIndex Id
                | CtmBytes !Bytes
                | CtmName  Id
                deriving Show


-- externals
data ExternName = Plain    !String
                | Decorate !String
                | Ordinal  !Int
                deriving Show

data CallConv   = CallC | CallStd | CallInstr
                deriving (Show, Eq, Enum)

data LinkConv   = LinkStatic | LinkDynamic | LinkRuntime
                deriving (Show, Eq, Enum)

declValue,declCon,declImport,declExtern :: Int
declValue      = 3
declCon        = 4
declImport     = 5
declExtern     = 7

{---------------------------------------------------------------
  Utility functions
---------------------------------------------------------------}
isImport (Import {})  = True
isImport other        = False


globals :: Module v -> IdSet
globals mod
  = unionSets
  [ setFromList (map fst (values mod))
  , setFromMap (abstracts mod)
  , setFromMap (externs mod)
  ]



mapValues :: (v -> w) -> Module v -> Module w
mapValues f mod
  = mapDValues (\id (DValue acc enc v custom) -> DValue acc enc (f v) custom) mod

mapDValues f mod
  = mod{ values = map (\(id,v) -> (id,f id v)) (values mod) }
