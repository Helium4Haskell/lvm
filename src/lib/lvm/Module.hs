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
             , DValue(..), DImport(..), DCon(..), DExtern(..), DCustom(..)
             , Customs, Custom(..)
             , ExternName(..), CallConv(..), LinkConv(..)

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
data Module v   = Module{ moduleName   :: !Id
                        , versionMajor :: !Int
                        , versionMinor :: !Int

                        , values       :: [(Id,DValue v)]
                        , imports      :: IdMap DImport
                        , constructors :: IdMap DCon
                        , externs      :: IdMap DExtern
                        , customs      :: IdMap DCustom
                        }

type DeclKind   = Int
data Access     = Private
                | Public
                | Import { importPublic :: !Bool
                         , importModule :: !Id, importName :: !Id
                         , importVerMajor :: !Int, importVerMinor :: !Int }


data DValue v   = DValue  { valueAccess  :: !Access, valueEnc :: !(Maybe Id), valueValue :: v, valueCustoms :: !Customs }
data DImport    = DImport { importAccess :: !Access, importArity :: !Arity }
data DCon       = DCon    { conAccess    :: !Access, conArity    :: !Arity, conTag :: !Tag, conCustoms :: !Customs }
data DExtern    = DExtern { externAccess :: !Access, externArity :: !Arity
                          , externType   :: !String
                          , externLink   :: !LinkConv, externCall :: !CallConv
                          , externLib    :: !String, externName :: !ExternName, externCustoms :: !Customs }
data DCustom    = DCustom { customAccess :: !Access, customKind :: !DeclKind, customCustoms :: !Customs }

type Customs    = [Custom]
data Custom     = CtmInt   !Int
                | CtmIndex !Id
                | CtmBytes !Bytes
                | CtmName  !Id


-- externals
data ExternName = Plain    !String
                | Decorate !String
                | Ordinal  !Int

data CallConv   = CallC | CallStd | CallInstr
                deriving (Eq, Enum)

data LinkConv   = LinkStatic | LinkDynamic | LinkRuntime
                deriving (Eq, Enum)


{---------------------------------------------------------------
  Utility functions
---------------------------------------------------------------}
isImport (Import {})  = True
isImport other        = False


globals :: Module v -> IdSet
globals mod
  = unionSets
  [ setFromList (map fst (values mod))
  , setFromMap (imports mod)
  , setFromMap (externs mod)
  ]



mapValues :: (v -> w) -> Module v -> Module w
mapValues f mod
  = mapDValues (\id (DValue acc enc v custom) -> DValue acc enc (f v) custom) mod

mapDValues f mod
  = mod{ values = map (\(id,v) -> (id,f id v)) (values mod) }
