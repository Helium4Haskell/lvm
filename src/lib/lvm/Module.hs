{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module Module( Module(..)
             , Decl(..)
             , Custom(..)
             , DeclKind(..)  -- instance Eq, Enum
             , Arity, Tag 
             , Access(..), ExternName(..), CallConv(..), LinkConv(..)
             
             , globalNames, externNames
             , mapValues
             , declKindFromDecl, shallowKindFromDecl -- , hasDeclKind
             , isDeclValue, isDeclAbstract, isDeclCon, isDeclExtern, isDeclImport, isDeclGlobal
             , public, private
             ) where

import Standard( unsafeCoerce )
import Byte    ( Bytes )
import Id      ( Id )
import IdSet   ( IdSet, setFromList )
import Instr   ( Arity, Tag )

{---------------------------------------------------------------
  A general LVM module structure parameterised by the
  type of values (Core expression, Asm expression or [Instr])
---------------------------------------------------------------}
data Module v   
  = Module{ moduleName     :: Id
          , moduleMajorVer :: !Int
          , moduleMinorVer :: !Int
          , moduleDecls    :: ![Decl v]
          }


data Decl v     
  = DeclValue     { declName :: Id, declAccess :: !Access, valueEnc :: Maybe Id, valueValue :: v, declCustoms :: ![Custom] }
  | DeclAbstract  { declName :: Id, declAccess :: !Access, declArity :: !Arity, declCustoms :: ![Custom] }
  | DeclCon       { declName :: Id, declAccess :: !Access, declArity :: !Arity, conTag :: !Tag, declCustoms :: [Custom] }
  | DeclExtern    { declName :: Id, declAccess :: !Access, declArity :: !Arity
                  , externType :: !String, externLink :: !LinkConv,   externCall  :: !CallConv
                  , externLib  :: !String, externName :: !ExternName, declCustoms :: ![Custom] } 
  | DeclCustom    { declName :: Id, declAccess :: !Access, declKind :: !DeclKind, declCustoms :: ![Custom] }

  | DeclImport    { declName :: Id, declAccess :: !Access, declCustoms :: ![Custom] }

data Custom
  = CustomInt   !Int
  | CustomBytes !Bytes
  | CustomName  Id
  | CustomLink  Id !DeclKind
  | CustomDecl  !DeclKind ![Custom]
  | CustomNothing

data DeclKind 
  = DeclKindName
  | DeclKindKind
  | DeclKindBytes
  | DeclKindCode
  | DeclKindValue
  | DeclKindCon
  | DeclKindImport
  | DeclKindModule
  | DeclKindExtern
  | DeclKindExternType
  | DeclKindCustom !Id
  deriving Eq

data Access
  = Defined  { accessPublic :: !Bool }
  | Imported { accessPublic :: !Bool, importModule :: Id, importName :: Id, importKind :: !DeclKind
             , importMajorVer :: !Int, importMinorVer :: !Int }
            
public  = Defined True
private = Defined False

-- externals
data ExternName = Plain    !String
                | Decorate !String
                | Ordinal  !Int
                deriving Show

data CallConv   = CallC | CallStd | CallInstr
                deriving (Show, Eq, Enum)

data LinkConv   = LinkStatic | LinkDynamic | LinkRuntime                
                deriving (Show, Eq, Enum)


instance Enum DeclKind where
  toEnum i  
    = case i of
        0 -> DeclKindName
        1 -> DeclKindKind
        2 -> DeclKindBytes
        3 -> DeclKindCode
        4 -> DeclKindValue
        5 -> DeclKindCon
        6 -> DeclKindImport
        7 -> DeclKindModule
        8 -> DeclKindExtern
        9 -> DeclKindExternType
        _ -> error ("Module.DeclKind.toEnum: unknown kind (" ++ show i ++ ")")

  fromEnum kind 
    = case kind of
        DeclKindName      -> 0
        DeclKindKind      -> 1
        DeclKindBytes     -> 2
        DeclKindCode      -> 3
        DeclKindValue     -> 4
        DeclKindCon       -> 5
        DeclKindImport    -> 6
        DeclKindModule    -> 7
        DeclKindExtern    -> 8
        DeclKindExternType-> 9
--      DeclKindCustom i  -> i
        other             -> error "Module.DeclKind.fromEnum: unknown kind"

declKindFromDecl decl
  = case decl of
      DeclValue{}    -> DeclKindValue
      DeclAbstract{} -> DeclKindValue
      DeclCon{}      -> DeclKindCon
      DeclExtern{}   -> DeclKindExtern
      DeclCustom{}   -> declKind decl
      DeclImport{}   -> importKind (declAccess decl)
      other          -> error "Module.kindFromDecl: unknown declaration"

shallowKindFromDecl decl
  = case decl of
      DeclValue{}    -> DeclKindValue
      DeclAbstract{} -> DeclKindValue
      DeclCon{}      -> DeclKindCon
      DeclExtern{}   -> DeclKindExtern
      DeclCustom{}   -> declKind decl
      DeclImport{}   -> DeclKindImport
      other          -> error "Module.shallowKindFromDecl: unknown declaration"



{---------------------------------------------------------------
  Utility functions
---------------------------------------------------------------}
isDeclValue (DeclValue{})       = True
isDeclValue other               = False

isDeclAbstract (DeclAbstract{}) = True
isDeclAbstract other            = False

isDeclImport (DeclImport{})     = True
isDeclImport other              = False

isDeclCon (DeclCon{})           = True
isDeclCon other                 = False

isDeclExtern (DeclExtern{})     = True
isDeclExtern other              = False

isDeclGlobal (DeclValue{})      = True
isDeclGlobal (DeclAbstract{})   = True
isDeclGlobal (DeclExtern{})     = True
isDeclGlobal other              = False

-- hasDeclKind kind decl           = (kind==declKindFromDecl decl)

{---------------------------------------------------------------
  More Utility functions
---------------------------------------------------------------}
globalNames :: Module v -> IdSet
globalNames mod
  = setFromList [declName d | d <- moduleDecls mod, isDeclValue d || isDeclAbstract d || isDeclExtern d]

externNames :: Module v -> IdSet
externNames mod
  = setFromList [declName d | d <- moduleDecls mod, isDeclExtern d]


mapValues :: (v -> w) -> Module v -> Module w
mapValues f mod
  = mod{ moduleDecls = map (mapDeclValue f) (moduleDecls mod)}

mapDeclValue :: (v->w) -> Decl v -> Decl w
mapDeclValue f decl 
  = case decl of
      DeclValue{} -> decl{ valueValue = f (valueValue decl) }
      decl        -> unsafeCoerce decl

  