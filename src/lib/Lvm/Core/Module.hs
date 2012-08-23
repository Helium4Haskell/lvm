{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

module Lvm.Core.Module( Module(..)
             , Decl(..)
             , Custom(..)
             , DeclKind(..)  -- instance Eq, Enum
             , Arity, Tag 
             , Access(..), ExternName(..), CallConv(..), LinkConv(..)
             
             , globalNames, externNames, filterPublic
             , mapDecls, mapValues
             , declKindFromDecl, shallowKindFromDecl -- , hasDeclKind
             , isDeclValue, isDeclAbstract, isDeclCon, isDeclExtern, isDeclImport, isDeclGlobal
             , public, private
             ) where

import Lvm.Common.Standard( unsafeCoerce )
import Lvm.Common.Byte    ( Bytes, stringFromBytes )
import Lvm.Common.Id      ( Id, idFromString )
import Lvm.Common.IdSet   ( IdSet, setFromList )
import Lvm.Core.PrettyId
import Lvm.Instr.Data   ( Arity, Tag )
import Text.PrettyPrint.Leijen

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
  deriving (Eq,Show)

data Access
  = Defined  { accessPublic :: !Bool }
  | Imported { accessPublic :: !Bool, importModule :: Id, importName :: Id, importKind :: !DeclKind
             , importMajorVer :: !Int, importMinorVer :: !Int }
            
public, private :: Access
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


instance Ord DeclKind where
  compare k1 k2
    = case (k1,k2) of
        (DeclKindCustom id1,DeclKindCustom id2) -> compare id1 id2
        (DeclKindCustom _,_)                    -> GT
        (_,DeclKindCustom _)                    -> LT
        _                                       -> compare (fromEnum k1) (fromEnum k2)



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
        _                 -> error "Module.DeclKind.fromEnum: unknown kind"

declKindFromDecl :: Decl a -> DeclKind
declKindFromDecl decl
  = case decl of
      DeclValue{}    -> DeclKindValue
      DeclAbstract{} -> DeclKindValue
      DeclCon{}      -> DeclKindCon
      DeclExtern{}   -> DeclKindExtern
      DeclCustom{}   -> declKind decl
      DeclImport{}   -> importKind (declAccess decl)
      -- other          -> error "Module.kindFromDecl: unknown declaration"

shallowKindFromDecl :: Decl a -> DeclKind
shallowKindFromDecl decl
  = case decl of
      DeclValue{}    -> DeclKindValue
      DeclAbstract{} -> DeclKindValue
      DeclCon{}      -> DeclKindCon
      DeclExtern{}   -> DeclKindExtern
      DeclCustom{}   -> declKind decl
      DeclImport{}   -> DeclKindImport
      -- other          -> error "Module.shallowKindFromDecl: unknown declaration"

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

instance Pretty a => Pretty (Module a) where
   pretty (Module name _ _ decls) =
      text "module" <+> ppConId name <+> text "where"
      <$> vcat (map (\decl -> pretty decl <> semi <> line) decls)
      <$> empty

instance Pretty a => Pretty (Decl a) where
   pretty decl = nest 2 $ 
      case decl of
         DeclValue{}     -> ppVarId (declName decl) <+> ppAttrs decl 
                            <$> text "=" <+> pretty (valueValue decl)
         DeclCon{}       -> text "con" <+> ppConId (declName decl) <+> ppAttrs decl 
                            <$> text "=" <+> text "#(" <> pretty (conTag decl) <> 
                                             text ","  <> pretty (declArity decl) <> text ")"
         DeclCustom{}    -> text "custom" <+> pretty (declKind decl) <+> ppId (declName decl) <+> ppAttrs decl
         DeclExtern{}    -> text "extern" <+> ppVarId (declName decl) <+> ppAttrs decl
                            <$> text "=" <> pretty (externLink decl) <> pretty (externCall decl)
                            <+> ppExternName (externLib decl) (externName decl) <+> pretty (declArity decl)
                            <+> ppExternType (externCall decl) (externType decl)
         DeclAbstract{}  -> text "abstract" <+> ppVarId (declName decl) <+> ppNoImpAttrs decl
                            <$> text "=" <+> ppImported (declAccess decl) <+> pretty (declArity decl)
         DeclImport{}    -> text "import" <+> pretty (importKind (declAccess decl)) 
                            <+> ppId (declName decl) <+> ppNoImpAttrs decl
                            <$> text "=" <+> ppImported (declAccess decl)

instance Pretty LinkConv where
   pretty linkConv =
      case linkConv of
         LinkRuntime -> text " runtime"
         LinkDynamic -> text " dynamic"
         LinkStatic  -> empty

instance Pretty CallConv where
   pretty callConv =
      case callConv of
         CallInstr -> text " instruction"
         CallStd   -> text " stdcall"
         CallC     -> empty

ppExternName :: String -> ExternName -> Doc
ppExternName libName extName
  = case extName of
      Plain name    -> ppQual name
      Decorate name -> text "decorate" <+> ppQual name
      Ordinal i     -> ppQual (show i)
  where
    ppQual name   = (if (null libName) then empty
                                       else ppConId (idFromString libName) <> char '.' )
                    <> ppVarId (idFromString name)

ppExternType :: CallConv -> String -> Doc
ppExternType callConv tp
  = text "::" <+> case callConv of
                    CallInstr -> text tp
                    _         -> ppString tp

ppNoImpAttrs :: Decl a -> Doc
ppNoImpAttrs = ppAttrsEx True

ppAttrs :: Decl a -> Doc
ppAttrs = ppAttrsEx False

ppAttrsEx :: Bool -> Decl a -> Doc
ppAttrsEx hideImp decl
  = if (null (declCustoms decl) && not (accessPublic (declAccess decl)))
     then empty
     else text ":" <+> ppAccess (declAccess decl) 
          <+> (if (not hideImp) then ppImportAttr (declAccess decl) else empty) 
          <> pretty (declCustoms decl)

ppAccess :: Access -> Doc
ppAccess acc 
   | accessPublic acc = text "public" 
   | otherwise        = text "private"

ppImportAttr :: Access -> Doc
ppImportAttr  acc
  = case acc of
      Defined _ -> empty
      Imported _ modid impid impkind _ _
        -> text "import" <+> pretty impkind <+> ppConId modid <> char '.' <> ppId impid <> space
  
ppImported :: Access -> Doc
ppImported acc
  = case acc of
      Defined _ -> error "ModulePretty.ppImported: internal error: abstract or import value should always be imported!"
      Imported _ modid impid _ _ _
        -> ppConId modid <> char '.' <> ppId impid

instance Pretty Custom where
   pretty custom =
      case custom of
         CustomInt i         -> pretty i
         CustomName x        -> ppId x
         CustomBytes bs      -> dquotes (string (stringFromBytes bs))
         CustomLink x kind   -> text "custom" <+> pretty kind <+> ppId x
         CustomDecl kind cs  -> text "custom" <+> pretty kind <+> pretty cs
         CustomNothing       -> text "nothing"
      
   prettyList customs
      | null customs = empty
      | otherwise    = list (map pretty customs)

instance Pretty DeclKind where
   pretty kind = 
      case kind of
         DeclKindCustom x   -> ppId x
--         DeclKindName        
--         DeclKindKind
--         DeclKindBytes       
--         DeclKindCode
         DeclKindValue       -> ppId (idFromString "val")
         DeclKindCon         -> ppId (idFromString "con")
         DeclKindImport      -> ppId (idFromString "import")
         DeclKindModule      -> ppId (idFromString "module")
         DeclKindExtern      -> ppId (idFromString "extern")
--         DeclKindExternType      
         _                   -> pretty (fromEnum kind)

{---------------------------------------------------------------
  Utility functions
---------------------------------------------------------------}

isDeclValue :: Decl a -> Bool
isDeclValue (DeclValue{})       = True
isDeclValue _                   = False

isDeclAbstract :: Decl a -> Bool
isDeclAbstract (DeclAbstract{}) = True
isDeclAbstract _                = False

isDeclImport :: Decl a -> Bool
isDeclImport (DeclImport{})     = True
isDeclImport _                  = False

isDeclCon :: Decl a -> Bool
isDeclCon (DeclCon{})           = True
isDeclCon _                     = False

isDeclExtern :: Decl a -> Bool
isDeclExtern (DeclExtern{})     = True
isDeclExtern _                  = False

isDeclGlobal :: Decl a -> Bool
isDeclGlobal (DeclValue{})      = True
isDeclGlobal (DeclAbstract{})   = True
isDeclGlobal (DeclExtern{})     = True
isDeclGlobal _                  = False

-- hasDeclKind kind decl           = (kind==declKindFromDecl decl)

{---------------------------------------------------------------
  More Utility functions
---------------------------------------------------------------}
filterPublic :: Module v -> Module v
filterPublic m
  = m { moduleDecls = [d | d <- moduleDecls m, accessPublic (declAccess d)] }

globalNames :: Module v -> IdSet
globalNames m
  = setFromList [declName d | d <- moduleDecls m, isDeclValue d || isDeclAbstract d || isDeclExtern d]

externNames :: Module v -> IdSet
externNames m
  = setFromList [declName d | d <- moduleDecls m, isDeclExtern d]

mapDecls :: (Decl v -> Decl w) -> Module v -> Module w
mapDecls f m
  = m { moduleDecls = map f (moduleDecls m) }

mapValues :: (v -> w) -> Module v -> Module w
mapValues f
  = mapDecls (mapDeclValue f)

mapDeclValue :: (v->w) -> Decl v -> Decl w
mapDeclValue f decl 
  = case decl of
      DeclValue{} -> decl{ valueValue = f (valueValue decl) }
      _           -> unsafeCoerce decl