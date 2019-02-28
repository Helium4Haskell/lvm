--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file 
-- is distributed under the terms of the BSD3 License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id: Data.hs 250 2012-08-22 10:59:40Z bastiaan $

module Lvm.Core.Type 
   ( Type(..), Kind(..), TypeConstant(..)
   , addForall, arityFromType, typeBool, typeToStrict, typeFunction
   ) where

import Lvm.Common.Id
import Lvm.Common.IdSet
import Text.PrettyPrint.Leijen

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
data Type = TAp !Type !Type
          | TForall !Id !Type
          | TExist !Id !Type
          | TStrict !Type
          | TVar !Id
          | TCon !TypeConstant
          | TAny

data TypeConstant
  = TConDataType !Id
  | TConTuple !Int
  | TConTypeClassDictionary !Id
  | TConFun
  deriving Eq

data Kind = KFun !Kind !Kind
          | KStar

typeToStrict :: Type -> Type
typeToStrict t@(TStrict _) = t
typeToStrict t = TStrict t

typeBool :: Type
typeBool = TCon $ TConDataType $ idFromString "Bool"

typeFunction :: [Type] -> Type -> Type
typeFunction [] ret = ret
typeFunction (a:as) ret = TAp (TAp (TCon TConFun) a) $ typeFunction as ret

arityFromType :: Type -> Int
arityFromType tp
  = case tp of
      TAp (TAp (TCon TConFun) _) t2 -> arityFromType t2 + 1
      TAp     _ _     -> 0 -- assumes saturated constructors!
      TForall _ t     -> arityFromType t
      TExist  _ t     -> arityFromType t
      TStrict t       -> arityFromType t
      TVar    _       -> 0
      TCon    _       -> 0
      TAny            -> 0

addForall :: Type -> Type
addForall tp
  = foldr TForall tp (listFromSet (varsInType tp))

varsInType :: Type -> IdSet
varsInType tp
  = case tp of
      TForall a t     -> deleteSet a (varsInType t)
      TExist  a t     -> deleteSet a (varsInType t)
      TAp     t1 t2   -> unionSet (varsInType t1) (varsInType t2)
      TStrict t       -> varsInType t
      TVar    a       -> singleSet a
      TCon    _       -> emptySet
      TAny            -> emptySet

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

instance Show Type where
   show = show . pretty

instance Show Kind where
   show = show . pretty

instance Pretty Type where
   pretty = ppType 0

instance Pretty Kind where
   pretty = ppKind 0

instance Pretty TypeConstant where
  pretty (TConDataType name) = pretty name
  pretty (TConTypeClassDictionary name) = text "(@dictionary" <+> pretty name <+> text ")"
  pretty (TConTuple arity) = text ('(' : (replicate (arity - 1) ',') ++ ")")
  pretty TConFun = text "->"

ppType :: Int -> Type -> Doc
ppType level tp
  = parenthesized $
    case tp of
      TAp (TCon a) t2 | a == TConDataType (idFromString "[]") -> text "[" <> pretty t2 <> text "]" 
      TAp (TAp (TCon TConFun) t1) t2 -> ppHi t1 <+> text "->" <+> ppEq t2
      TAp     t1 t2   -> ppEq t1 <+> ppHi t2
      TForall a t     -> text "forall" <+> pretty a <> text "." <+> ppEq t
      TExist  a t     -> text "exist" <+> pretty a <> text "." <+> ppEq t
      TStrict t       -> ppHi t <> text "!"
      TVar    a       -> pretty a
      TCon    a       -> pretty a
      TAny            -> text "any"
  where
    tplevel           = levelFromType tp
    parenthesized doc | level <= tplevel  = doc
                      | otherwise         = parens doc
    ppHi t            | level <= tplevel  = ppType (tplevel+1) t
                      | otherwise         = ppType 0 t
    ppEq  t           | level <= tplevel  = ppType tplevel t
                      | otherwise         = ppType 0 t

ppKind :: Int -> Kind -> Doc
ppKind level kind
  = parenthesized $
    case kind of
      KFun k1 k2    -> ppHi k1 <+> text "->" <+> ppEq k2
      KStar         -> text "*"
  where
    (klevel,parenthesized)
      | level <= levelFromKind kind   = (levelFromKind kind,id)
      | otherwise                     = (0,parens)

    ppHi = ppKind (if klevel<=0 then 0 else klevel+1)
    ppEq = ppKind klevel

levelFromType :: Type -> Int
levelFromType tp
  = case tp of
      TForall{} -> 2
      TExist{}  -> 2
      TAp (TAp (TCon TConFun) _) _ -> 3
      TAp{}     -> 4
      TStrict{} -> 5
      TVar{}    -> 6
      TCon{}    -> 6
      TAny      -> 7 

levelFromKind :: Kind -> Int
levelFromKind kind
  = case kind of
      KFun{}    -> 1
      KStar{}   -> 2
