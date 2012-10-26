--------------------------------------------------------------------------------
-- Copyright 2001-2012, Bastiaan Heeren, Jurriaan Hage, Daan Leijen. This file 
-- is distributed under the terms of the GNU General Public License. For more 
-- information, see the file "LICENSE.txt", which is included in the 
-- distribution.
--------------------------------------------------------------------------------
--  $Id: Data.hs 250 2012-08-22 10:59:40Z bastiaan $

module Lvm.Core.Type 
   ( Type(..), Kind(..)
   , addForall, arityFromType
   ) where

import Lvm.Common.Id
import Lvm.Common.IdSet
import Text.PrettyPrint.Leijen

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
data Type = TFun Type Type
          | TAp Type Type
          | TForall Id Type
          | TExist Id Type
          | TStrict Type
          | TVar Id
          | TCon Id
          | TAny
          | TString String

data Kind       = KFun {kind1::Kind, kind2::Kind}
                | KStar
                | KString {kindString::String}

-- data SuperKind  = Box

arityFromType :: Type -> Int
arityFromType tp
  = case tp of
      TFun    _ t2    -> arityFromType t2 + 1
      TAp     _ _     -> 0 -- assumes saturated constructors!
      TForall _ t     -> arityFromType t
      TExist  _ t     -> arityFromType t
      TStrict t       -> arityFromType t
      TVar    _       -> 0
      TCon    _       -> 0
      TAny            -> 0
      TString _       -> error "Core.arityFromType: string type"

{-
arityFromKind :: Kind -> Int
arityFromKind kind
  = case kind of
      KFun k1 _ -> arityFromKind k1 + 1
      KStar     -> 0
      KString _ -> error "Core.arityFromKind: string kind" -}

addForall :: Type -> Type
addForall tp
  = foldr TForall tp (listFromSet (varsInType tp))

varsInType :: Type -> IdSet
varsInType tp
  = case tp of
      TForall a t     -> deleteSet a (varsInType t)
      TExist  a t     -> deleteSet a (varsInType t)
      TString _       -> emptySet
      TFun    t1 t2   -> unionSet (varsInType t1) (varsInType t2)
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

ppType :: Int -> Type -> Doc
ppType level tp
  = parenthesized $
    case tp of
      TAp (TCon a) t2 | a == idFromString "[]" -> text "[" <> pretty t2 <> text "]" 
      TFun    t1 t2   -> ppHi t1 <+> text "->" <+> ppEq t2
      TAp     t1 t2   -> ppEq t1 <+> ppHi t2
      TForall a t     -> text "forall" <+> pretty a <> text "." <+> ppEq t
      TExist  a t     -> text "exist" <+> pretty a <> text "." <+> ppEq t
      TStrict t       -> ppHi t <> text "!"
      TVar    a       -> pretty a
      TCon    a       -> pretty a
      TAny            -> text "any"
      TString s       -> string s
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
      KString s     -> string s
  where
    (klevel,parenthesized)
      | level <= levelFromKind kind   = (levelFromKind kind,id)
      | otherwise                     = (0,parens)

    ppHi = ppKind (if klevel<=0 then 0 else klevel+1)
    ppEq = ppKind klevel

levelFromType :: Type -> Int
levelFromType tp
  = case tp of
      TString{} -> 1
      TForall{} -> 2
      TExist{}  -> 2
      TFun{}    -> 3
      TAp{}     -> 4
      TStrict{} -> 5
      TVar{}    -> 6
      TCon{}    -> 6
      TAny      -> 7 

levelFromKind :: Kind -> Int
levelFromKind kind
  = case kind of
      KString{} -> 1
      KFun{}    -> 2
      KStar{}   -> 3