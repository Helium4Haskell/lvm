--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file 
-- is distributed under the terms of the BSD3 License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id: Data.hs 250 2012-08-22 10:59:40Z bastiaan $

module Lvm.Core.Type 
   ( Type(..), Kind(..), TypeConstant(..), Quantor(..), QuantorNames
   , ppQuantor, ppType, showType, arityFromType, typeUnit, typeBool
   , typeToStrict, typeConFromString, typeFunction
   , typeInstantiate, typeSubstitute, typeTupleElements
   , typeSubstitutions, typeExtractFunction, typeApply, typeApplyList
   ) where

import Lvm.Common.Id
import Lvm.Common.IdSet
import Text.PrettyPrint.Leijen

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
data Type = TAp !Type !Type
          | TForall !Quantor !Kind !Type
          | TStrict !Type
          | TVar !Int
          | TCon !TypeConstant
          | TAny

data Quantor
  = Quantor !Int !(Maybe String)

type QuantorNames = [(Int, String)]

data TypeConstant
  = TConDataType !Id
  | TConTuple !Int
  | TConTypeClassDictionary !Id
  | TConFun
  deriving Eq

data Kind = KFun !Kind !Kind
          | KStar

typeConFromString :: String -> TypeConstant
typeConFromString "->" = TConFun
typeConFromString ('(' : str)
  | rest == ")" = TConTuple (length commas + 1)
  where
    (commas, rest) = span (== ',') str
typeConFromString name = TConDataType $ idFromString name

typeToStrict :: Type -> Type
typeToStrict t@(TStrict _) = t
typeToStrict t = TStrict t

typeUnit :: Type
typeUnit = TCon $ TConTuple 0

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
      TForall _ _ t   -> arityFromType t
      TStrict t       -> arityFromType t
      TVar    _       -> 0
      TCon    _       -> 0
      TAny            -> 0

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

instance Show Type where
  show = show . pretty

showType :: QuantorNames -> Type -> String
showType quantors tp = show $ ppType 0 quantors tp

instance Show Kind where
  show = show . pretty

instance Pretty Type where
  pretty = ppType 0 []

instance Pretty Kind where
  pretty = ppKind 0

instance Show Quantor where
  show (Quantor _ (Just name)) = name
  show (Quantor i _) = "v$" ++ show i

instance Pretty TypeConstant where
  pretty (TConDataType name) = pretty name
  pretty (TConTypeClassDictionary name) = text "(@dictionary" <+> pretty name <+> text ")"
  pretty (TConTuple arity) = text ('(' : (replicate (arity - 1) ',') ++ ")")
  pretty TConFun = text "->"

ppQuantor :: QuantorNames -> Int -> Doc
ppQuantor names i = case lookup i names of
  Just name -> text name
  Nothing -> text $ "v$" ++ show i

ppType :: Int -> QuantorNames -> Type -> Doc
ppType level quantorNames tp
  = parenthesized $
    case tp of
      TAp (TCon a) t2 | a == TConDataType (idFromString "[]") -> text "[" <> ppType 0 quantorNames t2 <> text "]" 
      TAp (TAp (TCon TConFun) t1) t2 -> ppHi t1 <+> text "->" <+> ppEq t2
      TAp     t1 t2   -> ppEq t1 <+> ppHi t2
      TForall a k t   ->
        let quantorNames' = case a of
              Quantor idx (Just name) -> (idx, name) : quantorNames
              _ -> quantorNames
        in text "forall" <+> text (show a) {- <> text ":" <+> pretty k -} <> text "."
            <+> ppType 0 quantorNames' t
      TStrict t       -> ppHi t <> text "!"
      TVar    a       -> ppQuantor quantorNames a
      TCon    a       -> pretty a
      TAny            -> text "any"
  where
    tplevel = levelFromType tp
    parenthesized doc
      | level <= tplevel  = doc
      | otherwise         = parens doc
    ppHi t
      | level <= tplevel  = ppType (tplevel+1) quantorNames t
      | otherwise         = ppType 0 quantorNames t
    ppEq t
      | level <= tplevel  = ppType tplevel quantorNames t
      | otherwise         = ppType 0 quantorNames t

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

typeInstantiate :: Int -> Type -> Type -> Type
typeInstantiate var newType (TForall q@(Quantor idx _) k t)
  | var == idx = typeSubstitute var newType t
  | otherwise = TForall q k $ typeInstantiate var newType t
typeInstantiate _ _ t = t

-- TODO: Handle shadowing
typeSubstitute :: Int -> Type -> Type -> Type
typeSubstitute var newType = substitute
  where
    substitute (TAp t1 t2) = TAp (substitute t1) (substitute t2)
    substitute (TForall q@(Quantor idx _) k t)
      | idx == var = TForall q k t
      | otherwise = TForall q k $ substitute t
    substitute (TStrict t) = substitute t
    substitute (TVar name)
      | name == var = newType
      | otherwise = TVar name
    substitute t = t
  
typeSubstitutions :: [(Int, Type)] -> Type -> Type
typeSubstitutions [] t = t
typeSubstitutions substitutions (TAp t1 t2) = TAp (typeSubstitutions substitutions t1) (typeSubstitutions substitutions t2)
typeSubstitutions substitutions (TStrict t) = TStrict $ typeSubstitutions substitutions t
typeSubstitutions substitutions (TVar idx) = case lookup idx substitutions of
  Just tp -> tp
  Nothing -> TVar idx
typeSubstitutions substitutions (TForall q@(Quantor idx _) k t) = TForall q k $ typeSubstitutions (filter (\(i, _) -> idx /= i) substitutions) t
typeSubstitutions _ t = t

typeListElement :: Type -> Type
typeListElement (TAp (TCon (TConDataType dataType)) a)
  | dataType == idFromString "[]" = a
typeListElement TAny = TAny
typeListElement tp = error $ "typeListElement: expected a list type, got " ++ show tp ++ " instead"

typeTupleElements :: Type -> [Type]
typeTupleElements = elements 0
  where
    elements n (TCon (TConTuple m))
      | n < m = error $ "typeTupleElements: expected a saturated tuple type, got a partially applied tuple type (received " ++ show n ++ " arguments, expected " ++ show m ++ ")"
      | n > m = error $ "typeTupleElements: got an over applied tuple type"
      | otherwise = []
    elements n (TAp t1 t2) = t2 : elements (n + 1) t1
    elements _ TAny = repeat TAny
    elements _ (TVar _) = error $ "typeTupleElements: expected a tuple type, got a type variable instead"
    elements _ tp = error $ "typeTupleElements: expected a tuple type, got " ++ show tp ++ " instead"

typeExtractFunction :: Type -> ([Type], Type)
typeExtractFunction (TAp (TAp (TCon TConFun) t1) t2) = (t1 : args, ret)
  where
    (args, ret) = typeExtractFunction t2
typeExtractFunction tp = ([], tp)

typeApply :: Type -> Type -> Type
typeApply (TForall (Quantor x _) _ t1) t2 = typeSubstitute x t2 t1
typeApply t1 t2 = TAp t1 t2

typeApplyList :: Type -> [Type] -> Type
typeApplyList = foldl typeApply
