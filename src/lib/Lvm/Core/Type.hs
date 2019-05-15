--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file 
-- is distributed under the terms of the BSD3 License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id: Data.hs 250 2012-08-22 10:59:40Z bastiaan $

module Lvm.Core.Type 
   ( Type(..), Kind(..), TypeConstant(..), Quantor(..), QuantorNames, IntType(..)
   , ppQuantor, ppType, showType, arityFromType, typeUnit, typeBool
   , typeToStrict, typeNotStrict, typeIsStrict, typeSetStrict, typeConFromString, typeFunction
   , typeInstantiate, typeSubstitute, typeTupleElements, typeRemoveArgumentStrictness
   , typeSubstitutions, typeExtractFunction, typeApply, typeApplyList, dictionaryDataTypeName
   ) where

import Lvm.Common.Id
import Text.PrettyPrint.Leijen

import qualified Data.Set as S
import qualified Data.Map as M

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
data Type = TAp !Type !Type
          | TForall !Quantor !Kind !Type
          -- * The inner type should have kind *. The inner type
          -- may not be TStrict
          | TStrict !Type
          | TVar !Int
          | TCon !TypeConstant
          deriving (Eq, Ord)

data Quantor
  = Quantor !Int !(Maybe String)
  deriving (Eq, Ord)

type QuantorNames = [(Int, String)]

data TypeConstant
  = TConDataType !Id
  | TConTuple !Int
  | TConTypeClassDictionary !Id
  | TConFun
  deriving (Eq, Ord)

data IntType
  = IntTypeInt
  | IntTypeChar
  -- Currently only used by the backend, for fields of thunks.
  -- To expose this to the frontend, we will also need to implement casts to / from int16
  | IntTypeInt16
  deriving (Eq, Ord)

instance Show IntType where
  show IntTypeInt = "Int"
  show IntTypeChar = "Char"

data Kind = KFun !Kind !Kind
          | KStar
          deriving (Eq, Ord)

typeConFromString :: String -> TypeConstant
typeConFromString "->" = TConFun
typeConFromString ('(' : str)
  | rest == ")" = TConTuple (length commas + 1)
  where
    (commas, rest) = span (== ',') str
typeConFromString name = TConDataType $ idFromString name

typeToStrict :: Type -> Type
typeToStrict (TForall quantor kind t) = TForall quantor kind $ typeToStrict t
typeToStrict t@(TStrict _) = t
typeToStrict t = TStrict t

typeNotStrict :: Type -> Type
typeNotStrict (TForall quantor kind t) = TForall quantor kind $ typeNotStrict t
typeNotStrict (TStrict t) = typeNotStrict t
typeNotStrict t = t

typeIsStrict :: Type -> Bool
typeIsStrict (TForall _ _ t) = typeIsStrict t
typeIsStrict (TStrict _) = True
typeIsStrict _ = False

typeSetStrict :: Bool -> Type -> Type
typeSetStrict True = typeToStrict
typeSetStrict False = typeNotStrict

typeRemoveArgumentStrictness :: Type -> Type
typeRemoveArgumentStrictness (TForall quantor kind tp) = TForall quantor kind $ typeRemoveArgumentStrictness tp
typeRemoveArgumentStrictness (TAp (TAp (TCon TConFun) tArg) tReturn) =
  TAp (TAp (TCon TConFun) $ typeNotStrict tArg) $ typeRemoveArgumentStrictness tReturn
typeRemoveArgumentStrictness (TStrict tp) = TStrict $ typeRemoveArgumentStrictness tp
typeRemoveArgumentStrictness tp = tp 

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

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

showType :: QuantorNames -> Type -> String
showType quantors tp = show $ ppType 5 quantors tp

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
  pretty (TConTuple arity) = text ('(' : replicate (arity - 1) ',' ++ ")")
  pretty TConFun = text "->"

dictionaryDataTypeName :: Id -> Id
dictionaryDataTypeName = idFromString . ("Dict$" ++) . stringFromId

instance Show TypeConstant where
  show = show . pretty

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
      TStrict t       -> text "!" <> ppHi t
      TVar    a       -> ppQuantor quantorNames a
      TCon    a       -> pretty a
  where
    tplevel = levelFromType tp
    parenthesized doc
      | level <= tplevel  = doc
      | otherwise         = parens doc
    ppHi  = ppType (tplevel+1) quantorNames
    ppEq = ppType tplevel quantorNames

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

-- When performing beta reduction on a term of the form (forall x. t1) t2
-- we need to substitute x in t1 with t2. This may cause issues with
-- capturing or shadowing. Consider the following substitution
--    forall b. (forall a. forall b. a) b
--    = forall b. forall c. b
-- We need to rename type variable 'b' in the last 'forall' with a
-- fresh type variable. As a general rule, we will rename quantors
-- in t1 if the variable is free in t2. The fresh variable must be fresh in t2
-- and must not occur in t1.
typeSubstitute :: Int -> Type -> Type -> Type
typeSubstitute var rightType leftType = typeSubstitutions [(var, rightType)] leftType
  
typeSubstitutions :: [(Int, Type)] -> Type -> Type
typeSubstitutions [] t = t
typeSubstitutions initialSubstitutions leftType = fst $ substitute leftType (M.fromList initialSubstitutions) $ filter (\idx -> idx `S.notMember` leftUsed && idx `S.notMember` rightFree) [0..]
  where
    rightFree = foldr1 S.union $ map (typeFreeVars . snd) initialSubstitutions
    leftUsed = typeUsedVars leftType

    substitute :: Type -> M.Map Int Type -> [Int] -> (Type, [Int])
    substitute (TAp t1 t2) mapping fresh = (TAp t1' t2', fresh'')
      where
        (t1', fresh') = substitute t1 mapping fresh
        (t2', fresh'') = substitute t2 mapping fresh'
    substitute (TForall (Quantor idx _) k t) mapping fresh
      | idx `S.member` rightFree =
        -- Conflicting type variable. Give the variable a new index
        let
          idx' : fresh' = fresh
          mapping' = M.insert idx (TVar idx') mapping
          (t', fresh'') = substitute t mapping' fresh'
        in
          (TForall (Quantor idx' Nothing) k t', fresh'')
      | otherwise =
        let
          mapping' = M.delete idx mapping
          (t', fresh') = substitute t mapping' fresh
        in
          (TForall (Quantor idx Nothing) k t', fresh')
    substitute (TStrict t) mapping fresh = (TStrict t', fresh')
      where
        (t', fresh') = substitute t mapping fresh
    substitute t@(TVar idx) mapping fresh = case M.lookup idx mapping of
      Just tp -> (tp, fresh)
      Nothing -> (t, fresh)
    substitute t _ fresh = (t, fresh)

typeListElement :: Type -> Type
typeListElement (TAp (TCon (TConDataType dataType)) a)
  | dataType == idFromString "[]" = a
typeListElement tp = error $ "typeListElement: expected a list type, got " ++ showType [] tp ++ " instead"

typeTupleElements :: Type -> [Type]
typeTupleElements tupleType = elements 0 tupleType []
  where
    elements n (TCon (TConTuple m)) accum
      | n < m = error $ "typeTupleElements: expected a saturated tuple type, got a partially applied tuple type (received " ++ show n ++ " arguments, expected " ++ show m ++ ")"
      | n > m = error $ "typeTupleElements: got an over applied tuple type"
      | otherwise = accum
    elements n (TAp t1 t2) accum = elements (n + 1) t1 (t2 : accum)
    elements _ (TVar _) _ = error $ "typeTupleElements: expected a tuple type, got a type variable instead"
    elements _ tp _ = error $ "typeTupleElements: expected a tuple type, got " ++ showType [] tp ++ " instead"

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

typeUsedVars :: Type -> S.Set Int
typeUsedVars (TAp t1 t2) = typeUsedVars t1 `S.union` typeUsedVars t2
typeUsedVars (TForall (Quantor idx _) _ tp) = S.insert idx $ typeUsedVars tp
typeUsedVars (TStrict tp) = typeUsedVars tp
typeUsedVars (TVar idx) = S.singleton idx
typeUsedVars (TCon _) = S.empty

typeFreeVars :: Type -> S.Set Int
typeFreeVars (TAp t1 t2) = typeFreeVars t1 `S.union` typeFreeVars t2
typeFreeVars (TForall (Quantor idx _) _ tp) = S.delete idx $ typeFreeVars tp
typeFreeVars (TStrict tp) = typeFreeVars tp
typeFreeVars (TVar idx) = S.singleton idx
typeFreeVars (TCon _) = S.empty
