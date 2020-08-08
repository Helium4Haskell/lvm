--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file
-- is distributed under the terms of the BSD3 License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id: Data.hs 250 2012-08-22 10:59:40Z bastiaan $

module Lvm.Core.Type where

import qualified Data.Map as M
import qualified Data.Set as S
import Lvm.Common.Id
import Text.PrettyPrint.Leijen

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
data Type
  = TAp !Type !Type
  | TForall !Quantor !Type
  | TVar !Int
  | TCon !TypeConstant
  -- Constraint on type level, of form p ⊑ q
  | TQTy Type [(UAnn, UAnn)]
  | TAnn !SAnn !UAnn
  deriving (Eq, Ord, Show)

type UVar = Int

data SAnn = SStrict | SNone deriving (Eq, Ord, Show)
data UAnn = UUnique | UShared | UVar !UVar | UNone deriving (Eq, Ord)

instance Show UAnn where
  show UUnique = "1"
  show UShared = "w"
  show (UVar u) = "u" ++ show u
  show UNone = "∅"

data Quantor
  = Quantor !Int !Kind !(Maybe String)
  deriving (Eq, Ord)

type QuantorNames = [(Int, String)]

data TypeConstant
  = TConDataType !Id
  | TConTuple !Int
  | TConTypeClassDictionary !Id
  | TConFun
  deriving (Show, Eq, Ord)

data IntType
  = IntTypeInt
  | IntTypeChar
  | -- Currently only used by the backend, for fields of thunks.
    -- To expose this to the frontend, we will also need to implement casts to / from int16
    IntTypeInt16
  deriving (Eq, Ord)

instance Show IntType where
  show IntTypeInt = "Int"
  show IntTypeChar = "Char"

data Kind
  = KFun !Kind !Kind
  | KStar
  | KAnn
  deriving (Eq, Ord)

typeConFromString :: String -> TypeConstant
typeConFromString "->" = TConFun
typeConFromString ('(' : str)
  | rest == ")" = TConTuple (length commas + 1)
  where
    (commas, rest) = span (== ',') str
typeConFromString name = TConDataType $ idFromString name

-- add strictness annotation to a variable or forall variable
addSAnnToType :: SAnn -> Type -> Type
addSAnnToType a (TForall quantor t) = TForall quantor $ addSAnnToType a t
addSAnnToType a (TAp (TAnn _ a2) t) = TAp (TAnn a a2) t
addSAnnToType a t = TAp (TAnn a UNone) t

-- add uniqueness annotation to a variable or forall variable
addUAnnToType :: UAnn -> Type -> Type
addUAnnToType a (TForall quantor t) = TForall quantor $ addUAnnToType a t
addUAnnToType a (TAp (TAnn a1 _) t) = TAp (TAnn a1 a) t
addUAnnToType a t = TAp (TAnn SNone a) t

-- Remove strictness annotation from a variable or forall variable
removeSAnnFromType :: Type -> Type
removeSAnnFromType (TForall quantor t) = TForall quantor $ removeSAnnFromType t
removeSAnnFromType (TAp (TAnn _ UNone) t) = t
removeSAnnFromType (TAp (TAnn _ a2) t) = TAp (TAnn SNone a2) t
removeSAnnFromType t = t

-- Remove uniqueness annotation from a variable or forall variable
removeUAnnFromType :: Type -> Type
removeUAnnFromType (TForall quantor t) = TForall quantor $ removeUAnnFromType t
removeUAnnFromType (TAp (TAnn SNone _) t) = t
removeUAnnFromType (TAp (TAnn a1 _) t) = TAp (TAnn a1 UNone) t
removeUAnnFromType t = t

-- remove annotation from a variable (strictness and uniqueness)
removeAnnotations :: Type -> Type
removeAnnotations (TForall quantor t) = TForall quantor $ removeAnnotations t
removeAnnotations (TAp (TAnn _ _) t) = t
removeAnnotations t = t

typeSetSAnn :: Bool -> SAnn -> Type -> Type
typeSetSAnn True a = addSAnnToType a
typeSetSAnn False _ = removeSAnnFromType

-- Remove all annotations
typeRemoveAnn :: Type -> Type
typeRemoveAnn = typeRemoveAnn' True

typeRemoveUAnn :: Type -> Type
typeRemoveUAnn = typeRemoveAnn' False

typeRemoveAnn' :: Bool -> Type -> Type
typeRemoveAnn' b (TForall (Quantor _ KAnn _) tp) = typeRemoveAnn' b tp
typeRemoveAnn' b (TForall quantor tp) = TForall quantor $ typeRemoveAnn' b tp
typeRemoveAnn' b@True (TAp (TAnn a1 a2) tp) = typeRemoveAnn' b tp
typeRemoveAnn' b@False (TAp (TAnn a1 a2) tp) = case a1 of
                                                 SNone -> typeRemoveAnn' b tp
                                                 _ -> TAp (TAnn a1 UNone) (typeRemoveAnn' b tp)
typeRemoveAnn' b (TQTy tp _) = typeRemoveAnn' b tp
typeRemoveAnn' b (TAp tp1 tp2) = TAp (typeRemoveAnn' b tp1) (typeRemoveAnn' b tp2)
typeRemoveAnn' _ tp = tp

typeRemoveArgumentAnn :: (Type -> Type) -> Type -> Type
typeRemoveArgumentAnn f (TForall quantor tp) = TForall quantor $ typeRemoveArgumentAnn f tp
typeRemoveArgumentAnn f (TAp (TAnn a1 a2) tp) = TAp (TAnn a1 a2) $ typeRemoveArgumentAnn f tp
typeRemoveArgumentAnn f (TAp (TAp (TCon TConFun) tArg) tReturn) =
  TAp (TAp (TCon TConFun) $ f tArg) $ typeRemoveArgumentAnn f tReturn
typeRemoveArgumentAnn _ tp = tp

typeToStrict :: Type -> Type
typeToStrict = addSAnnToType SStrict

typeNotStrict :: Type -> Type
typeNotStrict = removeSAnnFromType

typeIsStrict :: Type -> Bool
typeIsStrict (TForall _ t) = typeIsStrict t
typeIsStrict (TAp (TAnn a1 _) _)  = a1 == SStrict
typeIsStrict (TAnn a1 _)  = a1 == SStrict
typeIsStrict _ = False

typeSetStrict :: Bool -> Type -> Type
typeSetStrict b = typeSetSAnn b SStrict

typeRemoveArgumentStrictness :: Type -> Type
typeRemoveArgumentStrictness = typeRemoveArgumentAnn removeSAnnFromType

typeUnit :: Type
typeUnit = TCon $ TConTuple 0

typeBool :: Type
typeBool = TCon $ TConDataType $ idFromString "Bool"

typeFunction :: [Type] -> Type -> Type
typeFunction [] ret = ret
typeFunction (a : as) ret = TAp (TAp (TCon TConFun) a) $ typeFunction as ret

arityFromType :: Type -> Int
arityFromType tp =
  case tp of
    TAp (TAp (TCon TConFun) _) t2 -> arityFromType t2 + 1
    TAp (TAnn _ _) t -> arityFromType t
    TAp _ _ -> 0 -- assumes saturated constructors!
    TForall _ t -> arityFromType t
    TVar _ -> 0
    TCon _ -> 0

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

-- print type with empty quantor list
showType :: Type -> String
showType = showTypeWithQuantors []

-- Should be unnecessary if TVar contains the quantor name
showTypeWithQuantors :: QuantorNames -> Type -> String
showTypeWithQuantors quantors tp = show $ ppType 5 quantors tp

instance Show Kind where
  show = show . pretty

instance Pretty Type where
  pretty = ppType 0 []

instance Pretty Kind where
  pretty = ppKind 0

instance Show Quantor where
  show (Quantor i KAnn _) = "u$" ++ show i
  show (Quantor i _ _) = "v$" ++ show i

instance Pretty TypeConstant where
  pretty (TConDataType name) = pretty name
  pretty (TConTypeClassDictionary name) = text "(@dictionary" <+> pretty name <> text ")"
  pretty (TConTuple arity) = text ('(' : replicate (arity - 1) ',' ++ ")")
  pretty TConFun = text "->"

showTypeConstant :: TypeConstant -> String
showTypeConstant = show . pretty

dictionaryDataTypeName :: Id -> Id
dictionaryDataTypeName = idFromString . ("Dict$" ++) . stringFromId

ppQuantor :: QuantorNames -> Int -> Doc
ppQuantor _ i = text $ "v$" ++ show i

ppType :: Int -> QuantorNames -> Type -> Doc
ppType level quantorNames (TQTy t cs) = ppType (level + 1) quantorNames t <> text "," <+> list (map ppTQTy cs)
ppType level quantorNames tp =
    parenthesized $ case tp of
      TAp (TCon a) t2 | a == TConDataType (idFromString "[]") -> text "[" <> ppType 0 quantorNames t2 <> text "]"
      TAp (TAp (TAp (TAnn _ a2) (TCon TConFun)) t1) t2 -> ppHi t1 <+> text "->" <> text ":" <> ppUAnn a2 <+> ppEq t2
      TAp (TAp (TCon TConFun) t1) t2 -> ppHi t1 <+> text "->" <+> ppEq t2
      TAp (TAnn a1 a2) t -> ppSAnn a1 <> ppTUAnn True a2 <> ppEq t
      TAnn a1 a2 -> ppSAnn a1 <> ppTUAnn False a2
      TAp t1 t2 -> ppEq t1 <+> ppHi t2
      TForall a t ->
        let quantorNames' = case a of
              Quantor idx k (Just name) -> (idx, name) : quantorNames
              _ -> quantorNames
         in text "forall" <+> text (show a {- <> text ":" <+> pretty k -}) <> text "."
              <+> ppType 0 quantorNames' t
      TVar a -> ppQuantor quantorNames a
      TCon a -> pretty a
      _ -> error (show tp)
  where
    ppTUAnn isType u
      | u == UNone = text ""
      | otherwise = ppUAnn u <> if isType then text ":" else text ""
    tplevel = levelFromType tp
    parenthesized doc
      | level <= tplevel = doc
      | otherwise = parens doc
    ppHi = ppType (tplevel + 1) quantorNames
    ppEq = ppType tplevel quantorNames

ppTQTy :: (UAnn, UAnn) -> Doc
ppTQTy (u1, u2) = ppUAnn u1 <+> text "<=" <+> ppUAnn u2

ppSAnn :: SAnn -> Doc
ppSAnn SStrict = text "!"
ppSAnn SNone = text ""

ppUAnn :: UAnn -> Doc
ppUAnn UUnique = text "1"
ppUAnn UShared = text "w"
ppUAnn UNone = text ""
ppUAnn (UVar a) = text "u$" <> text (show a)

ppKind :: Int -> Kind -> Doc
ppKind level kind =
  parenthesized $
    case kind of
      KFun k1 k2 -> ppHi k1 <+> text "->" <+> ppEq k2
      KStar -> text "*"
      KAnn -> text "&"
  where
    (klevel, parenthesized)
      | level <= levelFromKind kind = (levelFromKind kind, id)
      | otherwise = (0, parens)
    ppHi = ppKind (if klevel <= 0 then 0 else klevel + 1)
    ppEq = ppKind klevel

levelFromType :: Type -> Int
levelFromType tp =
  case tp of
    TForall {} -> 2
    TAp (TAp (TCon TConFun) _) _ -> 3
    TAp (TAnn _ _) _ -> 5
    TAnn _ _ -> 5
    TAp {} -> 4
    TVar {} -> 6
    TCon {} -> 6

levelFromKind :: Kind -> Int
levelFromKind kind =
  case kind of
    KFun {} -> 1
    KStar {} -> 2
    KAnn {} -> 2

typeInstantiate :: Int -> Type -> Type -> Type
typeInstantiate var newType (TForall q@(Quantor idx _ _) t)
  | var == idx = typeSubstitute var newType t
  | otherwise = TForall q $ typeInstantiate var newType t
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
typeSubstitutions initialSubstitutions leftType = fst $ substitute leftType (M.fromList initialSubstitutions) $ filter (\idx -> idx `S.notMember` leftUsed && idx `S.notMember` rightFree) [0 ..]
  where
    rightFree = foldr1 S.union $ map (typeFreeVars . snd) initialSubstitutions
    leftUsed = typeUsedVars leftType
    substitute :: Type -> M.Map Int Type -> [Int] -> (Type, [Int])
    substitute (TAp (TAnn a1 a2) t) mapping fresh = (TAp (TAnn a1 a2) t', fresh')
      where
        (t', fresh') = substitute t mapping fresh
    substitute (TAp t1 t2) mapping fresh = (TAp t1' t2', fresh'')
      where
        (t1', fresh') = substitute t1 mapping fresh
        (t2', fresh'') = substitute t2 mapping fresh'
    substitute (TForall (Quantor idx k _) t) mapping fresh
      | idx `S.member` rightFree =
        -- Conflicting type variable. Give the variable a new index
        let idx' : fresh' = fresh
            mapping' = M.insert idx (TVar idx') mapping
            (t', fresh'') = substitute t mapping' fresh'
         in (TForall (Quantor idx' k Nothing) t', fresh'')
      | otherwise =
        let mapping' = M.delete idx mapping
            (t', fresh') = substitute t mapping' fresh
         in (TForall (Quantor idx k Nothing) t', fresh')
    substitute t@(TVar idx) mapping fresh = case M.lookup idx mapping of
      Just tp -> (tp, fresh)
      Nothing -> (t, fresh)
    substitute t _ fresh = (t, fresh)

typeListElement :: Type -> Type
typeListElement (TAp (TCon (TConDataType dataType)) a)
  | dataType == idFromString "[]" = a
typeListElement tp = error $ "typeListElement: expected a list type, got " ++ showType tp ++ " instead"

typeTupleElements :: Type -> [Type]
typeTupleElements tupleType = elements 0 tupleType []
  where
    elements n (TCon (TConTuple m)) accum
      | n < m = error $ "typeTupleElements: expected a saturated tuple type, got a partially applied tuple type (received " ++ show n ++ " arguments, expected " ++ show m ++ ")"
      | n > m = error $ "typeTupleElements: got an over applied tuple type"
      | otherwise = accum
    elements n (TAp t1 t2) accum = elements (n + 1) t1 (t2 : accum)
    elements _ (TVar _) _ = error $ "typeTupleElements: expected a tuple type, got a type variable instead"
    elements _ tp _ = error $ "typeTupleElements: expected a tuple type, got " ++ showType tp ++ " instead"

typeExtractFunction :: Type -> ([Type], Type)
typeExtractFunction (TAp (TAp (TCon TConFun) t1) t2) = (t1 : args, ret)
  where
    (args, ret) = typeExtractFunction t2
typeExtractFunction tp = ([], tp)

typeApply :: Type -> Type -> Type
typeApply (TForall (Quantor x _ _) t1) t2 = typeSubstitute x t2 t1
typeApply t1 t2 = TAp t1 t2

typeApplyList :: Type -> [Type] -> Type
typeApplyList = foldl typeApply

typeUsedVars :: Type -> S.Set Int
typeUsedVars (TAp t1 t2) = typeUsedVars t1 `S.union` typeUsedVars t2
typeUsedVars (TForall (Quantor idx KStar _) tp) = S.insert idx $ typeUsedVars tp
typeUsedVars (TVar idx) = S.singleton idx
typeUsedVars _ = S.empty

typeFreeVars :: Type -> S.Set Int
typeFreeVars (TAp t1 t2) = typeFreeVars t1 `S.union` typeFreeVars t2
typeFreeVars (TForall (Quantor idx KStar _) tp) = S.delete idx $ typeFreeVars tp
typeFreeVars (TVar idx) = S.singleton idx
typeFreeVars _ = S.empty
