module Lvm.Core.Analyses.Types where

import Lvm.Core.Analyses.Annotations
import Lvm.Core.Analyses.Constraints
import Lvm.Core.Analyses.Utils

import Lvm.Common.Id
import Lvm.Core.Expr

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Text.PrettyPrint.Leijen (Pretty, pretty)

----------------------------------------------------------------
-- typeCheck
----------------------------------------------------------------
--typeCheck cm = const cm exprsCheck
--    where
--        exprs = mapMaybe declValue $ moduleDecls cm
--        declValue decl@(DeclValue _ _ _ expr _) = Just (decl, expr)
--        declValue _ = Nothing
--
--        exprsCheck = map (w envEmpty) exprs

freshPi :: Fresh T
freshPi = Alpha <$> fresh

freshGamma :: Fresh Ts
freshGamma = Gamma <$> fresh

type TSub = Map Pi T
type Env = Map Id Ts
data Ts = Forall (Set Ann) (Set Pi) (Set (Constraint Ann T Ts Env)) T
        | TsAnn1 (Ts) Ann -- Usage annotation
        | TsAnn2 (Ts) (Ann,Ann) -- Usage & Demand annotation
        | Gamma Pi
    deriving (Show, Eq, Ord)
data T = TFn T T
       | TAp T T
       | TCon Id
       | TAnn1 T Ann -- Usage annotation
       | TAnn2 T (Ann,Ann) -- Usage & Demand annotation
       | TAnnD T [Ann] -- Usage & Demand annotation on datatype
       | Alpha Pi
    deriving (Show, Eq, Ord)

{- Type to TypeScheme -}
t2ts :: T -> Ts
t2ts t = Forall Set.empty Set.empty Set.empty t

{- Environment -}
envLookup :: Env -> Id -> Ts
envLookup env x = Map.findWithDefault (error $ show x ++ " : not found in env") x env

envAppend :: Id -> Ts -> Env -> Env
envAppend = Map.insert

envEmpty :: Env
envEmpty = Map.empty

{- Constructors used -}
consInT :: T -> Set Id
consInT (TFn t1 t2) = Set.union (consInT t1) (consInT t2)
consInT (TAp t1 t2) = Set.union (consInT t1) (consInT t2)
consInT (TCon i) = Set.singleton i
consInT (TAnn1 t _) = consInT t
consInT (TAnn2 t _) = consInT t
consInT (Alpha _) = Set.empty

{- Free type variables -}
isFreeIn :: Pi -> T -> Bool
isFreeIn pi (TFn t1 t2) = pi `isFreeIn` t1 || pi `isFreeIn` t2
isFreeIn pi (TAp t1 t2) = pi `isFreeIn` t1 || pi `isFreeIn` t2
isFreeIn pi (TCon _) = False
isFreeIn pi (TAnn1 t _) = pi `isFreeIn` t
isFreeIn pi (TAnn2 t _) = pi `isFreeIn` t
isFreeIn pi (Alpha x) = pi == x

freeInEnv :: Env -> Set Pi
freeInEnv = foldr (\ts acc -> Set.union acc $ freeInTs ts) Set.empty

freeInTs :: Ts -> Set Pi
freeInTs (Forall annotations alphas constraints t) = freeInT t Set.\\ alphas

freeInT :: T -> Set Pi
freeInT t = case t of
    TFn t1 t2 -> Set.union (freeInT t1) (freeInT t2)
    TAp t1 t2 -> Set.union (freeInT t1) (freeInT t2)
    TCon _ -> Set.empty
    TAnn1 t1 _ -> freeInT t1
    TAnn2 t1 _ -> freeInT t1
    Alpha pi -> Set.singleton pi

{- Generalize -}
-- TODO: annotations & constraints
generalize :: Env -> T -> Ts
generalize env t = Forall betas alphas constraints t
    where
    betas = Set.empty
    alphas = freeInT t Set.\\ freeInEnv env
    constraints = Set.empty


{- Instantiate -}
-- TODO: annotations & constraints
instantiate :: Ts -> Fresh T
instantiate (Forall annotations alphas constraints t) = (-$-) <$> sub <*> pure t
    where
    sub :: Fresh (TSub)
    sub = do
        freshs <- sequence $ take (Set.size alphas) $ repeat freshPi
        return $ Map.fromList $ zip (Set.toList alphas) freshs

{- Substitutions -}
idSub :: TSub
idSub = Map.empty

alphasSub :: Set Pi -> TSub
alphasSub = Map.fromSet Alpha

sub :: Pi -> T -> TSub
sub a t = Map.singleton a t

(-.-) :: TSub -> TSub -> TSub
(-.-) sub1 sub2 = Map.union sub1 $ Map.map (sub1 -$-) sub2

(-$-) :: TSub -> T -> T
(-$-) subs t = case t of
    TFn t1 t2 -> TFn (subs -$- t1) (subs -$- t2)
    TAp t1 t2 -> TAp (subs -$- t1) (subs -$- t2)
    TCon _ -> t
    a@(Alpha pi) -> Map.findWithDefault a pi subs

{- Substitute Environment -}
-- TODO: check annotations & constraints
envSubs :: TSub -> Env -> Env
envSubs sub = Map.map (\(Forall annotations alphas constraints t) -> Forall annotations alphas constraints (alphasSub alphas -.- sub -$- t))

{- Unify -}
unify :: T -> T -> TSub
unify t1 t2 = case tryUnify t1 t2 of
    Right sub -> sub
    Left err -> error err

tryUnify :: T -> T -> Either String TSub
tryUnify t1 t2 = traceUnify t1 t2 $ case (t1, t2) of
    (TAp t3 t4, TAp t5 t6) -> do
        subs1 <- tryUnify t3 t5
        subs2 <- tryUnify (subs1 -$- t4) (subs1 -$- t6)
        Right $ subs2 -.- subs1
    (TFn t3 t4, TFn t5 t6) -> do
        subs1 <- tryUnify t3 t5
        subs2 <- tryUnify (subs1 -$- t4) (subs1 -$- t6)
        Right $ subs2 -.- subs1
    (TCon n1, TCon n2) -> if n1 == n2 then Right idSub else failUnify t1 t2
    (Alpha a1, a2@(Alpha _)) -> Right $ sub a1 a2
    (Alpha a1, t) -> if not $ a1 `isFreeIn` t then Right $ sub a1 t else failUnify t1 t2
    (t, a2@(Alpha _)) -> tryUnify a2 t
    _ -> failUnify t1 t2

failUnify :: T -> T -> Either String TSub
failUnify t1 t2 = Left $ "Unable to unify t1: " ++ show t1 ++ " | with t2: " ++ show t2
traceUnify :: T -> T -> Either String TSub -> Either String TSub
traceUnify t1 t2 trace = do -- creating a stacktrace on a fail
    case trace of
        Left err -> Left $ err ++ "\n=> trace: " ++ (fromLeft $ failUnify t1 t2)
        otherwise -> trace
    where fromLeft (Left err) = err -- failUnify always returns Left

{- W algorithm -}
w :: Env -> Expr -> Fresh (T, TSub)
w env expr = case expr of
    Lit lit -> return (TCon $ case lit of
            LitInt _ -> idFromString "Int"
            LitDouble _ -> idFromString "Double"
            LitBytes _ -> idFromString "Bytes"
        , idSub)
    Var id -> do
        t <- instantiate $ envLookup env id
        return (t, idSub)
    Con (ConId id) -> do
        t <- instantiate $ envLookup env id
        return (t, idSub)
    Con (ConTag expr arity) -> todo $ "need: Con (ConTag expr arity) => expr: " ++ p2s expr ++ " | arity: " ++ p2s arity --TODO:
    Lam id expr -> do
        a <- freshPi
        (t, subs) <- w (envAppend id (t2ts a) env) expr
        return (TFn (subs -$- a) t, subs)
    Ap  expr1 expr2 -> do
        (t1, subs1) <- w env expr1
        (t2, subs2) <- w (envSubs subs1 env) expr2
        a <- freshPi
        let subs3 = unify (subs2 -$- t1) (TFn t2 a)
        return (subs3 -$- a, subs3 -.- subs2 -.- subs1)
    Match id alts -> todo $ "need: Match id alts => id: " ++ p2s id ++ " | alts: " ++ p2s alts --TODO:
    Let binds expr -> case binds of
        Rec bs -> todo $ "need: Rec bs => bs: " ++ p2s bs --TODO:
        Strict b -> todo $ "need: Strict b => b: " ++ p2s b --TODO:
        NonRec b -> todo $ "need: NonRec b => b: " ++ p2s b --TODO:
    where
        p2s :: Pretty a => a -> String
        p2s = show . pretty
        todo :: String -> Fresh (T, TSub)
        todo s = const undefined (error s)