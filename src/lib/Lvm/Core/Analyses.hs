{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------
-- Copyright 2018 Reinier Maas. This file
-- is distributed under the terms of the BSD3 License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------

module Lvm.Core.Analyses (coreAnalyses) where

import Control.Monad.State.Lazy (State, evalState, get, put)

import Data.List()
import Data.Maybe()
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Graph as Graph

import Lvm.Common.Id
import Lvm.Common.Byte
import Lvm.Core.Expr
import Lvm.Core.Module
import Lvm.Core.LetSort (topSort)
import qualified Lvm.Core.Type as Type
import Lvm.Core.Parsing.Lexer (lexer)
import Lvm.Core.Parsing.Parser (ptype)
import Text.ParserCombinators.Parsec (runParser)

import Text.PrettyPrint.Leijen (Pretty, pretty)

import qualified Debug.Trace as Trace

tracePretty :: Pretty a => String -> a -> a
tracePretty s x = trace s $ Trace.traceShow (pretty x) x

tracePretty' :: Pretty a => String -> a -> a
tracePretty' = flip const

traceShow :: Show a => a -> a
traceShow = Trace.traceShowId

traceShow' :: Show a => a -> a
traceShow' = id

tagTraceShow :: Show a => String -> a -> a
tagTraceShow s x = trace s $ traceShow x

trace :: String -> a -> a
trace = Trace.trace

trace' :: String -> a -> a
trace' = flip const

deriving instance Eq Type.Type
deriving instance Ord Type.Type

----------------------------------------------------------------
-- coreAnalyses
-- pre: [coreFreeVar]  each binding is annotated with free variables
--      [coreNoShadow] there is no shadowing
-- TODO: Maybe more?
----------------------------------------------------------------
coreAnalyses :: CoreModule -> CoreModule
coreAnalyses m = id $! trace (show $ annData (moduleDecls m)) corePrint $! tracePretty' "coreAnalyses => " m

corePrint :: CoreModule -> CoreModule
corePrint m = m{ moduleDecls = id $! declPrintCon $! declPrintData $! moduleDecls m }

declPrintData :: [Decl Expr] -> [Decl Expr]
declPrintData [] = []
declPrintData (decl:decls) = (case decl of
    DeclCustom{} -> case declKind decl of
        DeclKindCustom id -> tracePretty
            ("DeclCustom "
            ++ (show . pretty $ declKind decl)
            ++ ": "
            ++ printId id
            ++ " : "
            ++ (printCustoms $ declCustoms decl)
            ++ " : ")
            decl
        otherwise -> decl
    otherwise -> decl) : (declPrintData decls)

declPrintCon :: [Decl Expr] -> [Decl Expr]
declPrintCon [] = []
declPrintCon (decl:decls) = (case decl of
    DeclCon{} -> tracePretty
            ("DeclCon "
            ++ "(@"
            ++ (show $ conTag decl)
            ++ "," ++ (show $declArity decl)
            ++ ")"
            ++ " : "
            ++ (printCustoms $ declCustoms decl)
            ++ " : ")
            decl
    otherwise -> decl) : (declPrintCon decls)

printId :: Id -> String
printId x = "(Id " ++ (show $ intFromId x) ++ ")"

printCustoms :: [Custom] -> String
printCustoms [] = []
printCustoms (custom:customs) = "\n    " ++ (case custom of
    CustomInt x -> "CustomInt " ++ show x
    CustomBytes b -> "CustomBytes " ++ (show b) ++ " | " ++ (stringFromBytes b)
    CustomName i -> "CustomName " ++ show i
    CustomLink i declkind -> "CustomLink " ++ show i ++ " : " ++ show declkind
    CustomDecl declkind customs2 -> "CustomDecl " ++ show declkind ++ " :(" ++ printCustoms customs2 ++ ")"
    CustomNothing -> "CustomNothing") ++ printCustoms customs

-- more to come Cardinality and stuff


----------------------------------------------------------------
-- counting analysis
----------------------------------------------------------------

{- Annotations -}
data Ann = AnnVar Pi
         | AnnVal AnnValue
    deriving (Show, Eq, Ord)
type AnnValue = Set AnnPrim
data AnnPrim = Zero
             | One
             | Infinity
    deriving (Show, Eq, Ord)

annToInt :: AnnPrim -> Int
annToInt Zero = 0
annToInt One = 1
annToInt Infinity = 2;

annFromInt :: Int -> AnnPrim
annFromInt 0 = Zero
annFromInt 1 = One
annFromInt _ = Infinity

(.+) :: AnnPrim -> AnnPrim -> AnnPrim
x .+ y = annFromInt $ annToInt x + annToInt y

{- Named Annotations -}
annBot', annZero', annOne', annW', annTop' :: AnnValue
annBot' = Set.empty
annZero' = Set.singleton Zero
annOne' = Set.singleton One
annW' = Set.singleton Infinity
annTop' = Set.fromList [Zero, One, Infinity]

annBot, annZero, annOne, annW, annTop :: Ann
annBot = AnnVal annBot'
annZero = AnnVal annZero'
annOne = AnnVal annOne'
annW = AnnVal annW'
annTop = AnnVal annTop'

{- Annotation Operations -}
annPlus :: AnnValue -> AnnValue -> AnnValue
annPlus a1 a2 = Set.fromList [x .+ y | x <- Set.toList a1, y <- Set.toList a2]

annUnion :: AnnValue -> AnnValue -> AnnValue
annUnion = Set.union

annTimes :: AnnValue -> AnnValue -> AnnValue
annTimes a1 a2 = Set.fromList [annFromInt (sum $ map annToInt y) | x <- Set.toList a1, y <- f (annToInt x) $ Set.toList a2]
  where f 0 _ = [[]]
        f _ [] = []
        f n y@(x:xs) = f n xs ++ map (x:) (f (n-1) y)

annCond :: AnnValue -> AnnValue -> AnnValue
annCond a1 a2 = Set.unions $ map (\x -> if x == Zero then annZero' else a2) $ Set.toList a1

{- Constraints -}
type Constraints = Set Constraint
data Constraint = Eq Ann Ann -- phi1 == phi2
                | EqPlus Ann Ann Ann -- phi1 == phi2 (+) phi3
                | EqUnion Ann Ann Ann -- phi1 == phi2 (U) phi3
                | EqTimes Ann Ann Ann -- phi1 == phi2 (*) phi3
                | EqCond Ann Ann Ann -- phi1 == phi2 |> phi3
                | EqT T T -- t1 == t2
                | EqTs Ts Ts -- ts1 == ts2
                | InstEq Ts Ts -- inst(ts1) == t2
                | GenEq (T, Constraints, Env) Ts -- gen(t1(upsilon1, delta1), constraints, gamma) == ts2(upsilon2, delta2)
    deriving (Show, Eq, Ord)

----------------------------------------------------------------
-- annotate datatypes
----------------------------------------------------------------
--                  Data
type DataAnns = Map Id DataAnn
--                                             Cons
data DataAnn = DataAnn (Set Pi) (Set Ann) (Map Id T)
    deriving (Show, Eq)

dataAnn2cons :: DataAnn -> Set Id
dataAnn2cons (DataAnn _ _ m) = Set.unions $ map (consInT . snd) $ Map.toList m

dataAnn2alphas :: DataAnn -> Set Pi
dataAnn2alphas (DataAnn _ _ m) = Set.unions $ map (freeInT . snd) $ Map.toList m

alphasInDataAnn :: DataAnn -> DataAnn
alphasInDataAnn dataAnn@(DataAnn _ _ m) = DataAnn (dataAnn2alphas dataAnn) Set.empty m

alphasInDataAnns :: DataAnns -> DataAnns
alphasInDataAnns = Map.map alphasInDataAnn

annData :: [Decl Expr] -> DataAnns
annData decls = dataAlg $ alphasInDataAnns $ (foldr (decl2annCon (foldr typeSynonyms Map.empty decls)) (foldr decl2data Map.empty decls)) decls

-- all available type synonyms
type TypeSynonyms = Map Type.Type Type.Type
typeSynonyms :: Decl Expr -> TypeSynonyms -> TypeSynonyms
typeSynonyms decl@DeclCustom{} typeSynonyms | show (retrieveId $ declKind decl) == "\"typedecl\"" =
    trace
        ("typeSynonyms => DeclCustom "
        ++ (show . pretty $ declKind decl)
        ++ ": "
        ++ (show $ declName decl)
        ++ " |customs| "
        ++ (printCustoms $ declCustoms decl)
        ++ " | ")
        (Map.unionWithKey (\k _ _ -> error $ "Two typesynonyms defined for: " ++ (show k)) typeSynonyms (typeSynonymParser typesynonym))
    where
    retrieveId (DeclKindCustom i) = i
    typesynonym = let CustomBytes b = head $ declCustoms decl in stringFromBytes b
typeSynonyms _ typeSynonyms = typeSynonyms

typeSynonymParser :: String -> TypeSynonyms
typeSynonymParser typesynonym = Map.singleton lhs rhs
    where
    split (' ':'=':' ':rhs) = ([],rhs)
    split (lhs':splitting) = let (lhs,rhs) = split splitting in (lhs':lhs,rhs)
    (lhs,rhs) = let (lhs',rhs') = split typesynonym in (string2type lhs', string2type rhs')

-- all available datatypes
decl2data :: Decl Expr -> DataAnns -> DataAnns
decl2data decl@DeclCustom{} dataAnns | show (retrieveId $ declKind decl) == "\"data\"" =
    trace'
        ("decl2data => DeclCustom "
        ++ (show . pretty $ declKind decl)
        ++ ": "
        ++ (show $ declName decl)
        ++ " : "
        ++ (printCustoms $ declCustoms decl)
        ++ " : ")
        addData (declName decl) dataAnns
    where retrieveId (DeclKindCustom i) = i
decl2data _ dataAnns = dataAnns

-- all available constructors
addData :: Id -> DataAnns -> DataAnns
addData dataId dataAnns = Map.unionWith unionDataAnn dataAnns (Map.singleton dataId (DataAnn Set.empty Set.empty Map.empty))

decl2annCon :: TypeSynonyms -> Decl Expr -> DataAnns -> DataAnns
decl2annCon typesynonyms decl@DeclCon{} dataAnns =
    addAnnCon
        (customsData $ declCustoms decl)
        (declName decl)
        (customsT typesynonyms $ declCustoms decl)
        dataAnns
decl2annCon _ _ dataAnns = dataAnns

addAnnCon :: Id -> Id -> T -> DataAnns -> DataAnns
addAnnCon dataId conId ts dataAnns = Map.unionWith unionDataAnn dataAnns (Map.singleton dataId (DataAnn Set.empty Set.empty (Map.singleton conId ts)))

customsData :: [Custom] -> Id
customsData = foldr customDataLink (error "No data link found")

customDataLink :: Custom -> Id -> Id
customDataLink (CustomLink i (DeclKindCustom declkind)) _ | show declkind == "\"data\"" = i
customDataLink _ i = i

customsT :: TypeSynonyms -> [Custom] -> T
customsT typesynonyms cs = foldr (customT typesynonyms) (error $ "No type found: " ++ (show . pretty $ cs) ++ "\n=> PrintCustoms" ++ printCustoms cs) cs

customT :: TypeSynonyms -> Custom -> T -> T
customT typesynonyms (CustomDecl (DeclKindCustom declkind) [CustomBytes bs]) _ | show declkind == "\"type\"" = type2t typesynonyms $ string2type $ stringFromBytes bs
customT _ _ i = i

string2type :: String -> Type.Type
string2type string = case runParser ptype () "type" (lexer (1,1) string) of
    Right res -> res
    Left err -> error $ "Parsing type failed : " ++ string ++ " : " ++ (show err)

type2t :: TypeSynonyms -> Type.Type -> T
type2t typesynonyms t = case Map.lookup t typesynonyms of
    Just t' -> type2t typesynonyms t'
    Nothing -> case t of
        Type.TFun t1 t2 -> do
            let t1' = type2t typesynonyms t1
            let t2' = type2t typesynonyms t2
            TFn t1' t2'
        Type.TAp t1 t2 -> do
            let t1' = type2t typesynonyms t1
            let t2' = type2t typesynonyms t2
            TAp t1' t2'
        Type.TStrict t1 -> type2t typesynonyms t1 -- TODO: Forgot strictness
        Type.TVar i -> Alpha (intFromId i)
        Type.TCon i -> TCon i
        -- No analysis possible
        Type.TForall i t1 -> error $ "No constructor type? :: " ++ (show t)
        Type.TExist i t1 -> error $ "No constructor type? :: " ++ (show t)
        Type.TAny -> error $ "No constructor type? :: " ++ (show t)
        Type.TString s -> error $ "No constructor type? :: " ++ s

unionDataAnn :: DataAnn -> DataAnn -> DataAnn
unionDataAnn (DataAnn alphas1 anns1 cons1) (DataAnn alphas2 anns2 cons2) = DataAnn (Set.union alphas1 alphas2) (Set.union anns1 anns2) (Map.union cons1 cons2)

{- datatype algorithm -}
dataAlg :: DataAnns -> DataAnns
dataAlg = annotateDatass . sortData

-- zero -> no type annotations, one -> type annotations on each field, higher -> annotation variables generated inside types
iota :: Int
iota = 2 -- one level of annotation variables in types

annotateDatass :: [DataAnns] -> DataAnns
annotateDatass = foldl annotateDatas emptyDataEnv
    where emptyDataEnv = Map.empty

type DataEnv = DataAnns
--                    Data
--type DataAnns = Map Id DataAnn
--                                               Cons
--data DataAnn = DataAnn (Set Pi) (Set Ann) (Map Id T)
--local recursion needed here, fixpoint
annotateDatas :: DataEnv -> DataAnns -> DataEnv
--annotateDatas = Map.unionWith (\x1 x2 -> error $ "datatype analysed multiple times : " ++ (show x1) ++ " : " ++ (show x2))
annotateDatas env datas = do
    let datas' = evalState (Map.traverseWithKey (\_ -> annotateData env datas) datas) 0
    if datas == datas'
     then Map.unionWith (\x1 x2 -> error $ "datatype analysed multiple times : " ++ (show x1) ++ " : " ++ (show x2)) env datas
     else annotateDatas env datas'


annotateData :: DataEnv -> DataAnns -> DataAnn -> Fresh DataAnn
annotateData env current (DataAnn alphas betas cons) = do
    consbetas' <- Map.traverseWithKey (\_ -> annotateType env current) cons
    let betas' = Set.unions $ map (snd . snd) $ Map.toList consbetas'
    let cons' = Map.map fst consbetas'
    return $ DataAnn alphas betas' cons'

annotateType :: DataEnv -> DataAnns -> T -> Fresh (T, Set Ann)
annotateType env current t = do
    {-(t',betas) <- -}
    tagTraceShow "annotateType(1) => " <$> annotateType' env current t 1
    --tagTraceShow "annotateType(2) => " <$> if iota > 0
    -- then do
    --    upsilon <- freshAnn
    --    delta <- freshAnn
    --    return
    --        ( TAnn2 t' (upsilon, delta)
    --        , Set.union (Set.fromList [upsilon,delta]) betas)
    -- else return (TAnn2 t' (annTop, annTop), Set.empty)

annotateType' :: DataEnv -> DataAnns -> T -> Int -> Fresh (T, Set Ann)
annotateType' env current t level = traceShow' <$> case traceShow' $ t of
    a@Alpha{} -> return (a,Set.empty)
    TCon con -> processCon con []
    TAp _ _ -> case traceShow' $ trace' ("apstart => " ++ (show t)) $ apStart t [] of
        Right (con, ts') -> processCon con ts'
        Left (pi, ts') -> processAlpha pi ts'
    TFn t1 t2 -> do
        (t1',betas1) <- annotateType' env current t1 (level + 1)
        (t2',betas2) <- annotateType' env current t2 level
        if iota > level
         then do
            upsilon1 <- freshAnn
            delta1 <- freshAnn
            upsilon2 <- freshAnn
            return
                ( TFn (TAnn2 t1' (upsilon1, delta1)) (TAnn1 t2' upsilon2)
                , Set.union (Set.fromList [upsilon1, delta1, upsilon2]) (Set.union betas1 betas2))
         else
            return (TFn (TAnn2 t1' (annTop, annTop)) (TAnn1 t2' annTop), Set.empty)
    TAnn1 t' _ -> annotateType' env current t' level
    TAnn2 t' _ -> annotateType' env current t' level
    TAnnD t' _ -> annotateType' env current t' level
    x -> error $ "missing case: " ++ show x
    where
    apStart :: T -> [T] -> Either (Pi,[T]) (Id,[T])
    apStart (TAp t1 t2) ts = apStart t1 (t2:ts)
    apStart (TCon con) ts = Right (con, ts)
    apStart (Alpha pi) ts = Left (pi, ts)
    processCon :: Id -> [T] -> Fresh (T, Set Ann)
    processCon con ts' = do
        tsbetas <- sequence $ map (\t' -> annotateType' env current t' (level+1)) ts'
        let (ts, betas) = unzip tsbetas
        let betas' = Set.unions betas
        case Map.lookup con env of
            Just (DataAnn alphas betas'' cons) -> do
                betas''' <- sequence $ replicate (length betas'')
                    (if iota > level
                     then freshAnn
                     else return annTop)
                return $ traceShow' $
                    ( TAnnD (foldl TAp (TCon con) ts) betas'''
                    , (Set.union betas' $ Set.fromList betas''')
                        Set.\\ (Set.singleton annTop))
            Nothing -> case Map.lookup con current of
                Just (DataAnn alphas betas'' cons) -> do
                    let betas''' = if iota > level
                                    then Set.toList betas''
                                    else replicate (length betas'') annTop
                    return $ traceShow' $
                        ( TAnnD (foldl TAp (TCon con) ts) betas'''
                        , betas')
                Nothing -> return $ traceShow' $ trace' ("Not a datatype : " ++ (show con) ++ (show ts') ++ " : aps => " ++ (show ts)) (foldl TAp (TCon con) ts, Set.empty)
    processAlpha :: Pi -> [T] -> Fresh (T, Set Ann)
    processAlpha pi ts' = do
        tsbetas <- sequence $ map (\t' -> annotateType' env current t' (level+1)) ts'
        let (ts, betas) = unzip tsbetas
        let betas' = Set.unions betas
        return $ traceShow' $
            ( foldl TAp (Alpha pi) ts
            , betas')


{- topological sort (to reduce recursion in the annotator) -}
sortData :: DataAnns -> [DataAnns]
sortData dataAnns =
    let datas = Map.toList dataAnns
    in let names = Map.fromList $ zip (map fst datas) [0..]
    in let edges = concatMap (depends names) datas
    in let sorted = topSort (length names - 1) edges
    in let datas' = map (map (datas !!)) sorted
    in map Map.fromList datas'

depends :: Map Id Graph.Vertex -> (Id, DataAnn) -> [(Graph.Vertex,Graph.Vertex)]
depends names (i1,t) = foldr depend [] $ dataAnn2cons t
    where
    index = Map.findWithDefault (error "Analyses.depends: id not in data group??") i1 names
    depend x ds = maybe ds (\i2 -> (index,i2):ds) $ Map.lookup x names




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

type Pi = Int
type Name = String
type Fresh a = State Int a

fresh :: Fresh Int
fresh = do
    x <- get
    put $ x + 1
    return x

freshPi :: Fresh T
freshPi = Alpha <$> fresh

freshAnn :: Fresh Ann
freshAnn = AnnVar <$> fresh

type TSub = Map Pi T
type Env = Map Id Ts
data Ts = Forall (Set Ann) (Set Pi) (Set Constraint) T
        | TsAnn1 (Ts) Ann -- Usage annotation
        | TsAnn2 (Ts) (Ann,Ann) -- Usage & Demand annotation
        -- | Gamma Pi -- Maybe needed?
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
{-
    | Let       !Binds Expr
    | Match     !Id Alts
    | Ap        Expr Expr
    | Lam       !Id Expr
-}