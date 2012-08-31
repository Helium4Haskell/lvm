{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

----------------------------------------------------------------
-- Do "johnson" style lambda lifting
-- After this pass, each binding has either no free variables or no arguments.
-- maintains free variable information & normalised structure
----------------------------------------------------------------
module Lvm.Core.Lift ( coreLift ) where

import Data.List (foldl') 
import Data.Maybe
import Lvm.Common.Id      ( Id )
import Lvm.Common.IdMap   ( IdMap, elemMap, extendMap, lookupMap, emptyMap )
import Lvm.Common.IdSet   ( IdSet, elemSet, listFromSet, emptySet, foldSet
               , unionSet, sizeSet, setFromList )
import Lvm.Core.Expr
import Lvm.Core.Utils

----------------------------------------------------------------
-- The environment maps variables to variables that should
-- be supplied as arguments at each call site
----------------------------------------------------------------
data Env  = Env IdSet (IdMap [Id])     -- primitives && the free variables to be passed as arguments

elemFree :: Env -> Id -> Bool
elemFree (Env _ env) x
  = elemMap x env

lookupFree :: Env -> Id -> [Id]
lookupFree (Env _ env) x
  = fromMaybe [] (lookupMap x env)

isPrimitive :: Env -> Id -> Bool
isPrimitive (Env prim _) = (`elemSet` prim)

extendFree :: Env -> Id -> [Id] -> Env
extendFree (Env prim env) x fv
  = Env prim (extendMap x fv env)

----------------------------------------------------------------
-- coreLift
-- pre: [coreFreeVar]  each binding is annotated with free variables
--      [coreNoShadow] there is no shadowing
----------------------------------------------------------------
coreLift :: CoreModule -> CoreModule
coreLift m
  = fmap (liftExpr (Env primitives emptyMap)) m
  where
    primitives  = externNames m

liftExpr :: Env -> Expr -> Expr
liftExpr env expr
  = case expr of
      Let binds e
        -> let (binds',env') = liftBinds env binds
           in Let binds' (liftExpr env' e)
      Match x alts
        -> Match x (liftAlts env alts)
      Lam x e
        -> Lam x (liftExpr env e)
      Ap expr1 expr2
        -> Ap (liftExpr env expr1) (liftExpr env expr2)
      Var x
        -> foldl' (\e v -> Ap e (Var v)) expr (lookupFree env x)
      Con (ConTag tag arity)
        -> Con (ConTag (liftExpr env tag) arity)
      Note n e
        -> Note n (liftExpr env e)
      _
        -> expr

liftAlts :: Env -> Alts -> Alts
liftAlts env = mapAlts (\pat expr -> Alt pat (liftExpr env expr))

----------------------------------------------------------------
-- Lift binding groups
----------------------------------------------------------------

liftBinds :: Env -> Binds -> (Binds, Env)
liftBinds env binds
  = case binds of
      NonRec bind -> let ([bind'],env') = liftBindsRec env [bind]
                     in  (NonRec bind',env')      
      Rec recs    -> let (recs',env') = liftBindsRec env recs
                     in (Rec recs',env')
      Strict (Bind x rhs)
                  -> (Strict (Bind x (liftExpr env rhs)),env)
      


liftBindsRec :: Env -> [Bind] -> ([Bind],Env)
liftBindsRec env recs
  = let (ids,exprs)  = unzipBinds recs
        -- calculate the mutual free variables
        fvmap   = fixMutual (zip ids (map (liftedFreeVar env . freeVarSet) exprs))
        -- note these recursive equations :-)
        fvs     = map  (removeLifted env' .  listFromSet . snd) fvmap
        env'    = foldl insertLifted env (zip recs fvs)

        -- put the computed free variables back into the bindings as lambdas
        recs'  = zipWith (addLambdas env) fvs (zipWith Bind ids (map (liftExpr env') exprs))
    in (recs', env')

addLambdas :: Env -> [Id] -> Bind -> Bind
addLambdas env fv bind@(Bind x (Note (FreeVar _) expr))  
  | isAtomExpr env expr = bind
--   | isValueExpr expr    = Bind id (Note (FreeVar fvset) (Let (NonRec (Bind id (foldlStrict (\e v -> Ap e (Var v)) (Var id) fv))) (Var id)))
  | otherwise           = Bind x (Note (FreeVar emptySet) (foldr Lam expr fv))

addLambdas _ _ _
  = error "CoreLift.addLambdas: no free variable annotation. Do coreFreeVar first?"

insertLifted :: Env -> (Bind, [Id]) -> Env
insertLifted env (Bind x expr,fv)
  = if isAtomExpr env expr --  || isValueExpr expr)
     then env
     else extendFree env x fv

removeLifted :: Env -> [Id] -> [Id]
removeLifted env = filter (not . elemFree env)


fixMutual :: [(Id,IdSet)] -> [(Id,IdSet)]
fixMutual fvmap
  = let fvmap' = map addMutual fvmap
    in  if size fvmap' == size fvmap
         then fvmap
         else fixMutual fvmap'
  where
    addMutual (x,fv)
      = (x, foldSet addLocalFree fv fv)

    addLocalFree x fv0
      = case lookup x fvmap of
          Just fv1  -> unionSet fv0 fv1
          Nothing   -> fv0

    size xs
      = sum (map (sizeSet . snd) xs)


liftedFreeVar :: Env -> IdSet -> IdSet
liftedFreeVar env fv
  = unionSet fv (setFromList (concatMap (lookupFree env) (listFromSet fv)))

freeVarSet :: Expr -> IdSet
freeVarSet (Note (FreeVar fv) _) = fv
freeVarSet _ = error "CoreLetSort.freeVar: no annotation. Do coreFreeVar first?"

----------------------------------------------------------------
-- is an expression atomic: i.e. can we generate code inplace
----------------------------------------------------------------

isAtomExpr :: Env -> Expr -> Bool
isAtomExpr env expr
  = case expr of
      Ap e1 e2  -> isAtomExpr env e1 && isAtomExpr env e2
      Note _ e  -> isAtomExpr env e
      Var x     -> not (isPrimitive env x)
      Con _     -> True
      Lit _     -> True
      Let binds e  -> isAtomBinds env binds && isAtomExpr env e
      _         -> False

isAtomBinds :: Env -> Binds -> Bool
isAtomBinds env binds
  = case binds of
      Strict _             -> False
      NonRec (Bind _ expr) -> isAtomExpr env expr
      Rec bindings         -> all (isAtomExpr env) (snd (unzipBinds bindings))