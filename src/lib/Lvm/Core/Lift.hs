--------------------------------------------------------------------------------
-- Copyright 2001-2012, Bastiaan Heeren, Jurriaan Hage, Daan Leijen. This file 
-- is distributed under the terms of the GNU General Public License. For more 
-- information, see the file "LICENSE.txt", which is included in the 
-- distribution.
--------------------------------------------------------------------------------
--  $Id$

----------------------------------------------------------------
-- Do "johnson" style lambda lifting
-- After this pass, each binding has either no free variables or no arguments.
-- maintains free variable information & normalised structure
----------------------------------------------------------------
module Lvm.Core.Lift (coreLift) where

import Data.List
import Data.Maybe
import Lvm.Common.Id     
import Lvm.Common.IdMap   
import Lvm.Common.IdSet
import Lvm.Core.Expr
import Lvm.Core.FreeVar
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
  = fmap (liftExpr globals (Env primitives emptyMap)) m
  where
    primitives = externNames m
    globals    = globalNames m

liftExpr :: IdSet -> Env -> Expr -> Expr
liftExpr globals env expr
  = case expr of
      Let binds e
        -> let (binds',env') = liftBinds globals env binds
           in Let binds' (liftExpr globals env' e)
      Match x alts
        -> Match x (liftAlts globals env alts)
      Lam x e
        -> Lam x (liftExpr globals env e)
      Ap expr1 expr2
        -> Ap (liftExpr globals env expr1) (liftExpr globals env expr2)
      Var x
        -> foldl' (\e v -> Ap e (Var v)) expr (lookupFree env x)
      Con (ConTag tag arity)
        -> Con (ConTag (liftExpr globals env tag) arity)
      _
        -> expr

liftAlts :: IdSet -> Env -> Alts -> Alts
liftAlts globals env = mapAlts (\pat expr -> Alt pat (liftExpr globals env expr))

----------------------------------------------------------------
-- Lift binding groups
----------------------------------------------------------------

liftBinds :: IdSet -> Env -> Binds -> (Binds, Env)
liftBinds globals env binds
  = case binds of
      NonRec bind -> let ([bind'],env') = liftBindsRec globals env [bind]
                     in  (NonRec bind',env')      
      Rec recs    -> let (recs',env') = liftBindsRec globals env recs
                     in (Rec recs',env')
      Strict (Bind x rhs)
                  -> (Strict (Bind x (liftExpr globals env rhs)),env)
      
freeVar2 :: IdSet -> Expr -> IdSet
freeVar2 globals = (`diffSet` globals) . freeVar

liftBindsRec :: IdSet -> Env -> [Bind] -> ([Bind],Env)
liftBindsRec globals env recs
  = let (ids,exprs)  = unzipBinds recs
        -- calculate the mutual free variables
        fvmap   = fixMutual (zip ids (map (liftedFreeVar env . freeVar2 globals) exprs))
        -- note these recursive equations :-)
        fvs     = map  (removeLifted env' .  listFromSet . snd) fvmap
        env'    = foldl insertLifted env (zip recs fvs)

        -- put the computed free variables back into the bindings as lambdas
        recs'  = zipWith (addLambdas env) fvs (zipWith Bind ids (map (liftExpr globals env') exprs))
    in (recs', env')

addLambdas :: Env -> [Id] -> Bind -> Bind
addLambdas env fv bind@(Bind x expr)
  | isAtomExpr env expr = bind
  | otherwise           = Bind x (foldr Lam expr fv)

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

----------------------------------------------------------------
-- is an expression atomic: i.e. can we generate code inplace
----------------------------------------------------------------

isAtomExpr :: Env -> Expr -> Bool
isAtomExpr env expr
  = case expr of
      Ap e1 e2  -> isAtomExpr env e1 && isAtomExpr env e2
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