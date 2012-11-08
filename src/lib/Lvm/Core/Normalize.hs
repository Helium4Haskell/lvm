--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file 
-- is distributed under the terms of the BSD3 License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id$

----------------------------------------------------------------
-- Normalises Core:
--  * no lambda's, except directly at let-bindings
--  * each Ap argument is atomic & not a call to an instruction or external function
--  * each Ap target is atomic
--
-- an atomic expression is
--  * a Var
--  * a Lit
--  * a Con
--  * a normalised Ap
--  * a normalised Let(Rec) expression 
--
-- pre: [coreNoShadow, coreSaturate]
----------------------------------------------------------------
module Lvm.Core.Normalize (coreNormalize) where

import Lvm.Common.Id
import Lvm.Common.IdSet
import Lvm.Core.Expr
import Lvm.Core.Utils

----------------------------------------------------------------
-- Environment: the name supply
----------------------------------------------------------------
data Env   = Env NameSupply !IdSet {- instructions + externs -}

uniqueId :: Env -> Id
uniqueId (Env supply _) = fst (freshId supply)

splitEnv :: Env -> (Env, Env)
splitEnv (Env s d)
  = let (s0,s1) = splitNameSupply s in (Env s0 d, Env s1 d)

splitEnvs :: Env -> [Env]
splitEnvs (Env s d) = map (`Env` d) (splitNameSupplies s)

isDirect :: Env -> Id -> Bool
isDirect (Env _ d) x = elemSet x d

----------------------------------------------------------------
-- coreNormalise
----------------------------------------------------------------
coreNormalize :: NameSupply -> CoreModule -> CoreModule
coreNormalize supply m
  = mapExprWithSupply (normDeclExpr primitives) supply m
  where
    primitives  = externNames m

normDeclExpr :: IdSet -> NameSupply -> Expr -> Expr
normDeclExpr directs supply = normBind (Env supply directs)

----------------------------------------------------------------
-- Expression & bindings
----------------------------------------------------------------

normExpr :: Env -> Expr -> Expr
normExpr env expr
  = let (env1,env2) = splitEnv env
        expr'       = normBind env1 expr
    in case expr' of
         Lam _ _  -> let x = uniqueId env2
                     in (Let (NonRec (Bind x expr')) (Var x))
         _        -> expr'

-- can return lambda's on top
normBind :: Env -> Expr -> Expr
normBind env expr
  = case expr of
      Let binds e       -> let (env1,env2) = splitEnv env
                           in Let (normBinds env1 binds) (normExpr env2 e)
      Match x alts      -> Match x (normAlts env alts)
      Lam x e           -> Lam x (normBind env e)
      Ap _ _            -> normAtomExpr env expr
      _                 -> expr

normBinds :: Env -> Binds -> Binds
normBinds
  = zipBindsWith (\env x expr -> Bind x (normBind env expr)) . splitEnvs

normAlts :: Env -> Alts -> Alts
normAlts
  = zipAltsWith (\env pat expr -> Alt pat (normExpr env expr)) . splitEnvs

normAtomExpr :: Env -> Expr -> Expr
normAtomExpr env expr
  = let (atom,f) = normAtom env expr
    in  (f atom)

-- returns an atomic expression + a function that adds the right bindings
normAtom :: Env -> Expr -> (Expr, Expr -> Expr)
normAtom env expr
  = case expr of
      Match _ _         -> freshBinding
      Lam _ _           -> freshBinding
      Let (Strict _) _  -> freshBinding
      -- we could leave let bindings in place when they are fully
      -- atomic but otherwise the bindings get messed up (shadow7.core).
      -- we lift all bindings out and rely on asmInline to put them
      -- back again if possible.
      Let binds e       -> let (env1,env2) = splitEnv env
                               (atom,f)    = normAtom env1 e
                               -- (abinds,g)  = normAtomBinds env2 binds
                           in  (atom, Let (normBinds env2 binds) . f)
                               -- (abinds atom, f . g)
      Ap e1 e2          -> let (env1,env2) = splitEnv env
                               (atom,f)    = normAtom env1 e1
                               (arg,g)     = normArg  env2 e2
                           in (Ap atom arg, f . g)
      _                 -> (expr,id)
  where
    freshBinding         = let (env1,env2) = splitEnv env
                               expr'       = normBind env1 expr
                               x           = uniqueId env2
                           in  (Var x, Let (NonRec (Bind x expr')))

-- normAtomBinds returns two functions: one that adds atomic
-- let bindings and one that adds non-atomic bindings
{- normAtomBinds :: Env -> Binds -> (Expr -> Expr, Expr -> Expr)
normAtomBinds env binds
  = let (binds',(env',f)) = mapAccumBinds norm (env,id) binds 
    in (Let binds', f)
  where
    norm (env,f) id expr    = let (env1,env2) = splitEnv env
                                  (atom,g)    = normAtom env1 expr
                              in (Bind id atom, (env2, f . g))
-}
-- just as an atomic expression but binds 'direct' applications (ie. externs & instructions)
normArg :: Env -> Expr -> (Expr, Expr -> Expr)
normArg env expr
  = let (env1,env2) = splitEnv env
        (atom,f)    = normAtom env1 expr
    in  if isDirectAp env atom
         then let x = uniqueId env2
              in  (Var x, f . Let (NonRec (Bind x atom)))
         else (atom,f)

isDirectAp :: Env -> Expr -> Bool
isDirectAp env expr
  = case expr of
      Ap e1 _   -> isDirectAp env e1
      Var x     -> isDirect env x
      _         -> False
