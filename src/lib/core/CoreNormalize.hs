{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

----------------------------------------------------------------
-- Normalises Core:
-- * no lambda's, except directly at let-bindings
-- * each Ap argument is atomic & not a call to an instruction or external function
-- * each Ap target is atomic
--
-- an atomic expression is
-- * a Var
-- * a Lit
-- * a Con
-- * a normalised Ap
--
-- pre: [coreNoShadow, coreSaturate]
----------------------------------------------------------------
module CoreNormalize ( coreNormalize ) where

import Id     ( Id, NameSupply, splitNameSupply, splitNameSupplies, freshId )
import IdSet  ( IdSet, elemSet, setFromMap )
import Core

----------------------------------------------------------------
-- Environment: the name supply
----------------------------------------------------------------
data Env   = Env NameSupply !IdSet {- instructions + externs -}

uniqueId (Env supply directs)
  = fst (freshId supply)

splitEnv (Env s d)
  = let (s0,s1) = splitNameSupply s in (Env s0 d, Env s1 d)

splitEnvs (Env s d)
  = map (\s -> Env s d) (splitNameSupplies s)

isDirect (Env s d) id
  = elemSet id d

----------------------------------------------------------------
-- coreNormalise
----------------------------------------------------------------
coreNormalize :: NameSupply -> CoreModule -> CoreModule
coreNormalize supply mod
  = mapExprWithSupply (normDeclExpr primitives) supply mod
  where
    primitives  = setFromMap (externs mod)

normDeclExpr directs supply expr
  = normBind (Env supply directs) expr


----------------------------------------------------------------
-- Expression & bindings
----------------------------------------------------------------
normExpr env expr
  = let (env1,env2) = splitEnv env
        expr'       = normBind env1 expr
    in case expr' of
         Lam _ _  -> let id = uniqueId env2
                     in (Let (NonRec (Bind id expr')) (Var id))
         other    -> expr'

-- can return lambda's on top
normBind env expr
  = case expr of
      Let binds expr    -> let (env1,env2) = splitEnv env
                           in Let (normBinds env1 binds) (normExpr env2 expr)
      Case expr id alts -> let (env1,env2) = splitEnv env
                           in Case (normExpr env1 expr) id (normAlts env2 alts)
      Lam id expr       -> Lam id (normBind env expr)
      Note n expr       -> normBind env expr  -- de-annotate
      Ap expr1 expr2    -> normAp env expr
      other             -> expr

normBinds env binds
  = zipBindsWith (\env id expr -> Bind id (normBind env expr)) (splitEnvs env) binds

normAlts env alts
  = zipAltsWith (\env pat expr -> Alt pat (normExpr env expr)) (splitEnvs env) alts

normAp env expr
  = let (atom,f) = normAtom env expr
    in  (f atom)

-- returns an atomic expression + a function that adds the right bindings
normAtom env expr
  = case expr of
      Case {}           -> freshBinding
      Lam {}            -> freshBinding
      Let binds expr    -> let (env1,env2) = splitEnv env
                               (atom,f)    = normAtom env1 expr
                           in  (atom, Let (normBinds env2 binds) . f)
      Ap expr1 expr2    -> let (env1,env2) = splitEnv env
                               (atom,f)    = normAtom env1 expr1
                               (arg,g)     = normArg  env2 expr2
                           in (Ap atom arg, f . g)
      Note n expr       -> normAtom env expr  -- de-annotate
      other             -> (expr,id)
  where
    freshBinding         = let (env1,env2) = splitEnv env
                               expr'       = normBind env1 expr
                               id          = uniqueId env2
                           in  (Var id, Let (NonRec (Bind id expr')))

-- just as an atomic expression but binds 'direct' applications (ie. externs & instructions)
normArg env expr
  = let (env1,env2) = splitEnv env
        (atom,f)    = normAtom env1 expr
    in  if (isDirectAp env atom)
         then let id = uniqueId env2
              in  (Var id, f . Let (NonRec (Bind id atom)))
         else (atom,f)

isDirectAp env expr
  = case expr of
      Ap e1 e2  -> isDirectAp env e1
      Note n e  -> isDirectAp env e
      Var id    -> isDirect env id
      other     -> False
