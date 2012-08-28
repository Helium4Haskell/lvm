{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

----------------------------------------------------------------
-- Annotate let bound expression with their free variables
----------------------------------------------------------------
module Lvm.Core.FreeVar( coreFreeVar ) where

import Lvm.Common.IdSet( IdSet, emptySet, isEmptySet
            , listFromSet, setFromList
            , elemSet, insertSet, unionSets, unionSet, deleteSet, diffSet )
import Lvm.Core.Data
import Lvm.Core.Utils
import Debug.Trace

----------------------------------------------------------------
-- coreFreeVar
-- Annotate let bound expression with their free variables
----------------------------------------------------------------

coreFreeVar :: CoreModule -> CoreModule
coreFreeVar m
  = mapExpr (fvDeclExpr (globalNames m)) m

fvDeclExpr :: IdSet -> Expr -> Expr
fvDeclExpr globals expr
  = let (expr',fv) = fvExpr globals expr
    in if (isEmptySet fv)
        then Note (FreeVar fv) expr'
        else trace ("warning: CoreFreeVar.fvDeclExpr: top-level binding with free variables: "
                      ++ show (listFromSet fv)) (Note (FreeVar fv) expr')

fvBindExpr :: IdSet -> Expr -> (Expr, IdSet)
fvBindExpr globals expr
  = let (expr',fv) = fvExpr globals expr
    in  (Note (FreeVar fv) expr',fv)

fvExpr :: IdSet -> Expr -> (Expr,IdSet)
fvExpr globals expr
  = case expr of
      Let binds e
        -> let (expr',fv)       = fvExpr globals e
               (binds',fvbinds) = fvBinds globals binds
           in (Let binds' expr', diffSet (unionSet fvbinds fv) (setFromList (binders (listFromBinds binds))))
      Lam x e
        -> let (expr',fv) = fvExpr globals e
           in  (Lam x expr',deleteSet x fv)
      Match x alts
        -> let (alts',fvalts) = fvAlts globals alts
           in  (Match x alts',insertSet x fvalts)
      Ap e1 e2
        -> let (expr1',fv1)   = fvExpr globals e1
               (expr2',fv2)   = fvExpr globals e2
           in  (Ap expr1' expr2', unionSet fv1 fv2)
      Var x
        -> if (elemSet x globals)
            then (expr,emptySet)
            else (expr,insertSet x emptySet)
      Con (ConTag tag arity)
        -> let (tag',fv) = fvExpr globals tag
           in (Con (ConTag tag' arity),fv)
      Note n e
        -> let (expr',fv) = fvExpr globals e
           in  (Note n expr',fv)
      _
        -> (expr,emptySet)


fvAlts :: IdSet -> Alts -> (Alts,IdSet)
fvAlts globals alts
  = let alts' = mapAlts (\pat expr -> let (expr',fv)   = fvExpr globals expr                                          
                                      in  Alt pat (Note (FreeVar fv) expr')) alts
        fvs   = unionSets (map (\(Alt pat expr) -> diffSet (freeVar expr) (patBinders pat)) (alts'))
    in  (alts',fvs)

fvBinds :: IdSet -> Binds -> (Binds,IdSet)
fvBinds globals binds
  = case binds of
      NonRec (Bind x expr)
        -> nonrec NonRec x expr
      Strict (Bind x expr)
        -> nonrec Strict x expr
      _ 
        -> let binds' = mapBinds (\x rhs -> Bind x (fst (fvBindExpr globals rhs))) binds
               fvs    = unionSets (map (\(Bind _ rhs) -> freeVar rhs) (listFromBinds binds'))
           in  (binds',fvs)
  where
    nonrec make x expr
      = let (expr',fv) = fvBindExpr globals expr
        in if (elemSet x fv)
            then error "CoreFreeVar.fvBinds: non-recursive binding refers to itself? (do CoreNoShadow first?)"
            else (make (Bind x expr'),fv)

freeVar :: Expr -> IdSet
freeVar expr
  = case expr of
      Note (FreeVar fv) _  -> fv
      _                    -> error "CoreFreeVar.freeVar: no free variable annotation"
