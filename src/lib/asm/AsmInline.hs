{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$
module AsmInline (asmInline) where

import Standard ( assert )
import Id       ( Id )
import IdMap    ( IdMap, emptyMap, extendMap, deleteMap, elemMap, lookupMap )
import Module   ( mapValues )

import Asm
import AsmOccur (asmOccur)

{---------------------------------------------------------------
  Inline environment maps identifiers to their definition
---------------------------------------------------------------}
type Env  = IdMap Expr

removeIds ids env
  = foldr deleteMap env ids

{---------------------------------------------------------------
  asmInline
---------------------------------------------------------------}
asmInline :: AsmModule -> AsmModule
asmInline mod
  = mapValues inlineTop (asmOccur mod)

inlineTop :: Top -> Top
inlineTop (Top params expr)
  = Top params (inlineExpr emptyMap expr)

inlineExpr :: Env -> Expr -> Expr
inlineExpr env expr
  = case expr of
      -- dead variable
      Let id (Note (Occur Never) e1) e2
                    -> inlineExpr env e2
      -- once
      Let id (Note (Occur Once) e1) e2
                    -> let e1' = inlineExpr env e1
                       in  inlineExpr (extendMap id e1' env) e2

      -- many
      Let id e1 e2  -> let env' = deleteMap id env
                       in Let id (inlineExpr env e1) (inlineExpr env' e2)

      Eval id e1 e2 -> let env' = deleteMap id env 
                       in Eval id (inlineExpr env e1) (inlineExpr env' e2)

      LetRec bs e   -> let (bs',env') = inlineBinds env bs 
                       in LetRec bs' (inlineExpr env' e)

      Match id alts -> assert (not (elemMap id env)) "AsmInline.inlineExpr: invalid inlined identifier"  $
                       Match id (inlineAlts env alts)

      Ap id []      -> case lookupMap id env of
                         Just e   -> e
                         Nothing  -> Ap id []
      Ap id args    -> let args0 = inlineExprs env args
                       in case lookupMap id env of
                            Just e   -> case e of
                                          Ap id1 args1 -> Ap id1 (args1 ++ args0)     -- flatten applications
                                          Eval id1 e1 (Ap id2 [])  | id1==id2         -- special case for the strict inliner
                                                       -> Eval id1 e1 (Ap id1 args0)   
                                          other        -> Let id e (Ap id args)       -- don't inline!
                            Nothing  -> Ap id args0
      Con id args   -> Con id  (inlineExprs env args)
      Prim id args  -> Prim id (inlineExprs env args)
      Lit lit       -> expr
      Note note e   -> inlineExpr env e   -- de-annotate

inlineExprs :: Env -> [Expr] -> [Expr]
inlineExprs env exprs
  = [inlineExpr env expr | expr <- exprs]

inlineBinds :: Env -> [(Id,Expr)] -> ([(Id,Expr)],Env)
inlineBinds env binds 
  = let env' = removeIds (map fst binds) env
    in ([(id,inlineExpr env' e) | (id,e) <- binds], env')

inlineAlts :: Env -> [Alt] -> [Alt]
inlineAlts env alts
  = [inlineAlt env alt | alt <- alts]

inlineAlt env (Alt pat expr)
  = Alt pat (inlineExpr (removeIds (patIds pat) env) expr)
    where
    patIds (PatVar id)     = [id]
    patIds (PatCon id ids) = (id:ids)
    patIds (PatLit lit)    = []
