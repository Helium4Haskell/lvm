{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module AsmToLvm( asmToLvm )  where

import List   ( partition)
import Id     ( Id )
import IdMap  ( IdMap, emptyMap, lookupMap, extendMap, mapMapWithId
              , unionMaps, filterMap, mapMap, listFromMap, mapFromList
              )
import Asm
import Lvm

import Instr hiding  ( Con(..), Alt(..), Pat(..) )
import qualified Instr

import InstrResolve   ( instrResolve )
import InstrRewrite   ( instrRewrite )


{---------------------------------------------------------------
  asmToLvm: generate instructions from Asm expressions
---------------------------------------------------------------}
asmToLvm :: AsmModule -> LvmModule
asmToLvm mod
  = mapValues (codegen env) mod
  where
    env = initialEnv mod

codegen :: Env -> Top -> [Instr]
codegen env top
  = instrRewrite (instrResolve (cgTop env top))

{---------------------------------------------------------------
  top-level declarations
---------------------------------------------------------------}
cgTop :: Env -> Top -> [Instr]
cgTop env (Top params expr)
  = [ARGCHK (length params)] ++ cgParams env params ++ cgExpr env expr

cgParams env params
  = map PARAM (reverse params)

{---------------------------------------------------------------
 expressions
---------------------------------------------------------------}
cgExpr :: Env -> Expr -> [Instr]
cgExpr env expr
  = case expr of
      LetRec binds expr -> cgRec env binds ++ cgExpr env expr
      Let id atom expr  -> cgAtom env atom ++ [VAR id] ++ cgExpr env expr
      Eval id expr expr'-> [EVAL (cgExpr env expr)] ++ [VAR id] ++ cgExpr env expr'
      Match id alts     -> cgVar env id ++ cgMatch env alts
      Prim id args      -> cgPrim env id args
      Atom atom         -> [RESULT (cgAtom env atom)]
      other             -> error "AsmToCode.cgExpr: undefined case"

{---------------------------------------------------------------
 recursive bindings
---------------------------------------------------------------}
cgRec env binds
  = concat (map (cgAlloc env) binds ++ map (cgInit env) binds)

cgAlloc env (id,atom)
  = case atom of
      Asm.Ap x args   -> [ALLOCAP (length args + 1),VAR id]
      Asm.Con x args  -> [ALLOCCON (conFromId x (length args) env),VAR id]
      Asm.Lit lit     -> error "AsmToCode.cgAlloc: literal in recursive binding."

cgInit env (id,atom)
  = case atom of
      Asm.Ap x args   -> cgArgs env args ++ cgVar env x ++ [PACKAP (varFromId id) (length args + 1)]
      Asm.Con x args  -> cgArgs env args ++ [PACKCON (conFromId x (length args) env) (varFromId id)]
      Asm.Lit lit     -> error "AsmToCode.cgInit: literal in recursive binding."

{---------------------------------------------------------------
  alternatives
  result alternatives are 'normalized': the default alternative
  is always there and comes first.
---------------------------------------------------------------}
cgMatch env alts
  = case partition isVarAlt alts of
      ([],alts)    -> cgAlts env (Instr.Alt Instr.PatDefault []) alts
      ([alt],alts) -> cgAlts env (cgAlt env alt) alts
      (vars,alts)  -> error "AsmToCode.cgMatch: multiple default patterns"
  where
    isVarAlt (Alt (PatVar _) _)   = True
    isVarAlt other                = False

cgAlts env def alts
  | all isConAlt alts   = [MATCHCON (def:map (cgAlt env) alts)]
  | all isIntAlt alts   = [MATCHINT (def:map (cgAlt env) alts)]
  | otherwise           = error "AsmToCode.cgMatch: unknown or mixed type patterns"
  where
    isConAlt (Alt (PatCon _ _) _) = True
    isConAlt other                = False

    isIntAlt (Alt (PatLit (LitInt _)) _)  = True
    isIntAlt other                        = False


cgAlt env (Alt pat expr)
  = case pat of
      PatCon id params  -> Instr.Alt (Instr.PatCon (conFromId id (length params) env))
                                    (map PARAM (reverse params) ++ cgExpr env expr)
      PatLit (LitInt i) -> Instr.Alt (Instr.PatInt i) (cgExpr env expr)
      PatVar id         -> Instr.Alt (Instr.PatDefault) ([PARAM id] ++ cgExpr env expr)
      other             -> error "AsmToCode.cgAlt: unknown pattern"


{---------------------------------------------------------------
  primitives
---------------------------------------------------------------}
cgPrim env id args
  = case lookupInstr id env of
      Nothing    -> case lookupGlobal id env of
                      Nothing    -> error ("AsmToCode.cgPrim: unknown primitive " ++ show id)
                      Just arity -> if (arity /= length args)
                                     then error ("AsmToCode.cgPrim: unsaturated primitive " ++ show id)
                                     else result (CALL (Global id 0 arity))
      Just instr -> if (isCATCH instr)
                     then case args of
                            [handler,atom] -> cgAtom env handler ++ [CATCH [RESULT (cgAtom env atom)]]
                            other          -> error ("AsmToCode.cgPrim: CATCH expects 2 arguments")
                     else result instr
  where
    result instr  = [RESULT (cgArgs env args ++ [instr])]

{---------------------------------------------------------------
  atomic expressions
---------------------------------------------------------------}
cgAtom env atom
  = case atom of
      Ap id args  -> cgArgs env args ++ cgVar env id ++
                      (if null args then [] else [NEWAP (length args + 1)])
      Con id args -> cgArgs env args ++ [NEWCON (conFromId id (length args) env) ]
      Lit lit     -> cgLit lit

cgArgs env args
  = concatMap (cgAtom env) (reverse args)


{---------------------------------------------------------------
  literals and variables
---------------------------------------------------------------}
cgLit lit
  = case lit of
      LitInt i    -> [PUSHINT i]
      LitFloat d  -> [PUSHFLOAT d]
      LitBytes b  -> [PUSHBYTES b 0]


cgVar env id
  = case lookupGlobal id env of
      Nothing     -> [PUSHVAR (varFromId id)]
      Just arity  -> [PUSHCODE (Global id 0 arity)]



{---------------------------------------------------------------
  the code generation environment
---------------------------------------------------------------}
data Env  = Env{ arities :: IdMap Arity
               , instrs  :: IdMap Instr
               , cons    :: IdMap (Tag,Arity)
               }

lookupInstr id env
  = lookupMap id (instrs env)

lookupGlobal id env
  = lookupMap id (arities env)

varFromId id
  = Var id 0 0

conFromId id argcount env
  = case lookupMap id (cons env) of
      Just (tag,arity)  -> if (arity /= argcount)
                            then error ("AsmToCode.conFromId: unsaturated constructor " ++ show id)
                            else Instr.Con id 0 arity tag
      Nothing           -> error ("AsmToCode.conFromId: undeclared constructor " ++ show id)


-- create an initial environment from the declarations
initialEnv :: AsmModule -> Env
initialEnv mod
  = Env globals instrs cons
  where
    globals         = unionMaps [declared,imported,external]
    declared        = mapMap arityFromValue  (mapFromList (values mod))
    imported        = mapMap importArity (imports mod)
    external        = mapMap externArity (externs mod)

    instrs          = mapMap instrFromEx (filterMap isInstr (externs mod))
    cons            = mapMap (\con -> (conTag con, conArity con)) (constructors mod)

    arityFromValue (DValue acc enc (Top params x) custom)
                    = length params

    isInstr (DExtern acc arity tp link call lib name custom)
                    = call == CallInstr

    instrFromEx x   = case externName x of
                        Plain s    -> instrFromName s
                        Decorate s -> instrFromName s
                        Ordinal i  -> instrFromOpcode i
