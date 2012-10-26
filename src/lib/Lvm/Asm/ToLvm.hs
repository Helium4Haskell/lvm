--------------------------------------------------------------------------------
-- Copyright 2001-2012, Bastiaan Heeren, Jurriaan Hage, Daan Leijen. This file 
-- is distributed under the terms of the GNU General Public License. For more 
-- information, see the file "LICENSE.txt", which is included in the 
-- distribution.
--------------------------------------------------------------------------------
--  $Id$

module Lvm.Asm.ToLvm (asmToLvm)  where

import Control.Exception ( assert )
import Data.List 
import Lvm.Asm.Data
import Lvm.Common.Id
import Lvm.Common.IdMap 
import Lvm.Data
import Lvm.Instr.Data hiding  ( Con(..), Alt(..), Pat(..) )
import Lvm.Instr.Resolve   (instrResolve)
import Lvm.Instr.Rewrite   (instrRewrite)
import qualified Lvm.Asm.Data as Asm
import qualified Lvm.Instr.Data as Instr

{---------------------------------------------------------------
  asmToLvm: generate instructions from Asm expressions
---------------------------------------------------------------}
asmToLvm :: AsmModule -> LvmModule
asmToLvm m = fmap (codegen (initialEnv m)) m

codegen :: Env -> Top -> [Instr]
codegen env = instrRewrite . instrResolve . cgTop env

{---------------------------------------------------------------
  top-level declarations
---------------------------------------------------------------}
cgTop :: Env -> Top -> [Instr]
cgTop env (Top params expr)
  = [ARGCHK (length params)] ++ [ATOM (cgParams env params ++ cgExpr env expr)] ++ [ENTER]

cgParams :: Env -> [Id] -> [Instr]
cgParams _ = map PARAM . reverse

{---------------------------------------------------------------
 expressions
---------------------------------------------------------------}
cgExpr :: Env -> Expr -> [Instr]
cgExpr env expr
  = case expr of
      -- optimized schemes
      Eval id1 (Note (Occur Once) e1) (Match id2 alts)  | id1 == id2 && whnf env e1
                        -> ATOM (cgExpr env e1) : cgMatch env alts
      Eval id1 (Note (Occur Once) e1) (Match id2 alts)  | id1 == id2 
                        -> EVAL 0 [ATOM (cgExpr env e1),ENTER] : cgMatch env alts
      Eval x e1 e2     | whnf env e1
                        -> [ATOM (cgExpr env e1),VAR x] ++ cgExpr env e2
      Eval id1 e1 (Ap id2 []) | id1 == id2
                        -> cgExpr env e1

      -- basic cases
      LetRec binds e    -> cgLetRec env binds ++ cgExpr env e
      Let x atom e      -> cgLet env x atom ++ cgExpr env e
      Eval x e1 e2      -> [EVAL 0 [ATOM (cgExpr env e1),ENTER],VAR x] ++ cgExpr env e2
      Match x alts      -> cgVar env x ++ cgMatch env alts
      Prim x args       -> cgPrim env x args
      Note _ e          -> cgExpr env e
      atom              -> cgAtom env atom

{---------------------------------------------------------------
 let bindings
---------------------------------------------------------------}

cgLet :: Env -> Id -> Expr -> [Instr]
cgLet env x atom = cgAtom env atom ++ [VAR x]

cgLetRec :: Env -> [(Id, Atom)] -> [Instr]
cgLetRec env binds = concat (map (cgAlloc env) binds ++ map (cgInit env) binds)

cgAlloc :: Env -> (Id, Atom) -> [Instr]
cgAlloc env (x,atom) = [ATOM (cgAlloc' env atom),VAR x]

cgAlloc' :: Env -> Atom -> [Instr]
cgAlloc' env atom
  = case atom of
      Asm.Ap _ args    -> [ALLOCAP (length args + 1)]
      Asm.Let _ _ e2   -> cgAlloc' env e2
      Asm.LetRec _ e2  -> cgAlloc' env e2
      Asm.Note _ e     -> cgAlloc' env e
      Asm.Con (ConId x) args   
                       -> [ALLOCCON (conFromId x (length args) env)]
      Asm.Con (ConTag tag arity) args  -- TODO: tag may not be recursively bound!
                       -> assert (arity == length args) $ -- "AsmToCode.cgAlloc': constructor arity doesn't match arguments"
                          [PUSHINT arity] ++ cgAtom env tag ++ [ALLOC]
      Asm.Lit _        -> error "AsmToCode.cgAlloc': literal in recursive binding."
      _                -> error "AsmToCode.cgAlloc': non-atomic expression encountered."

cgInit :: Env -> (Id, Atom) -> [Instr]
cgInit env (x,atom) = [INIT (cgInit' env x atom)]

cgInit' :: Env -> Id -> Atom -> [Instr]
cgInit' env x atom
  = case atom of
      Asm.Ap y args    -> cgArgs env args ++ cgVar env y ++ [PACKAP (varFromId x) (length args + 1)]
      Asm.Let y e1 e2  -> cgLet env y e1 ++ cgInit' env y e2
      Asm.LetRec bs e2 -> cgLetRec env bs ++ cgInit' env x e2
      Asm.Note _ e     -> cgInit' env x e
      Asm.Con (ConId y) args   
                       -> cgArgs env args ++ [PACKCON (conFromId y (length args) env) (varFromId x)]
      Asm.Con (ConTag _ arity) args
                       -> cgArgs env args ++ [PACK arity (varFromId x)]
      Asm.Lit _        -> error "AsmToCode.cgInit: literal in recursive binding."
      _                -> error "AsmToCode.cgInit: non-atomic expression encountered."



{---------------------------------------------------------------
  alternatives
  result alternatives are 'normalized': the default alternative
  is always there and comes first.
---------------------------------------------------------------}

cgMatch :: Env -> [Alt] -> [Instr]
cgMatch env alts
  = case partition isVarAlt alts of
      ([],as)    -> cgAlts env (Instr.Alt Instr.PatDefault []) as
      ([alt],as) -> cgAlts env (cgAlt env alt) as
      _          -> error "AsmToCode.cgMatch: multiple default patterns"
  where
    isVarAlt (Alt (PatVar _) _)   = True
    isVarAlt _                    = False

cgAlts :: Env -> Instr.Alt -> [Alt] -> [Instr]
cgAlts env def alts
  | all isConIdAlt alts = [MATCHCON (def:map (cgAlt env) alts)]
  | all isConAlt alts   = [MATCH    (def:map (cgAltTag env) alts)]
  | all isIntAlt alts   = [MATCHINT (def:map (cgAlt env) alts)]
  | otherwise           = error "AsmToCode.cgMatch: unknown or mixed type patterns"
  where
    isConIdAlt (Alt (PatCon (ConId _) _) _) = True
    isConIdAlt _                            = False

    isConAlt (Alt (PatCon _ _) _) = True
    isConAlt _                    = False

    isIntAlt (Alt (PatLit (LitInt _)) _)  = True
    isIntAlt _                            = False

cgAlt :: Env -> Alt -> Instr.Alt
cgAlt env (Alt pat expr)
  = case pat of
      PatCon (ConId x) params  
          -> Instr.Alt (Instr.PatCon (conFromId x (length params) env))
                       [ATOM (map PARAM (reverse params) ++ cgExpr env expr)]
      PatLit (LitInt i) 
          -> Instr.Alt (Instr.PatInt i) [ATOM (cgExpr env expr)]
      PatVar x         
          -> Instr.Alt Instr.PatDefault [ATOM (PARAM x : cgExpr env expr)]
      _             
          -> error "AsmToCode.cgAlt: unknown pattern"

cgAltTag :: Env -> Alt -> Instr.Alt
cgAltTag env (Alt pat expr)
  = case pat of
      PatCon (ConTag tag arity) params  
          -> Instr.Alt (Instr.PatTag tag arity)
                       [ATOM (map PARAM (reverse params) ++ cgExpr env expr)]                      
      PatCon (ConId x) params  
          -> let (tag,arity) = tagArityFromId x (length params) env
             in Instr.Alt (Instr.PatTag tag arity)
                       [ATOM (map PARAM (reverse params) ++ cgExpr env expr)]
      PatVar x         
          -> Instr.Alt Instr.PatDefault [ATOM (PARAM x : cgExpr env expr)]
      _             
          -> error "AsmToCode.cgAltTag: invalid pattern"

{---------------------------------------------------------------
  primitives
---------------------------------------------------------------}

cgPrim :: Env -> Id -> [Atom] -> [Instr]
cgPrim env x args
  = case lookupInstr x env of
      Nothing    -> case lookupGlobal x env of
                      Nothing    -> error ("AsmToCode.cgPrim: unknown primitive " ++ show x)
                      Just arity -> if arity /= length args
                                     then error ("AsmToCode.cgPrim: unsaturated primitive " ++ show x)
                                     else result (CALL (Global x 0 arity))
      Just instr
         | isCATCH instr -> case args of
                            [handler,atom] -> let y = idFromString "@catch@" in
                                              cgAtom env handler ++ 
                                              [CATCH [EVAL 0 (cgAtom env atom ++[ENTER]),VAR y]]
                            _              -> error "AsmToCode.cgPrim: CATCH expects 2 arguments"
         | otherwise -> result instr
  where
    result instr  = [ATOM (cgArgs env args ++ [instr])]

{---------------------------------------------------------------
  atomic expressions
---------------------------------------------------------------}

cgAtom :: Env -> Expr -> [Instr]
cgAtom env atom = [ATOM (cgAtom' env atom)]

cgAtom' :: Env -> Expr -> [Instr]
cgAtom' env atom
  = case atom of
      Ap x args    -> cgArgs env args ++ cgVar env x ++
                      (if null args then [] else [NEWAP (length args + 1)])
      Lit lit      -> cgLit lit
      Let x e1 e2  -> cgLet env x e1 ++ cgAtom' env e2
      LetRec bs e2 -> cgLetRec env bs ++ cgAtom' env e2
      Note _ e     -> cgAtom' env e
      Con (ConId x) args 
                  -> cgArgs env args ++ [NEWCON (conFromId x (length args) env) ]
      Con (ConTag tag arity) args 
                  -> cgArgs env args ++ cgAtom env tag ++ [NEW arity]
            
      -- optimizer: inlined strict bindings 
      Eval x e1 e2  | whnf env e1
                  -> [ATOM (cgExpr env e1), VAR x] ++ cgAtom' env e2
      Eval x e1 e2
                  -> [EVAL 0 [ATOM (cgExpr env e1), ENTER], VAR x] ++ cgAtom' env e2


      _           -> error "AsmToCode.cgAtom: non-atomic expression encountered"

cgArgs :: Env -> [Atom] -> [Instr]
cgArgs env args
  = concatMap (cgAtom env) (reverse args)


{---------------------------------------------------------------
  literals and variables
---------------------------------------------------------------}

cgLit :: Lit -> [Instr]
cgLit lit
  = case lit of
      LitInt i    -> [PUSHINT i]
      LitFloat d  -> [PUSHFLOAT d]
      LitBytes b  -> [PUSHBYTES b 0]

cgVar :: Env -> Id -> [Instr]
cgVar env x
  = case lookupGlobal x env of
      Nothing     -> [PUSHVAR (varFromId x)]
      Just arity  -> [PUSHCODE (Global x 0 arity)]

{---------------------------------------------------------------
 whnf: returns True when the expression puts a weak head normal form
       value on the stack.
---------------------------------------------------------------}

whnf :: Env -> Expr -> Bool
whnf env expr
  = case expr of
      LetRec _ e    -> whnf env e
      Let _ _ e2    -> whnf env e2
      Eval _ _ e2   -> whnf env e2
      Match _ alts  -> all (whnfAlt env) alts
      Prim x _      -> whnfPrim env x
      Ap {}         -> False
      Con {}        -> True
      Lit {}        -> True
      Note _ e      -> whnf env e

whnfAlt :: Env -> Alt -> Bool
whnfAlt env (Alt _ e) = whnf env e

whnfPrim :: Env -> Id -> Bool
whnfPrim env x
  = case lookupInstr x env of
      Nothing    -> False -- TODO: look at the type of a primitive
      Just instr -> strictResult instr


{---------------------------------------------------------------
  the code generation environment
---------------------------------------------------------------}
data Env  = Env { aritiesMap :: IdMap Arity
                , instrsMap  :: IdMap Instr
                , consMap    :: IdMap (Tag,Arity)
                }

lookupInstr :: Id -> Env -> Maybe Instr
lookupInstr x = lookupMap x . instrsMap

lookupGlobal :: Id -> Env -> Maybe Arity
lookupGlobal x = lookupMap x . aritiesMap

varFromId :: Id -> Var
varFromId x = Var x 0 0

conFromId :: Id -> Arity -> Env -> Instr.Con
conFromId x argcount env
  = let (tag,arity) = tagArityFromId x argcount env
    in  Instr.Con x 0 arity tag

tagArityFromId :: Id -> Arity -> Env -> (Tag, Arity)
tagArityFromId x argcount env
  = case lookupMap x (consMap env) of
      Just (tag,arity)  -> if arity /= argcount
                            then error ("AsmToCode.conFromId: unsaturated constructor " ++ show x)
                            else (tag,arity)
      Nothing           -> error ("AsmToCode.conFromId: undeclared constructor " ++ show x)

-- create an initial environment from the declarations
initialEnv :: AsmModule -> Env
initialEnv asmMod = Env globals instrs cons
  where
    globals = mapFromList [(declName d,getArity d) | d <- moduleDecls asmMod
                                                    , isDeclValue d || isDeclAbstract d || isDeclExtern d ]

    instrs  = mapFromList [(declName d,instrFromEx d) | d <- moduleDecls asmMod
                                                      , isDeclExtern d, externCall d == CallInstr]

    cons    = mapFromList [(declName d,(conTag d,declArity d)) | d <- moduleDecls asmMod
                                                               , isDeclCon d]

    getArity (DeclValue{valueValue=Top args _})  = length args           
    getArity decl                                = declArity decl

    instrFromEx x   = case externName x of
                        Plain s    -> instrFromName s
                        Decorate s -> instrFromName s
                        Ordinal i  -> instrFromOpcode i
