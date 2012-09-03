{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

module Lvm.Core.ToAsm (coreToAsm) where

import Control.Exception (assert)
import Lvm.Common.Id
import Lvm.Common.IdSet
import Lvm.Core.Expr
import Lvm.Core.Utils
import qualified Lvm.Asm.Data as Asm

import Lvm.Core.NoShadow  (coreRename)    -- rename local variables
import Lvm.Core.Saturate  (coreSaturate)  -- saturate constructors, instructions and externs
import Lvm.Core.Normalize (coreNormalize) -- normalize core, ie. atomic arguments and lambda's at let bindings
import Lvm.Core.FreeVar   (coreFreeVar)   -- attach free variable information at let bindings
import Lvm.Core.LetSort   (coreLetSort)   -- find smallest recursive let binding groups
import Lvm.Core.Lift      (coreLift)      -- lambda-lift, ie. make free variables arguments

{---------------------------------------------------------------
  coreToAsm: translate Core expressions into Asm expressions
---------------------------------------------------------------}
coreToAsm :: NameSupply -> CoreModule -> Asm.AsmModule
coreToAsm supply
  = exprToTop 
  . coreLift
  . coreLetSort
  . coreFreeVar
  . coreNormalize supply2
  . coreSaturate supply1
  . coreRename supply0
  where        
    (supply0:supply1:supply2:_) = splitNameSupplies supply

exprToTop :: CoreModule -> Asm.AsmModule
exprToTop m
  = m{ moduleDecls = concatMap (asmDecl (externNames m)) (moduleDecls m) }

{---------------------------------------------------------------
  top-level bindings
---------------------------------------------------------------}

asmDecl :: IdSet -> Decl Expr -> [Decl Asm.Top]
asmDecl prim decl =
   case decl of 
      DeclValue x acc enc expr custom -> 
         let (pars,(lifted,asmexpr)) = asmTop prim expr
         in DeclValue x acc enc (Asm.Top pars asmexpr) custom : concatMap (asmLifted prim (declName decl)) lifted
      _ -> [fmap (error "asmDecl") decl]

asmLifted :: IdSet -> Id -> Bind -> [Decl Asm.Top]
asmLifted prim enc (Bind x expr)
  = let (pars,(lifted,asmexpr)) = asmTop prim expr
    in  DeclValue x (Defined False) (Just enc) (Asm.Top pars asmexpr) []
        : concatMap (asmLifted prim x) lifted

asmTop :: IdSet -> Expr -> ([Id], ([Bind], Asm.Expr))
asmTop prim expr
  = let (pars,expr') = splitParams expr
    in (pars,asmExpr prim expr')

splitParams :: Expr -> ([Id],Expr)
splitParams expr
  = case expr of
      Note _ e  -> splitParams e
      Lam x e   -> let (pars,e') = splitParams e in (x:pars,e')
      _         -> ([],expr)

{---------------------------------------------------------------
  expressions
---------------------------------------------------------------}
asmExpr :: IdSet -> Expr -> ([Bind],Asm.Expr)
asmExpr prim expr
  = case expr of
      Note _ e        -> asmExpr prim e
      Lam _ _         -> error "CoreToAsm.asmExpr: unexpected lambda expression (do 'coreNormalise' first?)"
      Let binds e     -> asmLet prim binds (asmExpr prim e)
      Match x alts    -> let (lifted,asmalts) = asmAlts prim alts
                         in (concat lifted, Asm.Match x asmalts)
      atom            -> let asmatom = asmAtom atom []  -- handles prim ap's too
                         in case asmatom of
                              Asm.Ap x args  | elemSet x prim
                                              -> ([],Asm.Prim x args)
                              _               -> ([],asmatom)

asmAlts :: IdSet -> [Alt] -> ([[Bind]], [Asm.Alt])
asmAlts prim alts
  = unzip (map (asmAlt prim) alts)

asmAlt :: IdSet -> Alt -> ([Bind], Asm.Alt)
asmAlt prim (Alt pat expr)
  = let (lifted,asmexpr) = asmExpr prim expr
    in (lifted, Asm.Alt (asmPat pat) asmexpr)

asmPat :: Pat -> Asm.Pat
asmPat pat
  = case pat of
      PatCon con params -> Asm.PatCon (asmPatCon con) params
      PatLit lit        -> Asm.PatLit (asmLit lit)
      PatDefault        -> Asm.PatVar (idFromString  ".def")

asmPatCon :: Con a -> Asm.Con a
asmPatCon con
  = case con of
      ConId x          -> Asm.ConId x
      ConTag tag arity -> Asm.ConTag tag arity

asmLet :: IdSet -> Binds -> ([Bind], Asm.Expr) -> ([Bind], Asm.Expr)
asmLet prim binds (lifted,asmexpr)
  = case binds of
      NonRec (Bind x expr)
                -> if isAtomic prim expr
                    then (lifted, Asm.Let x (asmAtom expr []) asmexpr)
                    else (Bind x expr:lifted,asmexpr)
      Strict (Bind x rhs)
                -> let (liftedrhs,asmrhs) = asmExpr prim rhs
                   in  (lifted ++ liftedrhs,Asm.Eval x asmrhs asmexpr)
      Rec bs    -> let (lifted',binds') = foldr asmRec (lifted,[]) bs
                   in if null binds'
                       then (lifted',asmexpr)
                       else (lifted',Asm.LetRec binds' asmexpr)
  where
    asmRec bind@(Bind x expr) (lft,bs)
      | isAtomic prim expr = (lft,(x,asmAtom expr []):bs)
      | otherwise          = (bind:lft,bs)


{---------------------------------------------------------------
 atomic expressions & primitive applications
---------------------------------------------------------------}

asmAtom :: Expr -> [Asm.Expr] -> Asm.Expr
asmAtom atom args
  = case atom of
      Note _ e  -> asmAtom e args
      Ap e1 e2  -> asmAtom e1 (asmAtom e2 []:args)
      Var x     -> Asm.Ap x args
      Con con   -> Asm.Con (asmCon con) args
      Lit lit   | null args -> Asm.Lit (asmLit lit)
      Let binds expr
                -> asmAtomBinds binds (asmAtom expr args)
      _ -> error "CoreToAsm.asmAtom: non atomic expression (do 'coreNormalise' first?)"

asmCon :: Con Expr -> Asm.Con Asm.Atom
asmCon con 
  = case con of
      ConId x          -> Asm.ConId x 
      ConTag tag arity  -> assert (simpleTag tag) $ -- "CoreToAsm.asmCon: tag expression too complex (should be integer or (strict) variable"
                           Asm.ConTag (asmAtom tag []) arity
  where
    simpleTag (Lit (LitInt _))  = True
    simpleTag (Var _)           = True
    simpleTag (Note _ e)        = simpleTag e
    simpleTag _                 = False

asmAtomBinds :: Binds -> Asm.Expr -> Asm.Expr
asmAtomBinds binds
  = case binds of
      NonRec (Bind x expr) -> Asm.Let x (asmAtom expr [])
      Rec bs               -> Asm.LetRec [(x,asmAtom expr []) | Bind x expr <- bs]
      _                    -> error "CoreToAsm.asmAtomBinds: strict binding as atomic expression (do 'coreNormalise first?)"

asmLit :: Literal -> Asm.Lit
asmLit lit
  = case lit of
     LitInt i    -> Asm.LitInt i
     LitDouble d -> Asm.LitFloat d
     LitBytes s  -> Asm.LitBytes s

{---------------------------------------------------------------
  is an expression atomic ?
---------------------------------------------------------------}

isAtomic :: IdSet -> Expr -> Bool
isAtomic prim expr
  = case expr of
      Note _ e  -> isAtomic prim e
      Ap e1 e2  -> isAtomic prim e1 && isAtomic prim e2
      Var x     -> not (elemSet x prim)
      Con (ConId _)    -> True
      Con (ConTag t _) -> isAtomic prim t
      Lit _   -> True
      Let binds e
                -> isAtomicBinds prim binds && isAtomic prim e
      _         -> False

isAtomicBinds :: IdSet -> Binds -> Bool
isAtomicBinds prim binds
  = case binds of
      Strict _  -> False
      _         -> all (isAtomic prim) (snd (unzipBinds (listFromBinds binds)))