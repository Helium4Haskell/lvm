{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module InstrRewrite( instrRewrite )  where

import Standard ( assert, trace )
import Id       ( Id, stringFromId )
import Instr    ( Instr(..), Alt(..), Pat(..), Var(..)
                , idFromVar, offsetFromVar, depthFromVar
                , arityFromGlobal, arityFromCon
                , instrHasStrictResult
                )
import InstrPretty

showInstr instr
  = showInstrs [instr]

showInstrs instrs
  = show (instrPretty instrs)

traceInstrs instrs 
  = trace ("trace:\n" ++ showInstrs instrs ++ "\n\n") instrs

{---------------------------------------------------------------
  rewrite rules
---------------------------------------------------------------}
instrRewrite :: [Instr] -> [Instr]
instrRewrite instrs
  = peepholes $ rewrites $ rewrites $ dummies $ rewrites $ rewrites $ instrs

rewrites instrs
  = case instrs of
  
      PUSHVAR (Var id 0 depth) : is
        | not (useds [depth] is)
        -> rewrites (squeeze depth 1 is)

      PUSHVAR (Var id0 1 d0) : PUSHVAR (Var id1 1 d1) : is
        | not (useds [d0,d1] is)
        -> rewrites (squeeze d1 2 is)

      PUSHVAR (Var id0 2 d0) : PUSHVAR (Var id1 2 d1) : PUSHVAR (Var id2 2 d2) : is
        | not (useds [d0,d1,d2] is)
        -> rewrites (squeeze d2 3 is)
   
{-   
      PUSHVAR (Var id 0 depth) : SLIDE 1 m d : is
        | m >= 1
        -> rewrites (SLIDE 1 (m-1) (d-1) : is)

      PUSHVAR (Var id0 1 d0) : PUSHVAR (Var id1 1 d1) : SLIDE 2 m d : is
        | m >= 2
        -> rewrites (SLIDE 2 (m-2) (d-2) : is)

      PUSHVAR (Var id0 2 d0) : PUSHVAR (Var id1 2 d1) : PUSHVAR (Var id2 2 d2) : SLIDE 3 m d : is
        | m >= 3
        -> rewrites (SLIDE 3 (m-3) (d-3) : is)
 -}  
      PUSHCODE f : is
        -> rewritePushCode f (rewrites is)

      NEWAP i : SLIDE n m d: ENTER : is
        -> SLIDE (n+i-1) m (d+i-1): ENTER : rewrites is

      NEWNAP i : SLIDE n m d: ENTER : is
        -> SLIDE (n+i-1) m (d+i-1): ENTER : rewrites is

      CALL global : SLIDE 1 m d: ENTER : is
        -> SLIDE arity m (d+arity-1): CALL global : ENTER : rewrites is
        where
          arity = arityFromGlobal global

      NEWCON con : SLIDE 1 m d: ENTER : is
        -> SLIDE n m (d+n-1): RETURNCON con : rewrites is
        where n = arityFromCon con

      PUSHINT i : SLIDE 1 m d: ENTER : is
        -> SLIDE 0 m (d-1): RETURNINT i : rewrites is

      instr : SLIDE 1 m d: ENTER : is
        | instrHasStrictResult instr
        -> instr : SLIDE 1 m d: RETURN : is

      EVAL d is' : is
        -> rewriteEval d (rewrites is') is

      SLIDE n0 m0 d0 : SLIDE n1 m1 d1 : is
        | n1 <= n0  -> rewrites (SLIDE n1 (m0+m1-(n0-n1)) d0 : is)

      -- essential rewrites
      MATCHCON alts : is
        -> [rewriteMatch MATCHCON alts is]

      SWITCHCON alts : is
        -> [rewriteMatch SWITCHCON alts is]

      MATCHINT alts : is
        -> [rewriteMatch MATCHINT alts is]

      instr:instrs  -> instr:rewrites instrs
      []            -> []


rewriteMatch match alts is
  = match (map (rewriteAlt is) alts)
{-
rewriteMatchInstrs is
  = case rewrites is of
      [SLIDE n m d, ENTER] -> [SLIDE n m d, ENTER]
      []    -> []
      other -> error ("InstrRewrite.rewriteMatch: unable to rewrite due to illegal instructions: " ++ showInstrs is)
-}
rewriteAlt instrs (Alt pat [])
  = Alt pat []

rewriteAlt instrs (Alt pat is)
  = Alt pat (rewrites (is ++ instrs))


-- rewrite PUSHCODE
rewritePushCode f instrs
  = case instrs of
      NEWAP n : is
        | arity >= n -> PUSHCODE f : NEWNAP n : is

      PACKAP var n : is
        | arity >= n -> PUSHCODE f : PACKNAP var n : is

      SLIDE n m d: ENTER : is
        | arity == (n-1) && arity /= 0  -> SLIDE (n-1) m (d-1): ENTERCODE f : is

      other
        -> PUSHCODE f  : instrs
  where
    arity = arityFromGlobal f

-- rewrite EVAL
rewriteEval d evalis is
  =  case span isUSE (reverse evalis) of
{-      (uses, RETURN : SLIDE 1 0 depth : instr : rest)
        -> squeeze depth 3 (reverse rest ++ [instr]) ++ rewrites (is ++ uses)
-}
      (uses, RETURN : rest)
        -> squeeze d 3 (reverse rest) ++ rewrites (is ++ uses)
      other
        -> 
           case evalis of
             (PUSHVAR (Var id ofs d) : SLIDE 1 m e : ENTER : uses)
                | all isUSE uses
                -> rewrites ([EVALVAR (Var id (ofs-3) d)] ++ is ++ uses)
             (PUSHVAR (Var id ofs d) : ENTER : uses)
                | all isUSE uses
                -> rewrites ([EVALVAR (Var id (ofs-3) d)] ++ is ++ uses)
             other
                -> [EVAL d evalis] ++ rewrites is
  where
    isUSE (USE id)  = True
    isUSE _         = False


{---------------------------------------------------------------
  peephole optimization
---------------------------------------------------------------}
dummies instrs
  = case instrs of
      -- structured
      EVAL d is' : is           -> EVAL d (dummies is') : dummies is
      MATCHCON alts : is        -> MATCHCON (map dummiesAlt alts)  : dummies is
      SWITCHCON alts : is       -> SWITCHCON (map dummiesAlt alts)  : dummies is
      MATCHINT alts : is        -> MATCHINT (map dummiesAlt alts)  : dummies is

      -- dummy
      NEWAP 1 : is              -> dummies is
      NEWNAP 1 : is             -> dummies is

      -- slide
      SLIDE n 0 d : is          -> dummies is
      SLIDE 1 m d : RETURN : is -> dummies (RETURN : is)
      SLIDE n m d : RETURNCON con : is
                                | arityFromCon con == n -> dummies (RETURNCON con : is)
      SLIDE 0 m d : RETURNINT i : is
                                -> dummies (RETURNINT i : is)

      -- defaults
      instr:is                  -> instr : dummies is
      []                        -> []

dummiesAlt (Alt pat is)
  = Alt pat (dummies is)


peepholes instrs
  = case instrs of
      -- structured
      EVAL d is' : is           -> EVAL d (peepholes is') : peepholes is
      MATCHCON alts : is        -> MATCHCON (map peepholeAlt alts)  : peepholes is
      SWITCHCON alts : is       -> SWITCHCON (map peepholeAlt alts)  : peepholes is
      MATCHINT alts : is        -> MATCHINT (map peepholeAlt alts)  : peepholes is

      PUSHVAR v : PUSHVAR w : is
                                -> PUSHVARS2 v w : peepholes is

      -- default
      instr:is                  -> peephole instr : peepholes is
      []                        -> []

peepholeAlt (Alt pat is)
  = Alt pat (peepholes is)

peephole instr
  = case instr of
      PUSHVAR var   -> case offsetFromVar var of
                         0     -> PUSHVAR0
                         1     -> PUSHVAR1
                         2     -> PUSHVAR2
                         3     -> PUSHVAR3
                         4     -> PUSHVAR4
                         other -> instr

      NEWAP n       -> case n of
                         2     -> NEWAP2
                         3     -> NEWAP3
                         4     -> NEWAP4
                         other -> instr

      NEWNAP n      -> case n of
                         2     -> NEWNAP2
                         3     -> NEWNAP3
                         4     -> NEWNAP4
                         other -> instr

      NEWCON con    -> case arityFromCon con of
                         0     -> NEWCON0 con
                         1     -> NEWCON1 con
                         2     -> NEWCON2 con
                         3     -> NEWCON3 con
                         other -> instr

      RETURNCON con -> case arityFromCon con of
                         0     -> RETURNCON0 con
                         other -> instr

      other         -> instr



{---------------------------------------------------------------
  squeeze: adjust all variable references to the stack
          used to squeeze out [n] slots from the stack at [depth]
          all offsets above depth should be adjusted by [delta = -n]
---------------------------------------------------------------}
squeeze depth n instrs
  = adjusts depth (-n) instrs

adjusts depth delta instrs
  = map (adjust depth delta) instrs

adjust depth delta instr
  = case instr of
      EVAL  d is          | (depth > d-3 && depth+delta < d) -> error ("CodeRewrite.adjust.Eval: invalid depth")
                          | depth < d  -> EVAL (d+delta) (adjusts depth delta is)
                          | otherwise  -> EVAL d (adjusts depth delta is)
      
      SLIDE n m d         | (depth > d-n && depth+delta < d) -> error ("CodeRewrite.adjust.Slide: invalid depth " ++ show delta ++ " " ++ show depth ++ " " ++ showInstr instr)
                          | (depth > d-n-m && depth <= d-n)   -> SLIDE n (m - min (depth - (d-n-m)) (-delta))  (d+delta)
                          | depth <= d-n-m                    -> SLIDE n m  (d+delta)
                          | otherwise                         -> SLIDE n m d


      MATCHCON alts       -> MATCHCON (map adjustAlt alts)
      MATCHINT alts       -> MATCHINT (map adjustAlt alts)

      PUSHVAR var         -> PUSHVAR (adjustVar var)
      STUB var            -> STUB (adjustVar var)
      PACKAP var n        -> PACKAP  (adjustVar var) n
      PACKNAP var n       -> PACKNAP (adjustVar var) n
      PACKCON con var     -> PACKCON con (adjustVar var)
      EVALVAR var         -> EVALVAR (adjustVar var)
{-
      SLIDE n m d         -> let base = d-m
                             in if (base == depth)
                                 then error "CodeRewrite.adjust: invalid depth"
                                 else 
                                  if (base < depth)
                                       then SLIDE n (m+delta) (d+delta)
                                       else SLIDE n m d
-}
      other               -> instr

  where
    adjustVar (Var id ofs d)  | d == depth  = error ("CodeRewrite.adjustVar: invalid depth at " ++ show (stringFromId id))
                              | depth < d   = Var id ofs (d+delta)
                              | otherwise   = Var id (ofs+delta) d

    adjustAlt (Alt pat is)    = Alt pat (adjusts depth delta is)


{---------------------------------------------------------------
  useds: is an identifier still used?
---------------------------------------------------------------}
{-
useds ids instrs
  = any (used ids) instrs

used ids instr
  = case instr of
      EVAL d is'        -> useds ids is'
      MATCHCON alts     -> any (usedAlt ids) alts
      MATCHINT alts     -> any (usedAlt ids) alts
      USE id            -> elem id ids
      STUB var          -> elem (idFromVar var) ids
      PUSHVAR var       -> elem (idFromVar var) ids
      PACKAP var n      -> elem (idFromVar var) ids
      PACKCON con var   -> elem (idFromVar var) ids
      other             -> False

usedAlt ids (Alt pat is)
  = useds ids is
-}

useds ds instrs
  = any (\d -> used d instrs) ds

used d instrs
  = any (use d) instrs

usedAlt d (Alt pat is)
  = used d is

use d instr
  = case instr of
      EVAL d' is'       -> used d is'
      MATCHCON alts     -> any (usedAlt d) alts
      MATCHINT alts     -> any (usedAlt d) alts
      STUB var          -> (depthFromVar var == d)
      PUSHVAR var       -> (depthFromVar var == d)
      PACKAP var n      -> (depthFromVar var == d)
      PACKNAP var n     -> (depthFromVar var == d)
      PACKCON con var   -> (depthFromVar var == d)
      EVALVAR var       -> (depthFromVar var == d)
      SLIDE n m d'      -> (d'-n < d) && (d <= d')
      other             -> False
