{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module CorePretty ( corePretty ) where

import PPrint
import Byte         ( stringFromBytes )
import Id           ( Id, stringFromId )
import IdSet        ( listFromSet )
import Core
import ModulePretty ( modulePretty )
----------------------------------------------------------------
--
----------------------------------------------------------------
corePretty :: CoreModule -> Doc
corePretty  mod
  = modulePretty (ppExpr 0) mod

----------------------------------------------------------------
--
----------------------------------------------------------------
ppExpr :: Int -> Expr -> Doc
ppExpr  p expr
  = case expr of
      Case x b as -> prec 0 $ (hang 2 (text "case" <+> ppExpr  0 x <+> text "of" <+> ppId  b <$> ppAlts  as))
      Let bs x  -> prec 0 $ align (nest 4 (text "let" <+> ppBinds  bs) <$> text "in" <+> ppExpr  0 x)
      Lam id x  -> prec 0 $ text "\\" <> ppId  id <+> ppLams "->" (</>)  x
      Ap e1 e2  -> prec 9 $ ppExpr  9 e1 <+> ppExpr  10 e2
      Var id    -> ppId  id
      Con tag   -> ppId  tag
      Lit lit   -> ppLit lit
      Note (FreeVar fv) e
                -> align (semiBraces (map (ppId ) (listFromSet fv))
                         <$> ppExpr  p e)
      Note n e  -> ppExpr  p e
      other     -> text "<unknown>"
  where
    prec p'  | p' >= p   = id
             | otherwise = parens

----------------------------------------------------------------
--
----------------------------------------------------------------
ppLams arrow next  x
  = case x of
      Lam id x -> ppId  id <+> ppLams arrow next  x
      other    -> text arrow `next` ppExpr  0 x


ppBinds  binds
  = vcat (map (ppBind ) (listFromBinds binds))

ppBind  (Bind id expr)
  = nest 2 (ppId  id <+> ppLams "=" (<+>)  expr)



ppAlts  alts
  = vcat (map (ppAlt ) alts)

ppAlt  (Alt pat expr)
  = nest 2 (ppPat  pat <+> text "->" </> ppExpr  0 expr)

----------------------------------------------------------------
--
----------------------------------------------------------------
ppPat  pat
  = case pat of
      PatCon id ids -> hsep (ppId  id  : map (ppId ) ids)
      PatLit lit  -> ppLit lit
      PatDefault  -> text "_"


ppLit lit
  = case lit of
      LitInt i    -> pretty i
      LitDouble d -> pretty d
      LitBytes s  -> dquotes (string (stringFromBytes s))

ppId :: Id -> Doc
ppId id
  = text (stringFromId id)
