{-*-----------------------------------------------------------------------
  The Core Assembler.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module AsmPretty( asmPretty )  where

import PPrint
import Byte         ( stringFromBytes )
import Id           ( stringFromId )
import Asm
import ModulePretty ( modulePretty )

{---------------------------------------------------------------
  pretty print Asm expressions
---------------------------------------------------------------}
asmPretty :: AsmModule -> Doc
asmPretty mod
  = modulePretty ppTop mod

ppTop (Top args expr)
  = nest 2 (text "\\" <> hsep (map ppId args) <+> text "->" <$> ppExpr expr)

{---------------------------------------------------------------
  expressions
---------------------------------------------------------------}
ppExpr expr
  = case expr of
      Let id atom e   -> (text "let" <+> ppBind (id,atom)) <$> nest 3 (text "in" <+> ppExpr e)
      LetRec binds e  -> nest 7 (text "letrec" <+> vcat (map ppBind binds)) <$> nest 3 (text "in" <+> ppExpr e)
      Eval id e e'    -> align $ hang 7 (text "let!" <+> ppId id <+> text "=" </> ppExpr e) <$> nest 3 (text "in" <+> ppExpr e')
      Match id alts   -> nest 2 (text "match" <+> ppId id <+> text "with" <$> vcat (map ppAlt alts))
      Prim id args    -> text "prim" <> squares (ppId id) <+> hsep (map ppArg args)
      Atom atom       -> ppAtom atom

ppBind (id,atom)
  = ppId id <+> text "=" <+> ppAtom atom


{---------------------------------------------------------------
  alternatives
---------------------------------------------------------------}
ppAlt (Alt pat expr)
  = hang 2 (ppPat pat <+> text "->" </> ppExpr expr)

ppPat pat
  = case pat of
      PatCon id params -> ppId id <+> hsep (map ppId params)
      PatVar id        -> ppId id
      PatLit lit       -> ppLit lit

{---------------------------------------------------------------
  atomic expressions
---------------------------------------------------------------}
ppAtom atom
  = ppAtomEx id atom

ppArg atom
  = ppAtomEx parens atom

ppAtomEx :: (Doc -> Doc) -> Atom -> Doc
ppAtomEx pars atom
  = case atom of
      Ap id args    | null args -> ppId id
                    | otherwise -> pars $ ppId id <+> hsep (map ppArg args)
      Con id args   | null args -> ppId id
                    | otherwise -> pars $ ppId id <+> hsep (map ppArg args)
      Lit lit       -> ppLit lit

{---------------------------------------------------------------
  literals and variables
---------------------------------------------------------------}
ppLit lit
  = case lit of
      LitInt i      -> pretty i
      LitFloat d    -> pretty d
      LitBytes b    -> dquotes (string (stringFromBytes b))

ppId id
  = text (stringFromId id)

commaBraces doc
  = encloseSep lbrace rbrace comma doc
