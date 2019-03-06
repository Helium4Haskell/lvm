--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file 
-- is distributed under the terms of the BSD3 License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id$

module Lvm.Core.Expr 
   ( CoreModule, CoreDecl, Expr(..), Binds(..), Bind(..)
   , Alts, Alt(..), Pat(..), Literal(..), Con(..), Variable(..)
   ) where

import Prelude hiding ((<$>))
import Lvm.Common.Byte
import Lvm.Common.Id
import Lvm.Core.Module
import Lvm.Core.PrettyId
import Lvm.Core.Type
import Text.PrettyPrint.Leijen

----------------------------------------------------------------
-- Modules
----------------------------------------------------------------
type CoreModule = Module Expr
type CoreDecl   = Decl Expr

----------------------------------------------------------------
-- Core expressions:
----------------------------------------------------------------
data Expr       = Let       !Binds Expr       
                | Match     !Id Alts
                | Ap        Expr Expr
                | Lam       !Variable Expr
                | Forall    !Id !Kind !Expr
                | Con       !(Con Expr)
                | Var       !Id
                | Lit       !Literal

data Variable = Variable { variableName :: !Id, variableType :: !Type }
data Binds      = Rec       ![Bind]
                | Strict    !Bind
                | NonRec    !Bind

data Bind       = Bind      !Variable !Expr

type Alts       = [Alt]
data Alt        = Alt       !Pat Expr

data Pat        = PatCon    !(Con Tag) ![Id]
                | PatLit    !Literal
                | PatDefault

data Literal    = LitInt    !Int
                | LitDouble !Double
                | LitBytes  !Bytes

data Con tag    = ConId  !Id
                | ConTag tag !Arity

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

instance Pretty Expr where
   pretty = ppExpr 0

ppExpr :: Int -> Expr -> Doc
ppExpr p expr
  = case expr of
      Match x as  -> prec 0 $ align (text "match" <+> ppVarId x <+> text "with" <+> text "{" <$> indent 2 (pretty as)
                              <+> text "}")
      Let bs x    -> prec 0 $ align (ppLetBinds bs (text "in" <+> ppExpr 0 x))
      Lam (Variable x t) e -> prec 0 $ text "\\" <> ppVarId x <> text ": " <> pretty t <+> text "->" <+> pretty e
      Forall tvar k e -> prec 0 $ text "forall" <+> ppVarId tvar <> text ": " <> pretty k <> text "." <+> pretty e
      Ap e1 e2    -> prec 9 $ ppExpr  9 e1 <+> ppExpr  10 e2
      Var x       -> ppVarId x
      Con con     -> pretty con
      Lit lit     -> pretty lit
  where
    prec p'  | p' >= p   = id
             | otherwise = parens

instance Pretty a => Pretty (Con a) where
   pretty con =
      case con of
         ConId x          -> ppConId x
         ConTag tag arity -> parens (char '@' <> pretty tag <> comma <> pretty arity)
 
----------------------------------------------------------------
--
----------------------------------------------------------------

ppLetBinds :: Binds -> Doc -> Doc
ppLetBinds binds doc
  = case binds of
      NonRec bind -> nest 4 (text "let" <+> pretty bind) <$> doc
      Strict bind -> nest 5 (text "let!" <+> pretty bind) <$> doc
      -- Rec recs    -> nest 8 (text "let rec" <+> pretty recs) <$> doc
      Rec recs    -> nest 4 (text "let" <+> pretty recs) <$> doc -- let rec not parsable

instance Pretty Bind where
   pretty (Bind (Variable x t) expr) =
      nest 2 (ppId  x <> text ": " <+> pretty t <> text " = " <+> pretty expr <> semi)
   prettyList = vcat . map pretty

instance Pretty Alt where
   pretty (Alt pat expr) =
      nest 4 (pretty pat <+> text "->" </> ppExpr 0 expr <> semi)
   prettyList = vcat . map pretty

----------------------------------------------------------------
--
----------------------------------------------------------------

instance Pretty Pat where 
   pretty pat = 
      case pat of
         PatCon con ids -> hsep (pretty con : map ppVarId ids)
         PatLit lit  -> pretty lit
         PatDefault  -> text "_"

instance Pretty Literal where 
   pretty lit = 
      case lit of
         LitInt i    -> pretty i
         LitDouble d -> pretty d
         LitBytes s  -> text (show (stringFromBytes s))
