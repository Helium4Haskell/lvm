{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module ModulePretty ( modulePretty ) where

import PPrint
import Id       ( Id, stringFromId )
import IdMap    ( listFromMap )
import Module

----------------------------------------------------------------
--
----------------------------------------------------------------
modulePretty :: (v -> Doc) -> Module v -> Doc
modulePretty ppValue mod
  = ppModule ppValue mod

ppModule ::  (v -> Doc) -> Module v -> Doc
ppModule ppValue (Module moduleName major minor decls)
  =  text "module" <+> ppId moduleName <+> text "where"
 <$> vcat (map (\decl -> ppDecl ppValue decl <> line) decls)
 <$> empty

ppDecl ppValue decl
  = case decl of
      DeclValue{declName=id,declAccess=access,valueValue=value}   
        -> nest 2 (ppId id <+> text "=" <+> ppAccess access <$> ppValue value)
      DeclCon{declName=id,conTag=tag,declArity=arity }
        -> text "con" <+> ppId  id <+> text "= <tag=" <+> pretty tag <+> text ", arity=" <+> pretty arity <> text ">"
      DeclExtern{declName=id}
        -> text "extern" <+> ppId  id
      DeclCustom{declName=id}
        -> text "custom" <+> ppId id
      DeclImport{declName=id,declAccess=access}
        -> text "import" <+> ppId id <+> text "=" <+> ppAccess access
      DeclAbstract{declName=id,declAccess=access}
        -> text "abstract" <+> ppId id <+> text "=" <+> ppAccess access
      other
        -> text "<unknown declaration>"

ppId :: Id -> Doc
ppId id
  = text (stringFromId id)

ppAccess acc
  = case acc of
      Defined public 
        -> ppPublic public
      Imported public modid impid impkind major minor
        -> ppPublic public <+> ppId modid <> char '.' <> ppId impid

ppPublic public
  = if public then text "public" else text "private"