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
ppModule ppValue (Module moduleName major minor values imports constructors externs customs)
  =  text "module" <+> ppId moduleName <+> text "where"
 <$> vcat (concat
     [map ppDCustom (listFromMap customs)
     ,map ppDExtern (listFromMap externs)
     ,map ppDImport (listFromMap imports)
     ,map ppDCon    (listFromMap constructors)
     ,map (ppDValue ppValue) values
     ])
 <$> empty

ppDecls pp decls
  = vcat (map (\(id,decl) -> pp id decl <> line) (listFromMap decls))



ppDValue ppValue (id,DValue{ valueValue = value })
  = nest 2 (ppId  id <+> text "=" <$> ppValue value)

ppDImport (id,DImport{ importArity=arity})
  = nest 2 (ppId  id <+> text "= <abstract arity=" <+> pretty arity <> text ">")


ppDCon (id,DCon{ conTag=tag, conArity=arity })
  =  text "con" <+> ppId  id
  <+> text "= <tag=" <+> pretty tag <+> text ", arity=" <+> pretty arity <> text ">"

ppDExtern (id,DExtern{})
  = text "extern" <+> ppId  id

ppDCustom (id,DCustom{})
  = text "custom" <+> ppId id


ppId :: Id -> Doc
ppId id
  = text (stringFromId id)
