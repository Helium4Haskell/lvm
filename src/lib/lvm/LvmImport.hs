{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module LvmImport( lvmImport ) where


import Monad    ( foldM )
import Standard ( foldlStrict )
import Id       ( Id, stringFromId )
import IdMap    ( IdMap, emptyMap, insertMap, elemMap, updateMap, listFromMap, lookupMap, findMap  )

import Module   
import Lvm
import LvmRead  ( lvmReadFile )

import ModulePretty
import InstrPretty
import PPrint

{--------------------------------------------------------------
  lvmImport: replace all import declarations with
  abstract declarations or constructors/externs
  TODO: this doesn't respect the namespaces
--------------------------------------------------------------}
lvmImport :: (Id -> IO FilePath) -> (Module v) -> IO (Module v)
lvmImport findModule mod
  = do{ mods <- lvmImportModules findModule mod
      ; let mods' = lvmResolveImports mods
            mod'  = findMap (moduleName mod) mods'
      ; return mod'{ moduleDecls = filter (not . isDeclImport) (moduleDecls mod') }
      }

{--------------------------------------------------------------
  lvmImportModules: 
    recursively read all imported modules
--------------------------------------------------------------}
lvmImportModules :: (Id -> IO FilePath) -> (Module v) -> IO (IdMap (Module v))
lvmImportModules findModule mod
  = readModuleImports findModule emptyMap (moduleName mod) mod
    
readModuleImports :: (Id -> IO FilePath) -> IdMap (Module v) -> Id -> (Module v) -> IO (IdMap (Module v))
readModuleImports findModule loaded id mod
  = foldM (readModule findModule) (insertMap id mod loaded) (imported mod)
  
readModule :: (Id -> IO FilePath) -> IdMap (Module v) -> Id -> IO (IdMap (Module v))
readModule findModule loaded id  
  | elemMap id loaded  = return loaded
  | otherwise          = do{ fname <- findModule id                        
                           ; mod   <- lvmReadFile fname
                           ; readModuleImports findModule loaded id mod
                           }

imported mod
  = [importModule (declAccess d) | d <- moduleDecls mod, isDeclImport d]
    
{---------------------------------------------------------------
lvmResolveImports:
  replaces all "DImport" declarations with the real
  declaration (except the access is Import). This is always
  needed for all modules.
---------------------------------------------------------------}
lvmResolveImports :: IdMap (Module v) -> IdMap (Module v)
lvmResolveImports mods
  = foldlStrict resolveImports mods (listFromMap mods)

resolveImports :: IdMap (Module v) -> (Id,Module v) -> IdMap (Module v)
resolveImports loaded (modid,mod)
  = foldlStrict (resolveImport [] modid) loaded (filter isDeclImport (moduleDecls mod))

resolveImport :: [Id] -> Id -> IdMap (Module v) -> Decl v -> IdMap (Module v)
resolveImport visited modid loaded x@(DeclImport id access@(Imported public imodid impid kind major minor) customs)
  | elem modid visited = error ("LvmImport.resolveImport: circular import chain: " ++ stringFromId imodid ++ "." ++ stringFromId impid)
  | otherwise = 
    let mod = findMap modid loaded in 
    case lookupMap imodid loaded of
      Nothing   -> error ("LvmImport.resolveImport: import module is not loaded: " ++ stringFromId imodid)
      Just imod -> case lookupDecl impid kind (moduleDecls imod) of
                     []   -> notfound imodid impid
                     ds   -> case filter (not . isDeclImport) ds of
                               []  -> case filter isDeclImport ds of
                                        []  -> notfound imodid impid
                                        [d] -> let loaded' = resolveImport (modid:visited) imodid loaded d
                                               in resolveImport (imodid:visited) modid loaded' x
                                        ds  -> ambigious imodid impid
                               [d] -> update mod{ moduleDecls = d{declName=id,declAccess = access} : (moduleDecls mod)}
                               ds  -> ambigious imodid impid

  where
    lookupDecl impid kind decls
      = [d | d <- decls, declName d==impid && hasDeclKind kind d]

    update mod'
      = updateMap modid mod' loaded
        
    notfound imodid impid
      = error ("LvmImport.resolveImport: unresolved identifier: " ++ stringFromId imodid ++ "." ++ stringFromId impid)

    ambigious imodid impid
      = error ("LvmImport.resolveImport: ambigious import record: " ++ stringFromId imodid ++ "." ++ stringFromId impid)      