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
import LvmRead  ( lvmReadFile )
import ModulePretty
import InstrPretty
import PPrint

{--------------------------------------------------------------
  lvmImport: replace all import declarations with
  abstract declarations or constructors/externs
  TODO: this doesn't respect the namespaces
--------------------------------------------------------------}
lvmImport :: (Id -> IO FilePath) -> Module v -> IO (Module v)
lvmImport findModule mod
  = do{ mods <- lvmImportModules findModule mod
      ; let mods' = lvmResolveImports mods
      ; return (findMap (moduleName mod) mods')
      }

{--------------------------------------------------------------
  lvmImportModules: 
    recursively read all imported modules
--------------------------------------------------------------}
lvmImportModules :: (Id -> IO FilePath) -> Module v -> IO (IdMap (Module v))
lvmImportModules findModule mod
  = readModuleImports findModule emptyMap (moduleName mod) mod
    
readModuleImports findModule loaded id mod
  = foldM (readModule findModule) (insertMap id mod loaded) (imported mod)
  
readModule findModule loaded id  
  | elemMap id loaded  = return loaded
  | otherwise          = do{ fname <- findModule id                        
                           ; mod   <- lvmReadFile fname
                           ; readModuleImports findModule loaded id mod
                           }

imported mod
  = map (importModule . importAccess . snd) (imports mod)
    
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
  = foldlStrict (resolveImport [] modid) loaded (imports mod)

resolveImport :: [Id] -> Id -> IdMap (Module v) -> (Id,DImport) -> IdMap (Module v)
resolveImport visited modid loaded x@(id,DImport access@(Import public imodid impid major minor) kind)
  | elem modid visited = error ("LvmImport.resolveImport: circular import chain: " ++ stringFromId imodid ++ "." ++ stringFromId impid)
  | otherwise = 
    let mod = findMap modid loaded in 
    case lookupMap imodid loaded of
      Nothing   -> error ("LvmImport.resolveImport: import module is not loaded: " ++ stringFromId imodid)
      Just imod -> case kind of
                     3 -> case lookupMap impid (abstracts imod) of
                            Nothing  -> notfound imod
                            Just abs -> update mod{ abstracts = insertMap id (abs{ abstractAccess = access }) (abstracts mod) }
                     4 -> case lookupMap impid (constructors imod) of
                            Nothing  -> notfound imod
                            Just con -> update mod{ constructors = insertMap id (con{ conAccess = access }) (constructors mod) }
                     7 -> case lookupMap impid (externs imod) of
                            Nothing  -> notfound imod
                            Just ext -> update mod{ externs = insertMap id (ext{ externAccess = access }) (externs mod) }
                     other
                       -> error "LvmImport.resolveImport: invalid declaration kind"
  where
    notfound imod 
      = case lookup impid (imports imod) of
          Nothing  -> error ("LvmImport.resolveImport: unresolved identifier: " ++ stringFromId imodid ++ "." ++ stringFromId impid)
          Just imp -> let loaded' = resolveImport (modid:visited) imodid loaded (impid,imp)
                      in resolveImport (imodid:visited) modid loaded' x

    update mod'
      = updateMap modid mod' loaded
        
