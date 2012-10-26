--------------------------------------------------------------------------------
-- Copyright 2001-2012, Bastiaan Heeren, Jurriaan Hage, Daan Leijen. This file 
-- is distributed under the terms of the GNU General Public License. For more 
-- information, see the file "LICENSE.txt", which is included in the 
-- distribution.
--------------------------------------------------------------------------------
--  $Id$

module Lvm.Import (lvmImport, lvmImportDecls) where

import Control.Monad
import Data.List 
import Lvm.Common.Id
import Lvm.Common.IdMap
import Lvm.Data
import Lvm.Read  (lvmReadFile)
import qualified Lvm.Core.Module as Module

{--------------------------------------------------------------
  lvmImport: replace all import declarations with
  abstract declarations or constructors/externs/customs
--------------------------------------------------------------}
lvmImport :: (Id -> IO FilePath) -> Module v -> IO (Module v)
lvmImport findModule m
  = do{ mods <- lvmImportModules findModule m
      ; let mods0 = lvmExpandModule mods (moduleName m) 
            mods1 = lvmResolveImports mods0
            mod1  = findMap (moduleName m) mods1
      ; return mod1{ moduleDecls = filter (not . isDeclImport) (moduleDecls mod1) }
      }

lvmImportDecls :: (Id -> IO FilePath) -> [Decl v] -> IO [[Decl v]]
lvmImportDecls findModule = mapM $ \importDecl -> do
   m <- lvmImport findModule
       Module.Module
           { Module.moduleName     = idFromString "Main"
           , Module.moduleMajorVer = 0
           , Module.moduleMinorVer = 0
           , Module.moduleDecls    = [importDecl]
           }
   return (moduleDecls m)

{--------------------------------------------------------------
  lvmImportModules: 
    recursively read all imported modules
--------------------------------------------------------------}
lvmImportModules :: (Id -> IO FilePath) -> Module v -> IO (IdMap (Module v))
lvmImportModules findModule m
  = readModuleImports findModule emptyMap (moduleName m) m
    
readModuleImports :: (Id -> IO FilePath) -> IdMap (Module v) -> Id -> Module v -> IO (IdMap (Module v))
readModuleImports findModule loaded x m
  = foldM (readModule findModule) (insertMap x m loaded) (imported m)

readModule :: (Id -> IO FilePath) -> IdMap (Module v) -> Id -> IO (IdMap (Module v))
readModule findModule loaded x
  | elemMap x loaded  = return loaded
  | otherwise         = do{ fname <- findModule x                        
                          ; m     <- lvmReadFile fname
                          ; readModuleImports findModule loaded x (filterPublic m)
                          }

imported :: Module v -> [Id]
imported m = [importModule (declAccess d) | d <- moduleDecls m, isDeclImport d]

{--------------------------------------------------------------
  lvmExpandModule loaded modname: 
    expand Module import declarations of [modname] 
    into declarations for all items exported from that module.
--------------------------------------------------------------}
lvmExpandModule :: IdMap (Module v) -> Id -> IdMap (Module v)
lvmExpandModule loaded modname
  = mapMap expand loaded
  where
    expand m | moduleName m == modname  = expandModule loaded m
             | otherwise                = m

expandModule :: IdMap (Module v) -> Module v -> Module v
expandModule loaded m
  = m{ moduleDecls = concatMap (expandDecl loaded (moduleName m)) (moduleDecls m) }

expandDecl :: IdMap (Module a) -> Id -> Decl a -> [Decl a]
expandDecl loaded modname DeclImport{declAccess = access@(Imported{importModule = imodname,importKind = DeclKindModule})}
  = case lookupMap imodname loaded of
      Nothing   -> error ("LvmImport.expandDecl: import module is not loaded: " ++ stringFromId modname)
      Just imod | moduleName imod == modname 
                -> error ("LvmImport.expandDecl: module imports itself: " ++ stringFromId modname)
      Just imod -> map importDecl (moduleDecls imod)
  where
    importDecl decl
      = decl{ declAccess = access{importName = declName decl, importKind = declKindFromDecl decl} }

expandDecl _ _ decl = [decl]

{---------------------------------------------------------------
lvmResolveImports:
  replaces all "DImport" declarations with the real
  declaration (except the access is Import). This is always
  needed for all modules.
---------------------------------------------------------------}
lvmResolveImports :: IdMap (Module v) -> IdMap (Module v)
lvmResolveImports mods = foldl' resolveImports mods (listFromMap mods)

resolveImports :: IdMap (Module v) -> (Id,Module v) -> IdMap (Module v)
resolveImports loaded (modid, m)
  = foldl' (resolveImport [] modid) loaded (filter isDeclImport (moduleDecls m))

resolveImport :: [Id] -> Id -> IdMap (Module v) -> Decl v -> IdMap (Module v)
resolveImport visited modid loaded imp@(DeclImport x access@(Imported _ imodid impid kind _ _) _)
  | modid `elem` visited = error ("LvmImport.resolveImport: circular import chain: " ++ stringFromId imodid ++ "." ++ stringFromId impid)
  | otherwise = 
    let m = findMap modid loaded in 
    case lookupMap imodid loaded of
      Nothing   -> error ("LvmImport.resolveImport: import module is not loaded: " ++ stringFromId imodid)
      Just imod -> case lookupDecl impid kind (moduleDecls imod) of
                     []   -> notfound imodid impid
                     ds   -> case filter (not . isDeclImport) ds of
                               []  -> case filter isDeclImport ds of
                                        []  -> notfound imodid impid
                                        [d] -> let loaded' = resolveImport (modid:visited) imodid loaded d
                                               in resolveImport (imodid:visited) modid loaded' imp
                                        _   -> ambigious imodid impid
                               [d] -> update m { moduleDecls = d{declName=x,declAccess = access} : moduleDecls m}
                               _   -> ambigious imodid impid
  where
    update m = updateMap modid m loaded
resolveImport _ _ _ _ = error "resolveImport: not DeclImport"

lookupDecl :: Id -> DeclKind -> [Decl a] -> [Decl a]
lookupDecl impid kind decls =
   [d | d <- decls, declName d==impid && declKindFromDecl d == kind]
        
notfound :: Id -> Id -> a
notfound imodid impid = 
   error ("LvmImport.resolveImport: unresolved identifier: " ++ stringFromId imodid ++ "." ++ stringFromId impid)

ambigious :: Id -> Id -> a
ambigious imodid impid = 
   error ("LvmImport.resolveImport: ambigious import record: " ++ stringFromId imodid ++ "." ++ stringFromId impid)
