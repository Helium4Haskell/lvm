--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file 
-- is distributed under the terms of the BSD3 License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id$

-- | This module performs import resolution for lazy virtual machine files.
--
-- Two versions of the import functions are provided.
--
-- A variation that runs in 'IO'
--
--     * Requires a function that, given a module 'Id', finds the 'FilePath' of a module.
--
-- A generalized version, suffixed by @'@
--
--     * Requires a function that, given a module 'Id', load a 'Module'.
--     * Lets the caller determine the choice of monad. This is useful
--       for testing and integration.
--
module Lvm.Import (lvmImport, lvmImport', lvmImportDecls, lvmImportDecls') where

import Control.Monad
import Data.List 
import Lvm.Common.Id
import Lvm.Common.IdMap
import Lvm.Data
import Lvm.Read  (lvmReadFile)
import qualified Lvm.Core.Module as Module

-- | Replace all import declarations with abstract declarations or
-- constructors\/externs\/customs
--
-- @lvmImport findModulePath = lvmImport' (findModulePath >=> 'lvmReadFile')@
--
lvmImport :: (Id -> IO FilePath) -> Module v -> IO (Module v)
lvmImport findModule = lvmImport' (findModule >=> lvmReadFile)

-- | A more general 'lvmImport'. Works in any monad, but requires the
-- caller to provide a 'Module' instead of a 'FilePath'. Replace all
-- import declarations with abstract declarations or
-- constructors\/externs\/customs.
lvmImport' :: Monad m => (Id -> m (Module v)) -> Module v -> m (Module v)
lvmImport' findModule m
  = do{ mods <- lvmImportModules findModule m
      ; let mods0 = lvmExpandModule mods (moduleName m)
            mods1 = lvmResolveImports mods0
            mod1  = findMap (moduleName m) mods1
      ; return mod1{ moduleDecls = filter (not . isDeclImport) (moduleDecls mod1) }
      }

-- | 
--
-- @lvmImportDecls findModulePath = lvmImportDecls' (findModulePath >=> 'lvmReadFile')@
--
lvmImportDecls :: (Id -> IO FilePath) -> [Decl v] -> IO [[Decl v]]
lvmImportDecls findModulePath = lvmImportDecls' (findModulePath >=> lvmReadFile)

-- | More general version of 'lvmImportDecls''. Works in any monad, but requires the
-- caller to provide a 'Module' instead of a 'FilePath'.
lvmImportDecls' :: Monad m => (Id -> m (Module v)) -> [Decl v] -> m [[Decl v]]
lvmImportDecls' findModule = mapM $ \importDecl -> do
   m <- lvmImport' findModule
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
lvmImportModules :: Monad m => (Id -> m (Module v)) -> Module v -> m (IdMap (Module v))
lvmImportModules findModule m
  = readModuleImports findModule emptyMap (moduleName m) m

readModuleImports :: Monad m => (Id -> m (Module v)) -> IdMap (Module v) -> Id -> Module v -> m (IdMap (Module v))
readModuleImports findModule loaded x m
  = foldM (readModule findModule) (insertMap x m loaded) (imported m)

readModule :: Monad m => (Id -> m (Module v)) -> IdMap (Module v) -> Id -> m (IdMap (Module v))
readModule findModule loaded x
  | elemMap x loaded  = return loaded
  | otherwise         = do{ m <- findModule x
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
lvmResolveImports mods = 
  let mods_ = mapMap (\x -> (x, [])) mods
      result = foldl' resolveImports mods_ (listFromMap mods)
  in mapMap fst result

resolveImports :: IdMap (Module v, [(Id, Id, DeclKind)]) -> (Id,Module v) -> IdMap (Module v, [(Id, Id, DeclKind)])
resolveImports loaded (modid, m)
  = foldl' (resolveImport [] modid) loaded (let res = filter isDeclImport (moduleDecls m) in {-traceShow (stringFromId modid, res)-} res)

resolveImport :: [Id] -> Id -> IdMap (Module v, [(Id, Id, DeclKind)]) -> Decl v -> IdMap (Module v, [(Id, Id, DeclKind)])
resolveImport visited modid loaded imp@(DeclImport x access@(Imported _ imodid impid kind _ _) _)
  | modid `elem` visited = error ("LvmImport.resolveImport: circular import chain: " ++ stringFromId imodid ++ "." ++ stringFromId impid)
  | otherwise = 
    let (m,seen) = findMap modid loaded in 
    if (imodid, impid, kind) `elem` seen 
      then loaded
      else
        case lookupMap imodid loaded of
          Nothing   -> error ("LvmImport.resolveImport: import module is not loaded: " ++ stringFromId imodid)
          Just (imod,_) -> case lookupDecl impid kind (moduleDecls imod) of
                            []   -> notfound imodid impid
                            ds   -> case filter (not . isDeclImport) ds of
                                      []  -> case filter isDeclImport ds of
                                              []  -> notfound imodid impid
                                              [d] -> let loaded' = resolveImport (modid:visited) imodid loaded d
                                                      in resolveImport (imodid:visited) modid loaded' imp
                                              _   -> ambigious imodid impid
                                      [d] -> update (m { moduleDecls = d{declName=x,declAccess = access} : moduleDecls m}, (imodid, impid, kind) : seen)
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
