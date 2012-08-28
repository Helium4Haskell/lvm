{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

module Main where

import System.Environment      ( getArgs )
import Text.PrettyPrint.Leijen ( pretty )

import Lvm.Path
import Lvm.Common.Id         ( Id, newNameSupply, stringFromId )

import Lvm.Core.Module ( Module, modulePublic )
import Lvm.Core.Parse  ( coreParseExport )
import Lvm.Core.Parser ( parseModule )       -- new core syntax

                                        -- parse text into Core
import Lvm.Core.RemoveDead( coreRemoveDead ) -- remove dead declarations
import Lvm.Core.ToAsm  ( coreToAsm )         -- enriched lambda expressions (Core) to Asm
import Lvm.Core.Data (Expr)

import Lvm.Asm.Optimize( asmOptimize )       -- optimize Asm (ie. local inlining)
import Lvm.Asm.ToLvm   ( asmToLvm )          -- translate Asm to Lvm instructions

import Lvm.Write   ( lvmWriteFile )      -- write a binary Lvm file
import Lvm.Import  ( lvmImport )         -- resolve import declarations
import Lvm.Read    ( lvmReadFile )       -- read a Lvm file
import Lvm.Data (LvmModule)

----------------------------------------------------------------
--
----------------------------------------------------------------

message :: Monad m => a -> m ()
message _ = return ()

main :: IO ()
main
  = do{ args <- getArgs
      ; if length args == 1 then 
           compile (head args)
         else
           putStrLn "Usage: coreasm <module>" 
      }

findModule :: [String] -> Id -> IO String
findModule paths = searchPath paths ".lvm" . stringFromId


findSrc :: [String] -> String -> IO (Either String String)
findSrc path src
  = do{ res <- searchPathMaybe path ".core" src
      ; case res of
          Just source -> return (Left source)
          Nothing     -> do{ source <- searchPath path ".cor" src
                           ; print source
                           ; return (Right source)
                           }
      }

parse :: [String] -> String -> IO (Module Expr, String)
parse path src
  = do{ res <- findSrc path src 
      ; case res of
          Left source -> do{ messageLn ("parsing")
                           ; (m, implExps, es) <- coreParseExport source
                           ; messageDoc "parsed"  (pretty m)
                           ; messageLn ("resolving imports")
                           ; chasedMod  <- lvmImport (findModule path) m
                           ; messageLn ("making exports public")
                           ; let publicmod = modulePublic implExps es chasedMod
                           ; return (publicmod,source)
                           }
          Right source ->do{ messageLn ("parsing")
                           ; m <- parseModule source
                           ; messageDoc "parsed"  (pretty m)
                           ; messageLn ("resolving imports")
                           ; chasedMod  <- lvmImport (findModule path) m
                           ; return (chasedMod,source)
                           }
      }                       

compile :: String -> IO ()
compile src
  = do{ lvmPath        <- getLvmPath
      ; let path = "." : lvmPath
      ; messageLn ("search path: " ++ show (map showFile path))
      
      ; (m,source) <- parse path src
      
      ; messageLn ("remove dead declarations")
      ; let coremod = coreRemoveDead m

      ; nameSupply  <- newNameSupply
      ; messageLn ("generating code")
      ; let asmmod  = coreToAsm nameSupply coremod
            asmopt  = asmOptimize asmmod
            lvmmod  = asmToLvm  asmopt

--      ; messageDoc "core"         (corePretty coremod)
--      ; messageDoc "assembler"    (asmPretty asmmod)
--      ; messageDoc "assembler (optimized)"    (asmPretty asmopt)
      ; messageDoc "instructions" (pretty lvmmod)

      ; let target  = (reverse (dropWhile (/='.') (reverse source)) ++ "lvm")
      ; messageLn  ("writing    : " ++ showFile target)
      ; lvmWriteFile target lvmmod

      ; messageLn   "\ndone."
      }

dump :: String -> IO ()
dump src
  = do{ path        <- getLvmPath
      ; messageLn ("search path: " ++ show (map showFile path))
      ; source      <- searchPath path ".lvm" src
      ; messageLn ("reading   : " ++ showFile source)
      ; m           <- lvmReadFile source
      ; messageDoc "module" (pretty m)
      ; coremod    <- lvmImport (findModule path) m
      ; messageDoc "resolved module" (pretty (coremod :: LvmModule))
      }

messageLn :: Monad m => t -> m ()
messageLn s    
  = do{ message s; message "\n" }
  
messageDoc :: (Show a, Monad m) => String -> a -> m ()
messageDoc h d 
  = do{ message (unlines $ ["",line, h, line])
       ; message (show d)
       }

line :: String
line = replicate 40 '-'

showFile :: String -> String
showFile = map (\c -> if (c == '\\') then '/' else c)

