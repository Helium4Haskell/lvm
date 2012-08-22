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
import Text.PrettyPrint.Leijen ( putDoc )

import Lvm.Common.Standard   ( getLvmPath, searchPath, searchPathMaybe )
import Lvm.Common.Id         ( newNameSupply, stringFromId )

import Lvm.Core.Pretty ( corePretty )        -- pretty print Core
import Lvm.Core.Parse  ( coreParseExport, modulePublic )
import Lvm.Core.Parser ( parseModule )       -- new core syntax

                                        -- parse text into Core
import Lvm.Core.RemoveDead( coreRemoveDead ) -- remove dead declarations
import Lvm.Core.ToAsm  ( coreToAsm )         -- enriched lambda expressions (Core) to Asm

import Lvm.Asm.Pretty  ( asmPretty )         -- pretty print low-level core (Asm)
import Lvm.Asm.Optimize( asmOptimize )       -- optimize Asm (ie. local inlining)
import Lvm.Asm.ToLvm   ( asmToLvm )          -- translate Asm to Lvm instructions

import Lvm.Pretty  ( lvmPretty )         -- pretty print instructions (Lvm)
import Lvm.Write   ( lvmWriteFile )      -- write a binary Lvm file
import Lvm.Import  ( lvmImport )         -- resolve import declarations
import Lvm.Read    ( lvmReadFile )       -- read a Lvm file

----------------------------------------------------------------
--
----------------------------------------------------------------
message s
   = return () 
  -- = putStr s

main
  = do{ args <- getArgs
      ; if length args == 1 then 
           compile (head args)
         else
           putStrLn "Usage: coreasm <module>" 
      }

findModule paths id
  = searchPath paths ".lvm" (stringFromId id)


findSrc path src
  = do{ res <- searchPathMaybe path ".core" src
      ; case res of
          Just source -> return (Left source)
          Nothing     -> do{ source <- searchPath path ".cor" src
                           ; print source
                           ; return (Right source)
                           }
      }

parse path src
  = do{ res <- findSrc path src 
      ; case res of
          Left source -> do{ messageLn ("parsing")
                           ; (mod, implExps, es) <- coreParseExport source
                           ; messageDoc "parsed"  (corePretty mod)
                           ; messageLn ("resolving imports")
                           ; chasedMod  <- lvmImport (findModule path) mod
                           ; messageLn ("making exports public")
                           ; let publicmod = modulePublic implExps es chasedMod
                           ; return (publicmod,source)
                           }
          Right source ->do{ messageLn ("parsing")
                           ; mod <- parseModule source
                           ; messageDoc "parsed"  (corePretty mod)
                           ; messageLn ("resolving imports")
                           ; chasedMod  <- lvmImport (findModule path) mod
                           ; return (chasedMod,source)
                           }
      }                       

compile src
  = do{ lvmPath        <- getLvmPath
      ; let path = "." : lvmPath
      ; messageLn ("search path: " ++ show (map showFile path))
      
      ; (mod,source) <- parse path src
      
      ; messageLn ("remove dead declarations")
      ; let coremod = coreRemoveDead mod

      ; nameSupply  <- newNameSupply
      ; messageLn ("generating code")
      ; let asmmod  = coreToAsm nameSupply coremod
            asmopt  = asmOptimize asmmod
            lvmmod  = asmToLvm  asmopt

--      ; messageDoc "core"         (corePretty coremod)
--      ; messageDoc "assembler"    (asmPretty asmmod)
--      ; messageDoc "assembler (optimized)"    (asmPretty asmopt)
      ; messageDoc "instructions" (lvmPretty lvmmod)

      ; let target  = (reverse (dropWhile (/='.') (reverse source)) ++ "lvm")
      ; messageLn  ("writing    : " ++ showFile target)
      ; lvmWriteFile target lvmmod

      ; messageLn   "\ndone."
      }


dump src
  = do{ path        <- getLvmPath
      ; messageLn ("search path: " ++ show (map showFile path))
      ; source      <- searchPath path ".lvm" src
      ; messageLn ("reading   : " ++ showFile source)
      ; mod         <- lvmReadFile source
      ; messageDoc "module" (lvmPretty mod)
      ; coremod    <- lvmImport (findModule path) mod
      ; messageDoc "resolved module" (lvmPretty coremod)
      }

messageLn s    
  = do{ message s; message "\n" }
messageDoc h d 
  = do{ message (unlines $ ["",line, h, line])
       ; message (show d)
       }

line           
  = replicate 40 '-'

showFile fname
  = map (\c -> if (c == '\\') then '/' else c) fname

