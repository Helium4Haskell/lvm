{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module Main where

import System     ( getArgs )
import PPrint     ( putDoc )

import Standard   ( getLvmPath, searchPath )
import Id         ( newNameSupply, stringFromId )

import CorePretty ( corePretty )        -- pretty print core
import CoreParse  ( coreParse )         -- parse text into core
import CoreToAsm  ( coreToAsm )         -- enriched lambda expressions (Core) to Asm

import AsmPretty  ( asmPretty )         -- pretty print low-level core (Asm)
import AsmOptimize( asmOptimize )       -- optimize Asm (ie. inlining)
import AsmToLvm   ( asmToLvm )          -- translate Asm to Lvm instructions


import LvmPretty  ( lvmPretty )         -- pretty print instructions (Lvm)
import LvmWrite   ( lvmWriteFile )      -- write a binary Lvm file
import LvmImport  ( lvmImport )         -- resolve import declarations

----------------------------------------------------------------
--
----------------------------------------------------------------
message s
  = return () 
 -- = putStr s

main
  = do{ [arg] <- getArgs
      ; compile arg
      }

findModule paths id
  = searchPath paths ".lvm" (stringFromId id)


compile src
  = do{ path        <- getLvmPath
      ; messageLn ("search path: " ++ show (map showFile path))
      ; source      <- searchPath path ".core" src
      ; messageLn ("compiling  : " ++ showFile source)

      ; mod        <- coreParse source
      ; coremod    <- lvmImport (findModule path) mod

      ; nameSupply  <- newNameSupply
      ; messageLn ("generating code")
      ; let asmmod  = coreToAsm nameSupply coremod
            asmopt  = asmOptimize asmmod
            lvmmod  = asmToLvm  asmopt
            target  = (reverse (drop 5 (reverse source)) ++ ".lvm")

      ; messageDoc "core"         (corePretty coremod)
      -- ; messageDoc "assembler"    (asmPretty asmmod)
      -- ; messageDoc "assembler (optimized)"    (asmPretty asmopt)
      -- ; messageDoc "instructions" (lvmPretty lvmmod)

      ; messageLn  ("writing    : " ++ showFile target)
      ; lvmWriteFile target lvmmod

      ; messageLn   "\ndone."
      }
  where
    messageLn s    = do{ message s; message "\n" }
    messageDoc h d = do{ message (unlines $ ["",line, h, line])
                       ; message (show d)
                       }
    line           = replicate 40 '-'

    showFile fname
      = map (\c -> if (c == '\\') then '/' else c) fname
