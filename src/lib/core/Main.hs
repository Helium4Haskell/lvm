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
import Id         ( newNameSupply )

import CorePretty ( corePretty )        -- pretty print core
import CoreParse  ( coreParse )         -- parse text into core
import CoreToAsm  ( coreToAsm )         -- enriched lambda expressions (Core) to Asm

import AsmPretty  ( asmPretty )         -- pretty print low-level core (Asm)
import AsmToLvm   ( asmToLvm )          -- translate Asm to instructions

import LvmPretty  ( lvmPretty )
import LvmWrite   ( lvmWriteFile )

----------------------------------------------------------------
--
----------------------------------------------------------------
main
  = do{ [arg] <- getArgs
      ; compile arg
      }

compile src
  = do{ path              <- getLvmPath
      ; putStrLn ("search path: " ++ show (map showFile path))
      ; source            <- searchPath path ".core" src
      ; putStrLn ("compiling  : " ++ showFile source)

      ; (coremod,imports) <- coreParse source
      ; nameSupply        <- newNameSupply

      ; putStrLn ("generating code")
      ; let asmmod        = coreToAsm nameSupply coremod
            lvmmod        = asmToLvm  asmmod
            target        = (reverse (drop 5 (reverse source)) ++ ".lvm")

      ; putHeader "core"; putDoc (corePretty coremod)
      ; putHeader "assembler"; putDoc (asmPretty asmmod)
      ; putHeader "instructions"; putDoc (lvmPretty lvmmod)

      ; putStrLn  ("writing    : " ++ showFile target)
      ; lvmWriteFile target lvmmod

      ; putStrLn   "\ndone."
      }
  where
    putHeader h = putStr $ unlines $ ["",line, h, line]
    line        = replicate 40 '-'

    showFile fname
      = map (\c -> if (c == '\\') then '/' else c) fname
