{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$

module Main where

import Control.Monad
import System.Environment      ( getArgs )
import System.Console.GetOpt
import System.Exit
import Text.PrettyPrint.Leijen ( Pretty(..), empty, vsep )

import Lvm.Path
import Lvm.Common.Id         ( Id, newNameSupply, stringFromId )

import Lvm.Core.Module ( Module, modulePublic )
import Lvm.Core.Parse  ( coreParseExport )
-- import Lvm.Core.Parser ( parseModule )       -- new core syntax

                                        -- parse text into Core
import Lvm.Core.RemoveDead( coreRemoveDead ) -- remove dead declarations
import Lvm.Core.ToAsm  ( coreToAsm )         -- enriched lambda expressions (Core) to Asm
import Lvm.Core.Data (Expr)

import Lvm.Asm.Optimize( asmOptimize )       -- optimize Asm (ie. local inlining)
import Lvm.Asm.ToLvm   ( asmToLvm )          -- translate Asm to Lvm instructions

import Lvm.Write   ( lvmWriteFile )      -- write a binary Lvm file
import Lvm.Import  ( lvmImport )         -- resolve import declarations
-- import Lvm.Read    ( lvmReadFile )       -- read a Lvm file
-- import Lvm.Data (LvmModule)

----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main = do
   args <- getArgs
   case getOpt Permute options args of
      (flags, files, errs)
         | Help `elem` flags -> do
              putStrLn (usageInfo header options)
              exitSuccess
         | Version `elem` flags -> do
              putStrLn ("coreasm, " ++ versionText)
              exitSuccess
         | null errs && not (null files) -> do
              mapM_ (compile flags) files
              exitSuccess
         | otherwise -> do
              putStrLn $ concat errs ++ 
                 "Usage: For basic information, try the --help option."
              exitFailure

versionText :: String
versionText = "version 1.7" -- $Id$"

header :: String
header = 
   "coreasm: The Core Assembler for the Lazy Virtual Machine\n" ++
   "Copyright 2001, Daan Leijen\n" ++
   "\nUsage: coreasm [OPTION] <core modules>\n" ++
   "\nOptions:"

data Flag = Help | Version | Verbosity Verbosity | Dump Dump (Maybe String)
   deriving Eq

data Verbosity = Silent | Normal | Verbose
   deriving (Eq, Ord)

data Dump = DumpCore | DumpCoreOpt | DumpAsm | DumpAsmOpt | DumpInstr
   deriving Eq

getVerbosity :: [Flag] -> Verbosity
getVerbosity flags = 
   case [ a | Verbosity a <- flags ] of
      [] -> Normal 
      xs -> minimum xs

flagVerbose, flagSilent :: Flag
flagVerbose = Verbosity Verbose
flagSilent  = Verbosity Silent

options :: [OptDescr Flag]
options =
     [ simple []  "version"       Version     "show version number"
     , simple "?" "help"          Help        "show options"
     , simple []  "verbose"       flagVerbose "verbose output"
     , simple []  "silent"        flagSilent  "no output"
     , dump "dump-core"     DumpCore    "pretty print core"
     , dump "dump-core-opt" DumpCoreOpt "pretty print core (optimized)"
     , dump "dump-asm"      DumpAsm     "pretty print assembler"
     , dump "dump-asm-opt"  DumpAsmOpt  "pretty print assembler (optimized)"
     , dump  "dump-instr"    DumpInstr   "pretty print instructions"
     ]
 where
   simple xs long = Option xs [long] . NoArg
   dump long d    = Option [] [long] (OptArg (Dump d) "file")

findModule :: [String] -> Id -> IO String
findModule paths = searchPath paths ".lvm" . stringFromId

findSrc :: [String] -> String -> IO String
findSrc paths = searchPath paths ".core"

parse :: [Flag] -> [String] -> String -> IO (Module Expr, String)
parse flags path src
  = do{ source <- findSrc path src 
      ; do{ -- messageLn flags "parsing"
                           ; (m, implExps, es) <- coreParseExport source
                           --; messageDoc flags "parsed"  (pretty m)
                          --  ; messageLn flags "resolving imports"
                           ; chasedMod  <- lvmImport (findModule path) m
                           -- ; messageLn flags "making exports public"
                           ; let publicmod = modulePublic implExps es chasedMod
                           ; return (publicmod,source)
                           } {- 
          Right source ->do{ messageLn flags "parsing"
                           ; m <- parseModule source
                           ; messageDoc flags "parsed"  (pretty m)
                           ; messageLn flags "resolving imports"
                           ; chasedMod  <- lvmImport (findModule path) m
                           ; return (chasedMod,source)
                           } -}
      }                       

compile :: [Flag] -> String -> IO ()
compile flags src = do
   message flags $ "Compiling " ++ showFile src
   lvmPath        <- getLvmPath
   let path = "." : lvmPath
   verbose $ "Search path: " ++ show (map showFile path)
   
   (m,source) <- parse flags path src
   dumpWith DumpCore flags "Core" m
   
   verbose "Remove dead declarations"
   let coremod = coreRemoveDead m
   dumpWith DumpCoreOpt flags "Core (dead declarations removed)" coremod

   verbose "Generating code"
   nameSupply <- newNameSupply
   
   let asmmod = coreToAsm nameSupply coremod
   dumpWith DumpAsm flags "Assembler" asmmod
   
   let asmopt = asmOptimize asmmod
   dumpWith DumpAsmOpt flags "Assembler (optimized)" asmopt
   
   let lvmmod = asmToLvm  asmopt
   dumpWith DumpInstr flags "Instructions" lvmmod

   let target  = reverse (dropWhile (/='.') (reverse source)) ++ "lvm"
   message flags $ "Writing " ++ showFile target
   lvmWriteFile target lvmmod

 where
   verbose :: Pretty a => a -> IO () 
   verbose = messageFor Verbose flags
       
       {-
dump :: [Flag] -> String -> IO ()
dump flags src
  = do{ path        <- getLvmPath
      ; messageLn flags ("search path: " ++ show (map showFile path))
      ; source      <- searchPath path ".lvm" src
      ; messageLn flags ("reading   : " ++ showFile source)
      ; m           <- lvmReadFile source
     --; messageDoc flags "module" (pretty m)
      ; coremod    <- lvmImport (findModule path) m
      --; messageDoc flags "resolved module" (pretty (coremod :: LvmModule))
      } -}

---------------------------------------------------------------------
-- Messages

message :: Pretty a => [Flag] -> a -> IO ()
message = messageFor Normal

messageFor :: Pretty a => Verbosity -> [Flag] -> a -> IO ()
messageFor a flags = 
   when (getVerbosity flags >= a) . messageDoc

messageDoc :: Pretty a => a -> IO ()
messageDoc = putStrLn . show . pretty

dumpWith :: Pretty a => Dump -> [Flag] -> String -> a -> IO () 
dumpWith dump flags s a = 
   case [ mf | Dump d mf <- flags, d == dump ] of
      Just file:_ -> do message flags ("Writing " ++ file)
                        writeFile file (show (pretty a))
      Nothing:_   -> messageDoc nice
      []          -> return ()
 where
   nice = vsep [line, pretty ("-- " ++ s), empty, pretty a, line]
   line = pretty (replicate 40 '-')

showFile :: String -> String
showFile = map (\c -> if c == '\\' then '/' else c)