{-*-----------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
-----------------------------------------------------------------------*-}

-- $Id$

module Lvm( module Module, LvmModule, LvmValue

          -- constants
          , recName,recBytes,recCode,recValue,recCon 
          , recImport,recModule,recExtern,recExternType,recHeader,recFooter 
          , flagPrivate, flagPublic
          ) where

import Byte    ( Byte )
import Id      ( Id )
import Instr   ( Instr )
import Module

{--------------------------------------------------------------
  An LVM module
---------------------------------------------------------------}
type LvmModule  = Module [Instr]
type LvmValue   = DValue [Instr]

{---------------------------------------------------------------
  Constants
---------------------------------------------------------------}
flagPrivate,flagPublic :: Int
flagPrivate   = 0
flagPublic    = 1

recName,recBytes,recCode,recValue,recCon :: Int
recImport,recModule,recExtern,recExternType,recHeader,recFooter :: Int
recName       = 0
recBytes      = 1
recCode       = 2
recValue      = 3
recCon        = 4
recImport     = 5
recModule     = 6
recExtern     = 7
recExternType = 8

recHeader     = 0x4c564d58
recFooter     = 0x4c564d59