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
          , declName, declType, declBytes, declModule, declValue
          , declCon, declExtern, declImport
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

declName,declType,declBytes,declModule,declValue,declCon,declExtern,declImport :: Int
declName      = 0
declType      = 1
declBytes     = 2
declModule    = 3
declValue     = 4
declCon       = 5
declExtern    = 6
declImport    = 7
