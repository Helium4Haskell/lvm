{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id$
module Lvm.Asm.AsmOptimize( asmOptimize ) where

import Lvm.Asm.Asm
import Lvm.Asm.AsmInline ( asmInline )

{---------------------------------------------------------------
  asmOptimize
---------------------------------------------------------------}
asmOptimize :: AsmModule -> AsmModule
asmOptimize mod
  = asmInline mod