#-----------------------------------------------------------------------
#  The Core Assembler.
#
#  Copyright 2001, Daan Leijen. All rights reserved. This file
#  is distributed under the terms of the GHC license. For more
#  information, see the file "license.txt", which is included in
#  the distribution.
#-----------------------------------------------------------------------

# $Id$

# make		 - build everything that is needed
# make depend	 - build dependencies for hs files, ag dependencies are	written	by hand
# make clean	 - remove .hi and .o files

include ../config/makefile

#the main program you want to generate
MAIN	= coreasm

#the haskell compiler
HC	= ghc
HC_OPTS = -static -package lang -i$(IFACES)

#Haskell sources
COMMON	= common/Id.hs common/IdMap.hs common/IdSet.hs common/IntMap.hs \
	  common/Byte.hs common/Standard.hs \
	  common/PPrint.hs common/TopSort.hs \
	  common/ghc/Special.hs

LVM	= lvm/Lvm.hs lvm/LvmPretty.hs lvm/LvmWrite.hs lvm/LvmRead.hs lvm/LvmImport.hs\
	  lvm/Module.hs lvm/ModulePretty.hs \
	  lvm/Instr.hs lvm/InstrPretty.hs lvm/InstrResolve.hs lvm/InstrRewrite.hs

ASM	= asm/AsmOptimize.hs asm/AsmInline.hs asm/AsmOccur.hs \
	  asm/Asm.hs asm/AsmPretty.hs asm/AsmToLvm.hs

CORE	= core/Core.hs core/CorePretty.hs core/CoreToAsm.hs \
	  core/CoreParse.hs core/CoreLexer.hs \
	  core/CoreNormalize.hs \
	  core/CoreNoShadow.hs core/CoreFreeVar.hs core/CoreLetSort.hs \
	  core/CoreLift.hs core/CoreSaturate.hs

PARSEC	= parsec/Parsec.hs \
	  parsec/ParsecChar.hs parsec/ParsecCombinator.hs \
	  parsec/ParsecPrim.hs parsec/ParsecError.hs parsec/ParsecPos.hs

IFACES	= common:common/ghc:lvm:asm:core:parsec


#Sources and object files
SRCS = $(COMMON) $(LVM) $(ASM) $(CORE) $(PARSEC) core/Main.hs
OBJS = $(SRCS:.hs=.o)

.SUFFIXES : .o .hs .hi .lhs .hc	.s .ag

# The main target
$(MAIN)$(EXE): $(OBJS)
	  $(HC)	-o $@ $(HC_OPTS) $(OBJS)
	  $(STRIP) $@

common/ghc/Special.hi: common/ghc/Special.hs
	$(HC) -c $< -cpp -fglasgow-exts $(HC_OPTS) -i$(LIBPATH)


# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.hi:
	$(HC) -c $< $(HC_OPTS) -i$(LIBPATH)

.lhs.hi:
	$(HC) -c $< $(HC_OPTS)

.ag.hs:
	$(AG) -a $<


# Clean	up
clean:
	$(RM) *.hi *.o $(MAIN)$(EXE)
	$(RM) common/*.hi common/*.o common/ghc/*.hi common/ghc/*.o
	$(RM) lvm/*.hi	lvm/*.o
	$(RM) asm/*.hi	asm/*.o
	$(RM) core/*.hi core/*.o
	$(RM) parsec/*.hi parsec/*.o


# Inter-module dependencies
depend :
	ghc -M $(HC_OPTS) $(SRCS)

# DO NOT DELETE: Beginning of Haskell dependencies
common/Id.o : common/Id.hs
common/Id.o : common/IntMap.hi
common/Id.o : common/Standard.hi
common/IdMap.o : common/IdMap.hs
common/IdMap.o : common/Id.hi
common/IdMap.o : common/IntMap.hi
common/IdSet.o : common/IdSet.hs
common/IdSet.o : common/IdMap.hi
common/IdSet.o : common/Id.hi
common/IntMap.o : common/IntMap.hs
common/Byte.o : common/Byte.hs
common/Byte.o : common/Standard.hi
common/Byte.o : common/ghc/Special.hi
common/Standard.o : common/Standard.hs
common/Standard.o : common/ghc/Special.hi
common/PPrint.o : common/PPrint.hs
common/TopSort.o : common/TopSort.hs
common/TopSort.o : common/ghc/Special.hi
common/ghc/Special.o : common/ghc/Special.hs
lvm/Lvm.o : lvm/Lvm.hs
lvm/Lvm.o : lvm/Module.hi
lvm/Lvm.o : lvm/Instr.hi
lvm/Lvm.o : common/Id.hi
lvm/Lvm.o : common/Byte.hi
lvm/LvmPretty.o : lvm/LvmPretty.hs
lvm/LvmPretty.o : lvm/ModulePretty.hi
lvm/LvmPretty.o : lvm/InstrPretty.hi
lvm/LvmPretty.o : lvm/Lvm.hi
lvm/LvmPretty.o : common/PPrint.hi
lvm/LvmWrite.o : lvm/LvmWrite.hs
lvm/LvmWrite.o : lvm/Lvm.hi
lvm/LvmWrite.o : lvm/Instr.hi
lvm/LvmWrite.o : common/Byte.hi
lvm/LvmWrite.o : common/IdMap.hi
lvm/LvmWrite.o : common/Id.hi
lvm/LvmWrite.o : common/Standard.hi
lvm/LvmRead.o : lvm/LvmRead.hs
lvm/LvmRead.o : lvm/ModulePretty.hi
lvm/LvmRead.o : lvm/InstrPretty.hi
lvm/LvmRead.o : lvm/Instr.hi
lvm/LvmRead.o : lvm/Lvm.hi
lvm/LvmRead.o : common/Byte.hi
lvm/LvmRead.o : common/IdMap.hi
lvm/LvmRead.o : common/Id.hi
lvm/LvmRead.o : common/Standard.hi
lvm/LvmRead.o : common/PPrint.hi
lvm/LvmImport.o : lvm/LvmImport.hs
lvm/LvmImport.o : common/PPrint.hi
lvm/LvmImport.o : lvm/InstrPretty.hi
lvm/LvmImport.o : lvm/ModulePretty.hi
lvm/LvmImport.o : lvm/LvmRead.hi
lvm/LvmImport.o : lvm/Lvm.hi
lvm/LvmImport.o : lvm/Module.hi
lvm/LvmImport.o : common/IdMap.hi
lvm/LvmImport.o : common/Id.hi
lvm/LvmImport.o : common/Standard.hi
lvm/Module.o : lvm/Module.hs
lvm/Module.o : lvm/Instr.hi
lvm/Module.o : common/IdSet.hi
lvm/Module.o : common/Id.hi
lvm/Module.o : common/Byte.hi
lvm/Module.o : common/Standard.hi
lvm/ModulePretty.o : lvm/ModulePretty.hs
lvm/ModulePretty.o : lvm/Module.hi
lvm/ModulePretty.o : common/IdMap.hi
lvm/ModulePretty.o : common/Id.hi
lvm/ModulePretty.o : common/Byte.hi
lvm/ModulePretty.o : common/PPrint.hi
lvm/Instr.o : lvm/Instr.hs
lvm/Instr.o : common/Byte.hi
lvm/Instr.o : common/Id.hi
lvm/Instr.o : common/Standard.hi
lvm/InstrPretty.o : lvm/InstrPretty.hs
lvm/InstrPretty.o : lvm/Instr.hi
lvm/InstrPretty.o : common/Id.hi
lvm/InstrPretty.o : common/Byte.hi
lvm/InstrPretty.o : common/PPrint.hi
lvm/InstrResolve.o : lvm/InstrResolve.hs
lvm/InstrResolve.o : lvm/Instr.hi
lvm/InstrResolve.o : common/IdMap.hi
lvm/InstrResolve.o : common/Id.hi
lvm/InstrResolve.o : common/Standard.hi
lvm/InstrRewrite.o : lvm/InstrRewrite.hs
lvm/InstrRewrite.o : lvm/InstrPretty.hi
lvm/InstrRewrite.o : lvm/Instr.hi
lvm/InstrRewrite.o : common/Id.hi
lvm/InstrRewrite.o : common/Standard.hi
asm/AsmOptimize.o : asm/AsmOptimize.hs
asm/AsmOptimize.o : asm/AsmInline.hi
asm/AsmOptimize.o : asm/Asm.hi
asm/AsmInline.o : asm/AsmInline.hs
asm/AsmInline.o : asm/AsmOccur.hi
asm/AsmInline.o : asm/Asm.hi
asm/AsmInline.o : lvm/Module.hi
asm/AsmInline.o : common/IdMap.hi
asm/AsmInline.o : common/Id.hi
asm/AsmInline.o : common/Standard.hi
asm/AsmOccur.o : asm/AsmOccur.hs
asm/AsmOccur.o : asm/Asm.hi
asm/AsmOccur.o : lvm/Module.hi
asm/AsmOccur.o : common/IdMap.hi
asm/AsmOccur.o : common/Id.hi
asm/Asm.o : asm/Asm.hs
asm/Asm.o : lvm/Module.hi
asm/Asm.o : common/Id.hi
asm/Asm.o : common/Byte.hi
asm/AsmPretty.o : asm/AsmPretty.hs
asm/AsmPretty.o : lvm/ModulePretty.hi
asm/AsmPretty.o : asm/Asm.hi
asm/AsmPretty.o : common/Id.hi
asm/AsmPretty.o : common/Byte.hi
asm/AsmPretty.o : common/PPrint.hi
asm/AsmToLvm.o : asm/AsmToLvm.hs
asm/AsmToLvm.o : asm/AsmOccur.hi
asm/AsmToLvm.o : lvm/InstrRewrite.hi
asm/AsmToLvm.o : lvm/InstrResolve.hi
asm/AsmToLvm.o : lvm/Instr.hi
asm/AsmToLvm.o : lvm/Lvm.hi
asm/AsmToLvm.o : asm/Asm.hi
asm/AsmToLvm.o : common/IdMap.hi
asm/AsmToLvm.o : common/Id.hi
asm/AsmToLvm.o : common/Standard.hi
core/Core.o : core/Core.hs
core/Core.o : common/IdSet.hi
core/Core.o : common/IdMap.hi
core/Core.o : lvm/Module.hi
core/Core.o : common/Id.hi
core/Core.o : common/Byte.hi
core/CorePretty.o : core/CorePretty.hs
core/CorePretty.o : lvm/ModulePretty.hi
core/CorePretty.o : core/Core.hi
core/CorePretty.o : common/IdSet.hi
core/CorePretty.o : common/Id.hi
core/CorePretty.o : common/Byte.hi
core/CorePretty.o : common/PPrint.hi
core/CoreToAsm.o : core/CoreToAsm.hs
core/CoreToAsm.o : core/CoreLift.hi
core/CoreToAsm.o : core/CoreLetSort.hi
core/CoreToAsm.o : core/CoreFreeVar.hi
core/CoreToAsm.o : core/CoreNormalize.hi
core/CoreToAsm.o : core/CoreSaturate.hi
core/CoreToAsm.o : core/CoreNoShadow.hi
core/CoreToAsm.o : asm/Asm.hi
core/CoreToAsm.o : core/Core.hi
core/CoreToAsm.o : common/IdSet.hi
core/CoreToAsm.o : common/IdMap.hi
core/CoreToAsm.o : common/Id.hi
core/CoreToAsm.o : common/Standard.hi
core/CoreParse.o : core/CoreParse.hs
core/CoreParse.o : core/CorePretty.hi
core/CoreParse.o : core/CoreLexer.hi
core/CoreParse.o : core/Core.hi
core/CoreParse.o : common/IdSet.hi
core/CoreParse.o : common/IdMap.hi
core/CoreParse.o : common/Id.hi
core/CoreParse.o : common/Byte.hi
core/CoreParse.o : parsec/Parsec.hi
core/CoreParse.o : common/Standard.hi
core/CoreParse.o : common/PPrint.hi
core/CoreLexer.o : core/CoreLexer.hs
core/CoreLexer.o : common/Standard.hi
core/CoreNormalize.o : core/CoreNormalize.hs
core/CoreNormalize.o : core/Core.hi
core/CoreNormalize.o : common/IdSet.hi
core/CoreNormalize.o : common/Id.hi
core/CoreNoShadow.o : core/CoreNoShadow.hs
core/CoreNoShadow.o : core/Core.hi
core/CoreNoShadow.o : common/IdSet.hi
core/CoreNoShadow.o : common/IdMap.hi
core/CoreNoShadow.o : common/Id.hi
core/CoreFreeVar.o : core/CoreFreeVar.hs
core/CoreFreeVar.o : core/Core.hi
core/CoreFreeVar.o : common/IdSet.hi
core/CoreFreeVar.o : common/Id.hi
core/CoreFreeVar.o : common/Standard.hi
core/CoreLetSort.o : core/CoreLetSort.hs
core/CoreLetSort.o : core/Core.hi
core/CoreLetSort.o : common/IdSet.hi
core/CoreLetSort.o : common/Id.hi
core/CoreLetSort.o : common/TopSort.hi
core/CoreLift.o : core/CoreLift.hs
core/CoreLift.o : core/Core.hi
core/CoreLift.o : common/IdSet.hi
core/CoreLift.o : common/IdMap.hi
core/CoreLift.o : common/Id.hi
core/CoreLift.o : common/Standard.hi
core/CoreSaturate.o : core/CoreSaturate.hs
core/CoreSaturate.o : core/Core.hi
core/CoreSaturate.o : common/IdMap.hi
core/CoreSaturate.o : common/Id.hi
parsec/Parsec.o : parsec/Parsec.hs
parsec/Parsec.o : parsec/ParsecChar.hi
parsec/Parsec.o : parsec/ParsecCombinator.hi
parsec/Parsec.o : parsec/ParsecPrim.hi
parsec/Parsec.o : parsec/ParsecError.hi
parsec/Parsec.o : parsec/ParsecPos.hi
parsec/ParsecChar.o : parsec/ParsecChar.hs
parsec/ParsecChar.o : parsec/ParsecPrim.hi
parsec/ParsecChar.o : parsec/ParsecPos.hi
parsec/ParsecCombinator.o : parsec/ParsecCombinator.hs
parsec/ParsecCombinator.o : parsec/ParsecPrim.hi
parsec/ParsecPrim.o : parsec/ParsecPrim.hs
parsec/ParsecPrim.o : parsec/ParsecError.hi
parsec/ParsecPrim.o : parsec/ParsecPos.hi
parsec/ParsecError.o : parsec/ParsecError.hs
parsec/ParsecError.o : parsec/ParsecPos.hi
parsec/ParsecPos.o : parsec/ParsecPos.hs
core/Main.o : core/Main.hs
core/Main.o : lvm/LvmRead.hi
core/Main.o : lvm/LvmImport.hi
core/Main.o : lvm/LvmWrite.hi
core/Main.o : lvm/LvmPretty.hi
core/Main.o : asm/AsmToLvm.hi
core/Main.o : asm/AsmOptimize.hi
core/Main.o : asm/AsmPretty.hi
core/Main.o : core/CoreToAsm.hi
core/Main.o : core/CoreParse.hi
core/Main.o : core/CorePretty.hi
core/Main.o : common/Id.hi
core/Main.o : common/Standard.hi
core/Main.o : common/PPrint.hi
# DO NOT DELETE: End of Haskell dependencies
