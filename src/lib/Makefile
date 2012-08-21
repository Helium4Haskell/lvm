.PHONY: coreasm

default: coreasm

#-----------------------------------------------------------------------
# Compilers and Tools
#-----------------------------------------------------------------------

HC      = ghc
HADDOCK = haddock
HLINT   = hlint

HC_OPTS = -Wall -odir $(OUTDIR) -hidir $(OUTDIR)

OUTDIR = out
DOCDIR = out

RM = rm -rf

coreasm:
	$(HC) --make $(HC_OPTS) -o coreasm Lvm/Core/Main

report:
	$(HLINT) --report=$(DOCDIR)/report.html .

# Clean	up
clean:
	$(RM) out
	$(RM) *.hi *.o $(MAIN)$(EXE)
	$(RM) common/*.hi common/*.o common/ghc/*.hi common/ghc/*.o
	$(RM) lvm/*.hi	lvm/*.o
	$(RM) asm/*.hi	asm/*.o
	$(RM) core/*.hi core/*.o