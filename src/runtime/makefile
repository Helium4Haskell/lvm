#-------------------------------------------------------------------------
#  The Lazy Virtual Machine.
#
#  Daan Leijen.
#
#  Copyright 2001, Daan Leijen. All rights reserved. This file is
#  distributed under the terms of the GNU Library General Public License.
#-------------------------------------------------------------------------

# $Id$ *

include ../config/makefile

CCOPT	   = -O2 $(CCOPTS)
LINKOPT    = $(LINKOPTS)
CCOPT_FAST = -O3 $(CCOPTS)

SRCS	= main.c stats.c \
	  primio.c primsys.c primstring.c \
	  schedule.c evaluator.c instr.c thread.c stack.c \
	  loader.c module.c print.c \
	  ccall.c dynamic.c static.c \
	  fail.c misc.c signals.c systhread.c sys.c \
	  fixed.c bytes.c \
	  alloc.c compact.c custom.c finalise.c freelist.c gc_ctrl.c \
	  major_gc.c minor_gc.c memory.c roots.c weak.c

OBJS	= $(SRCS:.c=.o)


lvmrun$(EXE): $(OBJS)
	$(LINK) -o $@ $(LINKOPT) $(OBJS)
	$(STRIP) $@

evaluator.o: evaluator.c
	$(CC) -c $< $(CCOPT_FAST)


.SUFFIXES: .c .o

.c.o:
	$(CC) -c $< $(CCOPT)

clean:
	$(RM) *.o *.bak lvmrun$(EXE)
	$(RM) runtime.ncb runtime.opt runtime.plg *.obj

depend:
	$(CC) -MM $(CCOPTS) *.c > .depend

include .depend