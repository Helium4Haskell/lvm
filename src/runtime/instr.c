/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>
#include "mlvalues.h"
#include "module.h"
#include "instr.h"
#include "fail.h"

/* build a table with instruction information */
struct instr_info
{
  enum instruction instr;
  char* name;
  int   args;
};

#define newinfo(instr,args)    { instr, #instr, args }

#define Ins(instr,args)   { instr, #instr, args }
struct instr_info  instr_infos[STOP+1] = { INSTRLIST };
#undef Ins


static struct instr_info* find_info( enum instruction instr )
{
  struct instr_info* info;

  Assert( instr <= STOP );
  if (instr > STOP) raise_internal( "invalid instruction (%i)", instr );

  info = &instr_infos[instr];
  Assert( info->instr == instr );
  return info;
}

int instr_arg_count( enum instruction instr )
{
  return find_info(instr)->args;
}

char* instr_name( enum instruction instr )
{
  return find_info(instr)->name;
}
