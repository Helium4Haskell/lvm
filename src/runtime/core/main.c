/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "mlvalues.h"
#include "memory.h"

#include "module.h"
#include "loader.h"
#include "schedule.h"
#include "options.h"
#include "stats.h"

void start_module( const char* name )
{
  CAMLparam0();
  CAMLlocal1(module);

  stat_start_init();
  module = load_module( name );
  debug_gc();
  stat_end_init();

  evaluate_name( module, "main$" );

  CAMLreturn0;
}

int main( int argc, const char** argv )
{
  const char** args;

  args = init_options(argv);
  if (args[0] == NULL) {
    show_options();
  }
  else {
    start_module( args[0] );
  }

  done_options(true);
  sys_exit(0);
  return 0;
}
