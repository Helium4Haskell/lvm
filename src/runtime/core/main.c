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
  CAMLlocal2(module,code);
  bool showfinal = false;

  stat_start_init();
  module = load_module( name );
  debug_gc();
  stat_end_init();

  /* main is specified */
  if (mainfun!=NULL) {
    code = find_code(module,mainfun);
    if (code==0) {
      fatal_error( "fatal error: \"%s\" is not exported from module \"%s\"\n", mainfun, name );
      CAMLreturn0;
    }
  }
  /* otherwise search for "main$" and than "main" */
  else {
    code = find_code(module,"main$" );
    if (code==0) {
      code = find_code(module,"main");
      if (code==0) {
        fatal_error( "fatal error: neither \"main\" or \"main$\" is defined\n" );
        CAMLreturn0;
      }
      showfinal = true;
    }
  }
  evaluate_code( module, code, showfinal );
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
