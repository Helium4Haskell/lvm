/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

/*----------------------------------------------------------------------
  loading of static libraries.
----------------------------------------------------------------------*/
#include <stdio.h>
#include <string.h>
#include "mlvalues.h"
#include "fail.h"
#include "prims.h"
#include "primitives.h"

#include "../core/sys.h"          /* get_msec_count */

/*----------------------------------------------------------------------
  declare all primitive functions
----------------------------------------------------------------------*/
#define PRIM(name,tp)  void name(void);
PRIMS
#undef PRIM

/*----------------------------------------------------------------------
  Table of primitives
----------------------------------------------------------------------*/
struct prim_info {
  const char* name;
  const char* tp;
  void* fun;
};

#define PRIM(name,tp)  { #name, tp, name },  
static struct prim_info primitives[] = {
  { "getTickCount", "l"   , get_msec_count },  
  PRIMS
  { NULL, NULL }
};
#undef PRIM

/*----------------------------------------------------------------------
   [load_static_symbol] loads the address of a symbol in a static libary. The library
   should not contain a file extension. If the [lib_name] is NULL, the function
   should be defined inside the lvm. This is the only mode supported at this moment.
----------------------------------------------------------------------*/
void* load_static_symbol( const char* lib_name, const char* name
                        , enum call_conv cconv, const char* type, enum name_flag flag )
{
  struct prim_info* prim;

  /* can only load primitives */
  if (lib_name != NULL && strlen(lib_name) > 0) {
    raise_internal( "loader: static linking is not supported (%s)", lib_name );
  }

  /* no ordinals allowed (decoration is ignored) */
  if (flag == Name_ordinal) {
    raise_internal( "loader: no ordinals allowed for static linking (%i)", (long)name );
  }


  /* lookup the name */
  prim = primitives;
  while (prim->name != NULL) {
    if (strcmp(prim->name,name) == 0) break;
    prim++;
  }

  if (prim->name == NULL) {
    raise_internal( "loader: unknown primitive: %s", name );
  }

  if (type && strlen(type) != strlen(prim->tp)) {
    raise_internal( "loader: can not match types for %s. \"%s\" vs. \"%s\"", name, type, prim->tp );
  }

  return prim->fun;
}
