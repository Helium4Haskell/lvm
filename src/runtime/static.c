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
#include "module.h"
#include "fail.h"
#include "static.h"

#include "sys.h"  /* get_msec_count */
#include "primio.h"
#include "primsys.h"
#include "primstring.h"
#include "primfloat.h"

#define PRIM(name,tp)  { #name, tp, name },
/*----------------------------------------------------------------------
  Table of primitives
----------------------------------------------------------------------*/
struct prim_info {
  const char* name;
  const char* tp;
  void* fun;
};

static struct prim_info primitives[] = {
  { "getTickCount", "l"   , get_msec_count },

  /* floating point control */
  PRIM(float_of_string,"Fz")
  PRIM(fp_reset       ,"v" )
  PRIM(fp_sticky_mask ,"li")
  PRIM(fp_set_sticky  ,"ll")
  PRIM(fp_get_sticky  ,"l" )
  PRIM(fp_trap_mask   ,"li")
  PRIM(fp_set_traps   ,"ll")
  PRIM(fp_get_traps   ,"l" )
  PRIM(fp_set_round   ,"ii")
  PRIM(fp_get_round   ,"i")
  
  /* file handle's */
  PRIM(prim_flag_mask,"ll")
  PRIM(prim_open,"lzl")
  PRIM(prim_close,"vl")

  PRIM(prim_open_descriptor,"alb")
  PRIM(prim_close_channel,"va")
  PRIM(prim_set_binary_mode, "vab" )
  PRIM(prim_flush_partial, "ba" )
  PRIM(prim_flush, "va" )
  PRIM(prim_output_char, "vac" )
  PRIM(prim_output,"vazll")
  PRIM(prim_input_char, "la" )

  /* string/list conversion */
  PRIM(prim_string_of_chars, "ala" )
  PRIM(prim_chars_of_string, "aa" )
  PRIM(prim_string_length, "la" )

  { NULL, NULL }
};


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
