/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _static_h
#define _static_h

#include "mlvalues.h"
#include "module.h"

/* [load_static_symbol] loads the address of a symbol in a static libary. The library
   should not contain a file extension. If the [lib_name] is NULL, the function
   should be defined inside the lvm. This is the only mode supported yet. */
void* load_static_symbol( const char* lib_name, const char* name
                        , enum call_conv cconv, const char* type, enum name_flag flag );


#endif
