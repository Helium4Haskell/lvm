/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _dynamic_h
#define _dynamic_h

#include "mlvalues.h"
#include "module.h"

/* Maximum library/symbol names. */
#define Max_lib_name  512
#define Max_name      128

/* [load_lib_symbol] loads the address of a symbol in a libary. The library
   should not contain an extension. */
void* load_dynamic_symbol( const char* lib_name, const char* name, enum call_conv cconv
                         , const char* type, enum name_flag flag );

#endif
