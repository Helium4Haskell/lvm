/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _dynamic_h
#define _dynamic_h

#include "module.h"

void init_dynamic(void);
void done_dynamic(void);

/*----------------------------------------------------------------------
  [load_dynamic_symbol]:
  the toplevel routine to load symbols. All resource management and
  decoration is done automatically. Returns a custom [Symbol] block with
  the symbol pointer in the [Symbol_fun] field.
----------------------------------------------------------------------*/
#define Symbol_lib(v) (((struct dynamic_lib**)(Data_custom_val(v)))[0])
#define Symbol_fun(v) (((void**)(Data_custom_val(v)))[1])

value load_dynamic_symbol( const char* lib_name, const char* name, enum call_conv cconv
                         , const char* type, enum name_flag flag );

#endif