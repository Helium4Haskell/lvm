/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _ccall_h
#define _ccall_h

#include "mlvalues.h"
#include "thread.h"

value call_extern( value* args, nat arg_count, void* fun
                 , enum call_conv cconv
                 , value type, value name );

#endif
