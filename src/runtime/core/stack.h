/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _stack_h
#define _stack_h

#include "mlvalues.h"

void    lazy_blackhole( value* fp );
value*  recover_semi_synchronous( value* fp );
value*  recover_synchronous( value* fp, value exn );
value*  recover_asynchronous( value* fp, value* sp );
void    recover_eager( struct thread_state* thread );

#endif
