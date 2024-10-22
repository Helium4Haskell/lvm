/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _evaluator_h
#define _evaluator_h

#include "mlvalues.h"
#include "module.h"
#include "thread.h"

void init_evaluator(void);
void evaluate( struct thread_state* thread );

#endif /* _evaluator_h */
