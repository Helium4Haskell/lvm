/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _schedule_h
#define _schedule_h

#include "mlvalues.h"
#include "module.h"

void evaluate_name( value module, const char* name );

#endif /* _schedule_h */