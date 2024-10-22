/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef heap_h
#define heap_h

#include "mlvalues.h"

#include "memory.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "minor_gc.h"
#include "major_gc.h"

/*---------------------------------------------------------
   Tests
---------------------------------------------------------*/
#define Is_heap_val(v)  (Is_block(v) && (Is_young(v) || Is_in_heap(v)))


#endif /* heap_h */
