/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _stats_h
#define _stats_h

void stat_start_total(void);
void stat_end_total(void);

void stat_start_init(void);
void stat_end_init(void);

void stat_start_done(void);
void stat_end_done(void);

void stat_start_gc(void);
void stat_end_gc(void);

void stat_timings_report(void);
void stat_heap_report(void);


#endif