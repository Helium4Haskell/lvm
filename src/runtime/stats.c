/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <string.h>
#include "mlvalues.h"
#include "sys.h"
#include "gc_ctrl.h"
#include "memory.h"
#include "print.h"
#include "stats.h"

static nat ticks_total, ticks_user, ticks_system;
static nat ticks_init, ticks_init_system, ticks_init_user;
static nat ticks_done, ticks_done_system, ticks_done_user;
static nat ticks_gc,   ticks_gc_system, ticks_gc_user;

void stat_start_init(void)
{
  ticks_init = ticks_init_system = ticks_init_user = 0;
  ticks_done = ticks_done_system = ticks_done_user = 0;
  ticks_gc   = ticks_gc_system   = ticks_gc_user   = 0;
  get_process_ticks( &ticks_init, &ticks_init_user, &ticks_init_system );
}

void stat_end_init(void)
{
  nat total, user, system;
  get_process_ticks( &total, &user, &system );
  ticks_init        = total  - ticks_init;
  ticks_init_system = system - ticks_init_system;
  ticks_init_user   = user   - ticks_init_user;
}

void stat_start_done(void)
{
  get_process_ticks( &ticks_done, &ticks_done_user, &ticks_done_system );
}

void stat_end_done(void)
{
  get_process_ticks( &ticks_total, &ticks_user, &ticks_system );
  ticks_done        = ticks_total  - ticks_done;
  ticks_done_system = ticks_system - ticks_done_system;
  ticks_done_user   = ticks_user   - ticks_done_user;
}


static nat _ticks_gc, _ticks_gc_system, _ticks_gc_user;

void stat_start_gc(void)
{
  get_process_ticks( &_ticks_gc, &_ticks_gc_user, &_ticks_gc_system );
}

void stat_end_gc(void)
{
  nat total,system,user;
  get_process_ticks( &total, &user, &system );
  ticks_gc        += (total - _ticks_gc);
  ticks_gc_system += (system - _ticks_gc_system );
  ticks_gc_user   += (user - _ticks_gc_user );
}



void stat_timings_report(void)
{
#define TICKS(t) (msecs_of_ticks(t) / 1000), (msecs_of_ticks(t) % 1000)
  nat ticks_eval, ticks_eval_user, ticks_eval_system;
  ticks_eval        = ticks_total - ticks_gc - ticks_init - ticks_done;
  ticks_eval_user   = ticks_user - ticks_gc_user - ticks_init_user - ticks_done_user;
  ticks_eval_system = ticks_system - ticks_gc_system - ticks_init_system - ticks_done_system;

  error_message( "\n" );
  error_message( "timings:\n" );
  error_message( " process total:       %3li.%03lis   (user: %3li.%03lis  system: %3li.%03lis)\n",
                                        TICKS(ticks_total), TICKS(ticks_user), TICKS(ticks_system) );
  error_message( "  evaluator:          %3li.%03lis   (user: %3li.%03lis  system: %3li.%03lis)\n",
                                        TICKS(ticks_eval), TICKS(ticks_eval_user), TICKS(ticks_eval_system) );

  error_message( "  garbage collector:  %3li.%03lis   (user: %3li.%03lis  system: %3li.%03lis)\n",
                                        TICKS(ticks_gc), TICKS(ticks_gc_user), TICKS(ticks_gc_system) );
  error_message( "  runtime manager:    %3li.%03lis   (user: %3li.%03lis  system: %3li.%03lis)\n",
                                        TICKS(ticks_init + ticks_done), TICKS(ticks_init_user+ticks_done_user), TICKS(ticks_init_system + ticks_done_system) );
  error_message( " effective time:      %3li%%\n",  (ticks_total > 0 ? ((100* (ticks_total - ticks_gc - ticks_init - ticks_done)) / ticks_total) : 100) );
}

void stat_heap_report(void)
{
extern wsize_t stack_wsize_peak;
  wsize_t major_alloced = stat_major_words + allocated_words;
  wsize_t minor_alloced = stat_minor_words + Wsize_bsize(young_end - young_ptr);
  char minor_size_str[100];
  strcpy(minor_size_str,Bstring_of_bsize(minor_heap_size));

  error_message( "\n" );
  error_message( "heap statistics:\n" );
  error_message( " peak heap size:      %4s   (+minor %s)\n",  Bstring_of_bsize(stat_peak_heap_bsize), minor_size_str);
  error_message( " peak stack(s) size:  %4s\n",  Bstring_of_wsize(stack_wsize_peak));
  error_message( " total allocated:     %4s\n",  Bstring_of_wsize(minor_alloced + major_alloced - stat_promoted_words) );
  error_message( "  minor allocated:    %4s\n",  Bstring_of_wsize(minor_alloced) );
  error_message( "  major allocated:    %4s\n",  Bstring_of_wsize(major_alloced) );
  error_message( "  promoted:           %4s\n",  Bstring_of_wsize(stat_promoted_words) );
  error_message( " minor collections:   %4li\n", stat_minor_collections );
  error_message( " major collections:   %4li\n", stat_major_collections );
  error_message( " compactions:         %4li\n", stat_compactions );
}
