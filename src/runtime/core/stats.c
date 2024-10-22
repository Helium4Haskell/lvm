/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <string.h>
#include "mlvalues.h"
#include "heap/heap.h"
#include "sys.h"
#include "print.h"
#include "options.h"
#include "stats.h"

#define Stat_counter(name)  \
  static nat _ticks_##name, _ticks_##name##_system, _ticks_##name##_user; \
  static nat ticks_##name = 0, ticks_##name##_system = 0, ticks_##name##_user = 0; \
  \
  void stat_start_##name(void) { \
    get_process_ticks( &_ticks_##name, &_ticks_##name##_user, &_ticks_##name##_system ); \
  } \
  \
  void stat_end_##name(void) { \
    nat total,system,user; \
    get_process_ticks( &total, &user, &system ); \
    ticks_##name        += (total - _ticks_##name); \
    ticks_##name##_system += (system - _ticks_##name##_system ); \
    ticks_##name##_user   += (user - _ticks_##name##_user ); \
  }

Stat_counter(gc)
Stat_counter(init)
Stat_counter(done)
Stat_counter(total)

void stat_timings_report(void)
{
#define TICKS(t) (msecs_of_ticks(t) / 1000), (msecs_of_ticks(t) % 1000)
  nat ticks_eval, ticks_eval_user, ticks_eval_system;
  ticks_eval        = ticks_total - ticks_gc - ticks_init - ticks_done;
  ticks_eval_user   = ticks_total_user - ticks_gc_user - ticks_init_user - ticks_done_user;
  ticks_eval_system = ticks_total_system - ticks_gc_system - ticks_init_system - ticks_done_system;

  error_message( "\n" );
  error_message( "timings:\n" );
  error_message( " process total:       %3li.%03lis   (user: %3li.%03lis  system: %3li.%03lis)\n",
                                        TICKS(ticks_total), TICKS(ticks_total_user), TICKS(ticks_total_system) );
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
