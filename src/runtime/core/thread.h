/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _thread_
#define _thread_

struct thread_state;  /* forward declaration */
#include "signals.h"  /* just for [Sig_count] */

/*----------------------------------------------------------------------
   Thread states
----------------------------------------------------------------------*/
enum thread_result {
  Thread_yield,
  Thread_complete,
  Thread_exception
};

#define Runnable(t)   ((t)->result == Thread_yield || (t)->result == Thread_raise)

struct thread_state {
  struct thread_state* next;
  enum thread_result   result;

  wsize_t stack_max_wsize;
  wsize_t stack_threshold_wsize;
  value   code;
  value   code_exn;
  value   module;

  value*  stack;
  value*  stack_top;
  value*  stack_lim;
  value*  stack_sp;
  value*  stack_fp;
  value*  exn_fp;

  /* trapped signals & exception frames */
  value   save_signals[Sig_count];
  struct  exception_frame* exn_frame;

  /* floating point state */
  long    fp_sticky;
  long    fp_traps;
};

/*----------------------------------------------------------------------
   The list of all threads:
   used by the GC as roots. (note that thread_state's are stat_alloc'd)
----------------------------------------------------------------------*/
extern struct thread_state*  threads;

/*----------------------------------------------------------------------
   Functions on threads
----------------------------------------------------------------------*/
struct thread_state* thread_new( unsigned long stack_init_wsize, unsigned long threshold_wsize
                                   , unsigned long stack_max_wsize, value module, value code );
void                 thread_destroy( struct thread_state* thread );
void                 thread_grow_stack( struct thread_state* thread );

void                 thread_set_stack_max_wsize( struct thread_state* thread, unsigned long new_max_wsize );
unsigned long        thread_get_stack_max_wsize( struct thread_state* thread );


void                 set_current_thread( struct thread_state* thread );
struct thread_state* get_current_thread( void );




/*----------------------------------------------------------------------
 frame pointers -- so we can walk the stack of a thread
 this assumes that there are no heap pointers with these values!
 (we might consider using even numbers but that might interfere with the compacter)
----------------------------------------------------------------------*/
#define frame_cont    ((value)(4))
#define frame_update  ((value)(8))
#define frame_catch   ((value)(12))
#define frame_stop    ((value)(16))

#define Frame_frame(fp)   (fp[0])
#define Frame_next(fp)    (fp + Long_val(fp[1]))
#define Frame_value(fp)   (fp[2])
#define Frame_size        3

/* suspension fields */
enum susp_field {
  Field_susp_base,
  Field_susp_top,
  Susp_info_wosize
};


#endif /* _thread_ */