/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>
#include <string.h>
#include "config.h"
#include "mlvalues.h"
#include "alloc.h"
#include "memory.h"
#include "fail.h"
#include "module.h"
#include "signals.h"
#include "thread.h"

extern wsize_t stack_wsize_peak;
extern wsize_t stack_wsize_total;

/*----------------------------------------------------------------------
  thread states: a toplevel module, a stack and registers
----------------------------------------------------------------------*/
struct thread_state* threads = NULL;

struct thread_state* thread_new( unsigned long stack_init_wsize, unsigned long threshold_wsize
                                   , unsigned long stack_max_wsize, value module, value code )
{
  struct thread_state* thread;
  int i;

  /* alloc a thread */
  thread = (struct thread_state*) stat_alloc( sizeof(struct thread_state) );

  thread->next       = NULL;
  thread->result     = Thread_yield;
  thread->stack_max_wsize       = stack_max_wsize;
  thread->stack_threshold_wsize = threshold_wsize;

  thread->module     = module;
  thread->code = thread->code_exn = 0;
  thread->stack = thread->stack_lim = thread->stack_top = NULL;
  thread->stack_sp = thread->stack_fp = thread->exn_fp = NULL;
  thread->exn_frame  = NULL;
  for( i = 0; i < Sig_count; i++) thread->save_signals[i] = 0;

  thread->fp_sticky = 0;
  thread->fp_traps  = 0; /* no fp exception is trapped by default */

  /* insert into the threads list: this makes the module & stack visible to the GC */
  thread->next = threads;
  threads = thread;

  /* allocate a stack and put a stop frame on top */
  if (stack_init_wsize < 2*threshold_wsize) stack_init_wsize = 2*threshold_wsize;
  if (stack_init_wsize > stack_max_wsize)   stack_init_wsize = stack_max_wsize;

  thread->stack       = (value*) stat_alloc(Bsize_wsize(stack_init_wsize));
  stack_wsize_total += stack_init_wsize;
  if (stack_wsize_total > stack_wsize_peak) stack_wsize_peak = stack_wsize_total;

  thread->stack_top   = thread->stack + stack_init_wsize;
  thread->stack_lim   = thread->stack + thread->stack_threshold_wsize;
  thread->stack_sp    = thread->stack_top-1;
  thread->stack_sp[0] = frame_stop;
  thread->stack_fp    = thread->stack_sp;

  /* push the value to enter */
  thread->stack_sp--;
  thread->stack_sp[0] = code;

  /* if we deal with an IO value, execute it */
  /*
  if (strcmp(find_type_of_code(module,code),"IO ()") == 0) {
    value mainIO = find_qualified_code( module, "LvmLang", "unsafePerformStrictIO" );
    if (mainIO != 0) {
      thread->stack_sp--;
      thread->stack_sp[0] = mainIO;
    }
  }
  */

  return thread;
}

void thread_destroy( struct thread_state* thread )
{
   struct thread_state* prev;
   struct thread_state* next;

   /* exception handlers are stack alloc'd, no need to remove */
   /* remove any signal handlers */
   uninstall_signal_handler( thread );

   /* unlink the thread from the list */
   prev = NULL;
   for( prev = NULL, next = threads;
        next != NULL && next != thread;
        prev = next, next = next->next ) {}
   Assert( next != NULL ); if (next == NULL) return;

   if (prev) prev->next  = thread->next;
        else threads = thread->next;

   /* release the statically allocated memory */
   if (thread->stack) {
     stack_wsize_total -= (thread->stack_top - thread->stack);
     stat_free( thread->stack );
   }

   stat_free(thread);
   return;
}

void thread_grow_stack( struct thread_state* thread )
{
  wsize_t size;
  value*  new_stack;
  value*  new_top;
  value*  new_sp;

  Assert(thread);
  Assert(thread->stack_sp >= thread->stack);

  /* allocate the new stack */
  size = thread->stack_top - thread->stack;
  if (size >= thread->stack_max_wsize) {
    raise_stack_overflow(Bsize_wsize(thread->stack_max_wsize));
  }

  size *= 2;
  if (size > thread->stack_max_wsize) {
    size = thread->stack_max_wsize;
  }

  gc_message( 0x08, "Growing stack to %luk bytes\n",
                    Bsize_wsize(size) / Kilo);

  new_stack = stat_alloc( Bsize_wsize(size) );
  new_top   = new_stack + size;

  stack_wsize_total += (size / 2);
  if (stack_wsize_total > stack_wsize_peak) stack_wsize_peak = stack_wsize_total;


#define shift(p)     ((char *)new_top - (((char *)thread->stack_top) - (char *)(p)))

  /* copy the stack */
  new_sp    = (value*)shift( thread->stack_sp );
  bcopy( (char*) thread->stack_sp,
         (char*) new_sp,
         (thread->stack_top - thread->stack_sp) * sizeof(value) );
  stat_free( thread->stack );

  /* adjust the frame pointers */
  thread->stack_fp = (value*) shift(thread->stack_fp);

  /* jul 2001: the frame links are now relative :-) */
  /*
  for( fp = thread->stack_fp; Frame_frame(fp) != frame_stop; ) {
    fp = Frame_next(fp) = (value*)shift(Frame_next(fp));
  }
  */

  /* initialize the new values */
  thread->stack      = new_stack;
  thread->stack_top  = new_top;
  thread->stack_lim  = thread->stack + thread->stack_threshold_wsize;
  thread->stack_sp   = new_sp;

#undef shift
  return;
}

/*----------------------------------------------------------------------
  current thread
----------------------------------------------------------------------*/
static struct thread_state* _current_thread = NULL;

void set_current_thread( struct thread_state* thread )
{
  _current_thread = thread;
}

struct thread_state* get_current_thread( void )
{
  return _current_thread;
}


/*----------------------------------------------------------------------
  max stack size
----------------------------------------------------------------------*/
void thread_set_stack_max_wsize( struct thread_state* thread, unsigned long new_max_wsize )
{
  if (thread != NULL) {
    wsize_t wsize = thread->stack_top - thread->stack;
    if (wsize < new_max_wsize) {
      thread->stack_max_wsize = new_max_wsize;
    }
  }
  return;
}

unsigned long thread_get_stack_max_wsize( struct thread_state* thread )
{
  if (thread == NULL) return 0;
                else  return (thread->stack_max_wsize);
}
