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

#include "print.h"
#include "fail.h"
#include "thread.h"
#include "signal.h"
#include "evaluator.h"


/*----------------------------------------------------------------------
  simple toplevel evaluation function
----------------------------------------------------------------------*/
static void schedule( struct thread_state* thread )
{
  set_current_thread( thread );
  install_signal_handler( Sig_int,  thread );
  install_signal_handler( Sig_break,thread );
  install_signal_handler( Sig_term, thread );
  install_signal_handler( Sig_segv, thread );
  install_signal_handler( Sig_ill, thread );

  while (1) {
    while (pending_signal()) {
      enum signal_t sig = pop_pending_signal();
      switch(sig) {
        case Sig_none:
        case Sig_yield:
          break;
        case Sig_gc:
          minor_collection();
          break;
        case Sig_lost:
        default: {
          schedule_signal_handler( sig );
        }
      }
    } /* while pending signal */

    switch(thread->result) {
      case Thread_yield:
        evaluate(thread);
        break;
      case Thread_complete:
        set_current_thread(NULL);
        return;
      case Thread_exception:
        fatal_uncaught_exception( thread->stack_sp[0] );
        return;
    }
  } /* while(1) */
}

void evaluate_name( value module, char* name )
{
  CAMLparam1(module);
  CAMLlocal1(v);

  struct thread_state* thread;
  value code = find_code( module, name );

extern wsize_t stack_wsize_max, stack_wsize_threshold, stack_wsize_init;

  if (code == 0) {
    fatal_error( "fatal error: function \"%s\" is not defined\n", name );
    CAMLreturn0;
  }

  thread = thread_create( stack_wsize_init, stack_wsize_threshold, stack_wsize_max, module, code );
  schedule( thread );

  switch (thread->result) {
  case Thread_exception: {
    fatal_uncaught_exception( thread->stack_sp[0] );
    break;
  }
  case Thread_complete:
  case Thread_yield:
  default: {
    print( "final value: " );
    print_value(module, thread->stack_sp[0]);
    print( "\n");
    break;
  }}

  debug_gc();
  thread_destroy(thread);
  CAMLreturn0;
}