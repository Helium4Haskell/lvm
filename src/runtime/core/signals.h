/**----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. This file is distributed under the terms
  of the GNU Library General Public License. This file is based on the
  original Objective Caml source copyrighted by INRIA Rocquencourt.
----------------------------------------------------------------------**/

/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _signals_
#define _signals_

/*----------------------------------------------------------------------
  custom signals
----------------------------------------------------------------------*/
enum signal_t {
  Sig_none = 0,       /* no signal */
  Sig_gc,             /* garbage collection needed */
  Sig_yield,          /* time's up, yield soon */

  Sig_lost,           /* lost signal */
  Sig_int,
  Sig_break,
  Sig_tstp = Sig_break,
  Sig_fpe,
  Sig_segv,
  Sig_ill,
  Sig_abrt,
  Sig_term,
  Sig_kill,
  Sig_quit,
  Sig_alrm,
  Sig_vtalrm,
  Sig_ttin,
  Sig_ttout,
  Sig_cont,
  Sig_hup,
  Sig_pipe,
  Sig_chld,
  Sig_stop,
  Sig_prof,
  Sig_usr1,
  Sig_usr2,
  Sig_count,          /* maximal signal */

  /* human readable names :-) */
  Sig_background_read         = Sig_ttin,
  Sig_background_write        = Sig_ttout,
  Sig_continue_process        = Sig_cont,
  Sig_floatingpoint_exception = Sig_fpe,
  Sig_illegal_instruction     = Sig_ill,
  Sig_internal_abort          = Sig_abrt,
  Sig_keyboard_signal         = Sig_int,
  Sig_keyboard_stop           = Sig_tstp,
  Sig_keyboard_terminate      = Sig_quit,
  Sig_kill_process            = Sig_kill,
  Sig_lost_connection         = Sig_hup,
  Sig_open_ended_pipe         = Sig_pipe,
  Sig_process_status_changed  = Sig_chld,
  Sig_real_time_alarm         = Sig_alrm,
  Sig_segmentation_violation  = Sig_segv,
  Sig_software_stop           = Sig_stop,
  Sig_software_termination    = Sig_term,
  Sig_user_signal_1           = Sig_usr1,
  Sig_user_signal_2           = Sig_usr2
};

const char* signal_description( enum signal_t sig );

#include "mlvalues.h"
#include "thread.h"


/*----------------------------------------------------------------------
  GC
----------------------------------------------------------------------*/
extern int volatile force_major_slice;
void urge_major_slice (void);


/*----------------------------------------------------------------------
  signal handling
----------------------------------------------------------------------*/
extern bool volatile _pending_signal;       /* hidden */
#define pending_signal()  _pending_signal

void          push_pending_signal( enum signal_t sig );
enum signal_t pop_pending_signal( void );

void   schedule_signal_handler( enum signal_t sig );
void   init_signals(void);
void   done_signals(void);

void   install_signal_handler( enum signal_t sig, struct thread_state* thread );
void   uninstall_signal_handler( struct thread_state* thread );
struct thread_state* get_signal_handler( enum signal_t sig );


#endif /* _signals_ */