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
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _failexn_h
#define _failexn_h

#include <setjmp.h>
#include <signal.h>
#include "mlvalues.h"
#include "heap/heap.h"

/*----------------------------------------------------------------------
 exception handling
----------------------------------------------------------------------*/
#ifdef POSIX_SIGNALS
struct longjmp_buffer {
  sigjmp_buf buf;
};
#else
struct longjmp_buffer {
  jmp_buf buf;
};
#define sigsetjmp(buf,save) setjmp(buf)
#define siglongjmp(buf,val) longjmp(buf,val)
#endif


struct exception_frame {
  struct exception_frame*   _prev;
  struct caml__roots_block* _local_roots;
  struct longjmp_buffer     _jmp;
  value                     _exn;
};

#define Setup_exception_handler(frame,thread,exn,handler) \
                 { if (thread != NULL) { \
                      frame._prev = thread->exn_frame; \
                      thread->exn_frame = &frame; \
                   } else { \
                      frame._prev = global_exn_frame; \
                      global_exn_frame  = &frame;     \
                   } \
                   frame._exn = 0; \
                   frame._local_roots = local_roots;   \
                   if (sigsetjmp(frame._jmp.buf, 0)) { \
                       exn = frame._exn; \
                      local_roots = frame._local_roots; \
                      handler; \
                   } \
                 }

#define Restore_exception_handler(frame,thread) \
                 { if (thread != NULL) { \
                      thread->exn_frame = frame._prev; \
                   } else { \
                      global_exn_frame = frame._prev; \
                   } \
                 }

struct exception_frame* global_exn_frame;


#endif /* _failexn_h */