\/**----------------------------------------------------------------------
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

#include <stdio.h>
#include "mlvalues.h"

#ifdef OS_WINDOWS
#include <windows.h>
#endif

#ifdef HAS_FLOAT_H
#ifdef __MINGW32__
#include <../mingw/float.h>
#else
#include <float.h>
#endif
#endif

#include "alloc.h"
#include "fail.h"
#include "thread.h"
#include "systhread.h"
#include "signals.h"


/*----------------------------------------------------------------------
  define signals
----------------------------------------------------------------------*/
#ifndef SIGABRT
#define SIGABRT -1
#endif
#ifndef SIGALRM
#define SIGALRM -1
#endif
#ifndef SIGFPE
#define SIGFPE -1
#endif
#ifndef SIGHUP
#define SIGHUP -1
#endif
#ifndef SIGILL
#define SIGILL -1
#endif
#ifndef SIGINT
#define SIGINT -1
#endif
#ifndef SIGKILL
#define SIGKILL -1
#endif
#ifndef SIGPIPE
#define SIGPIPE -1
#endif
#ifndef SIGQUIT
#define SIGQUIT -1
#endif
#ifndef SIGSEGV
#define SIGSEGV -1
#endif
#ifndef SIGTERM
#define SIGTERM -1
#endif
#ifndef SIGUSR1
#define SIGUSR1 -1
#endif
#ifndef SIGUSR2
#define SIGUSR2 -1
#endif
#ifndef SIGCHLD
#define SIGCHLD -1
#endif
#ifndef SIGCONT
#define SIGCONT -1
#endif
#ifndef SIGSTOP
#define SIGSTOP -1
#endif
#ifndef SIGTSTP
#define SIGTSTP -1
#endif
#ifndef SIGTTIN
#define SIGTTIN -1
#endif
#ifndef SIGTTOU
#define SIGTTOU -1
#endif
#ifndef SIGVTALRM
#define SIGVTALRM -1
#endif
#ifndef SIGPROF
#define SIGPROF -1
#endif
#ifndef SIGBREAK
#define SIGBREAK SIGINT
#endif

/*----------------------------------------------------------------------
   conversion to POSIX signals
----------------------------------------------------------------------*/
/* was used from set_signal_handler */
/*
static int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF
};

static int convert_posix_signal(int signo)
{
  if (signo < 0 && (-signo) <= (sizeof(posix_signals) / sizeof(int)))
    return posix_signals[-signo-1];
  else
    return signo;
}
*/

/*----------------------------------------------------------------------
   conversion signal_t / int signo
----------------------------------------------------------------------*/
static int system_signals[Sig_count] = {
  -1, -1, -1, -1,
  SIGINT, SIGBREAK, SIGFPE, SIGSEGV, SIGILL, SIGABRT,
  SIGTERM, SIGKILL, SIGQUIT, SIGALRM, SIGVTALRM, SIGTTIN, SIGTTOU,
  SIGCONT, SIGHUP, SIGPIPE, SIGCHLD, SIGSTOP, SIGPROF, SIGUSR1, SIGUSR2
};

static int convert_to_system_signal( enum signal_t sig )
{
  if (sig > 0 && sig < Sig_count)
    return system_signals[sig];
  else
    return -1;
}

static enum signal_t convert_to_custom_signal( int signo )
{
  enum signal_t sig;
  for( sig = 0; sig < Sig_count; sig++) {
    if (system_signals[sig] == signo) break;
  }

  if (sig == Sig_count)
    return Sig_none;
  else
    return sig;
}



/*----------------------------------------------------------------------
   portable "signal" function
----------------------------------------------------------------------*/
typedef void (*sighandler_t)(int signo);

static sighandler_t set_signal_handler( enum signal_t sig, sighandler_t act )
{
  int signo;
  sighandler_t oldact;
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#endif

  signo = convert_to_system_signal( sig );
  if (signo == -1) raise_user("set_signal_handler: could not install handler for signal %i", sig );
  /* signo = convert_posix_signal(signo); */

#ifdef POSIX_SIGNALS
  sigact.sa_handler = act;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  if (sigaction(signo, &sigact, &oldsigact) == -1) raise_user("signal: couldn't install signal handler for signal %i", signo );
  oldact = oldsigact.sa_handler;
#else
#if defined(OS_WINDOWS)
  if (signo == SIGINT || signo == SIGBREAK || signo == SIGTERM) /* handled by ctrl_handler */
    oldact = SIG_DFL;
  else
    oldact = signal(signo,act);
#else
  oldact = signal(signo, act);
#endif
  if (oldact == SIG_ERR) raise_user("set_signal_handler: could not install handler for signal %i", sig );
#endif

  return oldact;
}

/*----------------------------------------------------------------------
  signal handlers
----------------------------------------------------------------------*/
static struct thread_state* signal_handlers[Sig_count];

#ifdef POSIX_SIGNALS
static sigset_t signals_installed;
static sigset_t signals_saved;
#endif

static void block_signals(void)
{
#ifdef POSIX_SIGNALS
  sigprocmask(SIG_SETMASK, &signals_installed, &signals_saved);
#endif
}

static void unblock_signals(void)
{
#ifdef POSIX_SIGNALS
  sigprocmask(SIG_SETMASK, &signals_saved, NULL);
#endif
}

/*----------------------------------------------------------------------
  signal queue
----------------------------------------------------------------------*/
int  volatile force_major_slice = 0;
bool volatile _pending_signal   = false;

#define MAXPENDING 32   /* at least 1 */
static enum signal_t signal_queue[MAXPENDING];
static volatile int  qhead = -1;
static volatile int  qtail = -1;
static mutex         qmutex;

#define Qinc(i)   ((i) + 1 % MAXPENDING)

void push_pending_signal( enum signal_t sig )
{
  mutex_lock( &qmutex );
  {
    if (qhead < 0) {
      /* buffer is empty */
      qhead = qtail = 0;
      signal_queue[qtail] = sig;
    }
    else if (Qinc(qtail) == qhead) {
      /* buffer is full */
      signal_queue[qtail] = Sig_lost;  /* overwrite last signal */
    }
    else {
      /* buffer has still room */
      qtail = Qinc(qtail);
      signal_queue[qtail] = sig;
    }
    if (sig == Sig_gc)  force_major_slice = 1;
    _pending_signal = true;
  }
  mutex_unlock( &qmutex );
}

enum signal_t pop_pending_signal(void)
{
  int sig;
  mutex_lock( &qmutex );
  block_signals();
  {
    if (qhead < 0) {
      /* buffer is empty */
      sig = Sig_none;
    }
    else {
      sig = signal_queue[qhead];
      if (qhead == qtail) {
        /* no more elements */
        qhead = qtail = -1;
        _pending_signal = false;
      }
      else {
        /* pop 1 element */
        qhead = Qinc(qhead);
      }
    }
  }
  unblock_signals();
  mutex_unlock( &qmutex );
  return sig;
}


/*----------------------------------------------------------------------
  The signal handler
----------------------------------------------------------------------*/
static void handle_signal(int signo )
{
  enum signal_t sig;
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
  signal(signo, handle_signal);   /* reinstall the handler */
#endif

  sig = convert_to_custom_signal(signo);
  if (sig != Sig_none) push_pending_signal( sig );
}

void urge_major_slice (void)
{
  push_pending_signal( Sig_gc );
}

/*----------------------------------------------------------------------
   a thread can be installed as a signal handler.
   the scheduler raises the signal in the associated thread.
   the user uses "catch" to catch the signals.
----------------------------------------------------------------------*/
void uninstall_signal_handler( struct thread_state* thread )
{
  enum signal_t sig;
  if (thread == NULL) return;

  for (sig = 0; sig < Sig_count; sig++) {
    if (thread == signal_handlers[sig]) {
      /* install previous handler again */
      value oldact = thread->save_signals[sig];
      if (Is_long(oldact))
        set_signal_handler( sig, (sighandler_t)(Long_val(oldact)) );
      else if (oldact != 0)
        set_signal_handler( sig, (sighandler_t)oldact );

      signal_handlers[sig] = NULL;
      #ifdef POSIX_SIGNALS
      sigdelset( &signals_installed, convert_to_system_signal(sig) );
      #endif
    }
  }
}

void install_signal_handler(enum signal_t sig, struct thread_state* thread)
{
  struct thread_state* old_handler;
  sighandler_t         old_act;

  /* no fpe allowed */
  if (sig == Sig_fpe)
     raise_internal( "signal handlers for SIGFPE are not allowed" );

  /* check if not yet installed */
  old_handler = signal_handlers[sig];
  if (old_handler != NULL) {
    if (old_handler == thread)
      return;   /* already installed */
    else
      raise_user("signal: multiple threads try to capture the same signal (%i)", sig);
  }

  /* install the handler */
  old_act = set_signal_handler( sig, handle_signal );
  signal_handlers[sig] = thread;

  /* save the previous handler */
  if (old_act == SIG_IGN || old_act == SIG_DFL)
    thread->save_signals[sig] = Val_int(old_act);
  else
    thread->save_signals[sig] = (value)old_act;

  return;
}

/*----------------------------------------------------------------------
  floating point signals are synchronous -- we use longjmp to escape
  Since the signal SIG_FPE carries too little information, we try
  to use system dependent functions in order to access the extra
  floating point error code.
----------------------------------------------------------------------*/
static sighandler_t oldfpe = SIG_DFL;

struct fpe_info {
  int                 syserr;
  enum exn_arithmetic err;
};


/* floating point exceptions are synchronous */
#if defined(OS_WINDOWS)
static struct fpe_info  fpe_table[] = {
  { _FPE_INVALID            , Fpe_invalid },
  { _FPE_DENORMAL           , Fpe_denormal },
  { _FPE_ZERODIVIDE         , Fpe_zerodivide },
  { _FPE_OVERFLOW           , Fpe_overflow },
  { _FPE_UNDERFLOW          , Fpe_underflow },
  { _FPE_INEXACT            , Fpe_inexact },

  { _FPE_UNEMULATED         , Fpe_unemulated },
  { _FPE_SQRTNEG            , Fpe_sqrtneg },
  { _FPE_STACKOVERFLOW      , Fpe_stackoverflow },
  { _FPE_STACKUNDERFLOW     , Fpe_stackunderflow },

  { _FPE_EXPLICITGEN        , Fpe_invalid },
  { -1                      , Fpe_invalid }
};

static void handle_signal_fpe( int sig, int syserr )
{
  enum exn_arithmetic err = Fpe_invalid;
  struct fpe_info* info;
  for( info = fpe_table; info->syserr != -1; info++ ) {
    if (info->syserr == syserr) { err = info->err; break; }
  }
  _fpreset();
  raise_arithmetic_exn( err );
}

static void init_fpe_handler( void )
{
  oldfpe = signal( SIGFPE, (sighandler_t)(handle_signal_fpe) );  
}

static void done_fpe_handler( void )
{
  signal( SIGFPE, oldfpe );
  oldfpe = SIG_DFL;
}

/* POSIX */
#elif defined(POSIX_SIGNALS)
static struct fpe_info  fpe_table[] = {
  { FPE_FLTINV, Fpe_invalid },
  { FPE_FLTDIV, Fpe_zerodivide },
  { FPE_FLTOVF, Fpe_overflow },
  { FPE_FLTUND, Fpe_underflow },
  { FPE_FLTRES, Fpe_inexact },
  { -1        , Fpe_invalid }
};

static void handle_signal_fpe( int sig, siginfo_t* siginfo, void* p)
{
  enum exn_arithmetic err = Fpe_invalid;
  struct fpe_info* info;
  for( info = fpe_table; info->syserr != -1; info++ ) {
    if (info->syserr == siginfo->si_code) { err = info->err; break; }
  }
  raise_arithmetic_exn( err );
}

static void init_fpe_handler( void )
{
  struct sigaction sigact, oldsigact;

  sigact.sa_handler   = NULL;
  sigact.sa_sigaction = handle_signal_fpe;
  sigact.sa_flags     = SA_SIGINFO;
  sigemptyset(&sigact.sa_mask);
  if (sigaction(SIGFPE, &sigact, &oldsigact) == -1) 
    raise_user("signal: couldn't install floating point signal handler");
  oldfpe = oldsigact.sa_handler;
  sigaddset(&signals_installed, SIGFPE );
}

static void done_fpe_handler( void )
{
  set_signal_handler( Sig_fpe, oldfpe );
  sigdelset(&signals_installed, SIGFPE );
  oldfpe = SIG_DFL;
}

/* other systems */
#else
void handle_signal_fpe( int sig )
{
  raise_arithmetic_exn( Fpe_invalid );
}

void init_fpe_handler( void )
{
  oldfpe = set_signal_handler( Sig_fpe, (sighandler_t)handle_signal_fpe );
}

void done_fpe_handler( void )
{
  set_signal_handler( Sig_fpe, oldfpe );
  oldfpe = SIG_DFL;
}
#endif


/*----------------------------------------------------------------------
  On windows, we use SetConsoleCtrlHandler for certain signals.
  This handler runs in its own thread so [push_pending_signal] has
  to be multi-thread resistant :-)
  The threading enables us for example to handle application shutdown
  events properly, ie. the program has about 2.5 secs to perform
  a decent shutdown.
----------------------------------------------------------------------*/
#if defined(OS_WINDOWS)
BOOL WINAPI ctrl_handler( DWORD type )
{
  switch (type) {
  case CTRL_C_EVENT:
    push_pending_signal( Sig_int );
    break;
  case CTRL_BREAK_EVENT:
    push_pending_signal( Sig_break );
    break;
  case CTRL_CLOSE_EVENT:
  case CTRL_SHUTDOWN_EVENT:
    push_pending_signal( Sig_term );
    Sleep(2500);  /* give a chance to handle the termination */
    break;
  default:
    return FALSE;
  }

  return TRUE; /* handled */
}
#endif



/*----------------------------------------------------------------------
  init/done
----------------------------------------------------------------------*/
static bool signals_initialised = false;

void init_signals(void)
{
  enum signal_t sig;
  if (signals_initialised) return;

  for( sig = Sig_none; sig < Sig_count; sig++) signal_handlers[sig] = NULL;
#ifdef POSIX_SIGNALS
  sigemptyset(&signals_installed);
#endif
  mutex_init( &qmutex );

  /* set fpe handler */
  init_fpe_handler();

  /* set windows console handler */
#if defined(OS_WINDOWS)
  SetConsoleCtrlHandler(ctrl_handler,TRUE);
#endif

  signals_initialised = true;
  return;
}

void done_signals(void)
{
  enum signal_t sig;
  if (!signals_initialised) return;

  /* uninstall signal handlers */
  for( sig = 0; sig < Sig_count; sig++) {
    struct thread_state* thread = signal_handlers[sig];
    if (thread) {
      if (Is_long(thread->save_signals[sig]))
        set_signal_handler( sig, (sighandler_t)Long_val(thread->save_signals[sig]) );
      else
        set_signal_handler( sig, (sighandler_t)thread->save_signals[sig] );
    }
  }

  done_fpe_handler();
#ifdef POSIX_SIGNALS
  sigemptyset(&signals_installed);
#endif

  mutex_done( &qmutex );
}



/*----------------------------------------------------------------------
  Invoke a signal handler
----------------------------------------------------------------------*/
value alloc_signal_exception( enum signal_t sig, struct thread_state* thread )
{
  CAMLparam0();
  CAMLlocal2(_raise,exn);

  exn = alloc( 1, Exn_async_signal );

  switch (sig) {
  case Sig_fpe:  {
    Store_field(exn,Field_exn_val1,Val_int(0));
    break;
  }
  default: {
    Store_field(exn,Field_exn_val1,Val_int(sig));
    break;
  }}

  _raise = alloc( 1, Raise_tag );
  Store_field(_raise,0,exn);

  CAMLreturn(_raise);
}

void schedule_signal_handler( enum signal_t sig )
{
  struct thread_state* thread;
  if (sig < 0 || sig >= Sig_count) return;

  thread = signal_handlers[sig];
  if (thread == NULL) return;

  if (thread->result != Thread_yield) {
    /* the thread is already done -- ignore the signal? */
    /* push_pending_signal( Sig_lost ); */  /* this can loop! */
  }
  else {
    /* push a "raise exception" value on the stack */
    thread->stack_sp[-1] = alloc_signal_exception( sig, thread );
    thread->stack_sp--;
  }
  return;
}


struct sig_info
{ enum signal_t sig;
  const char*   desc;
};

static struct sig_info signal_infos[] =
{ { Sig_gc,     "garbage collection needed" }
, { Sig_yield,  "thread should yield" }
, { Sig_lost,   "signal(s) lost" }
, { Sig_int,    "interactive interrupt (ctrl-c)" }
, { Sig_break,  "interactive stop (ctrl-break)" }
, { Sig_fpe,    "floating point exception" }
, { Sig_segv,   "invalid memory reference" }
, { Sig_ill,    "illegal hardware instruction" }
, { Sig_abrt,   "abnormal termination" }
, { Sig_term,   "unexpected termination" }
, { Sig_kill,   "forced termination" }
, { Sig_quit,   "interactive termination" }
, { Sig_alrm,   "timeout" }
, { Sig_vtalrm, "timeout in virtual time" }
, { Sig_ttin,   "terminal read from background process" }
, { Sig_ttout,  "terminal write from background process" }
, { Sig_cont,   "continue process" }
, { Sig_hup,    "lost connection" }
, { Sig_pipe,   "broken pipe" }
, { Sig_chld,   "child process terminated" }
, { Sig_stop,   "process stopped" }
, { Sig_prof,   "profiling interrupt" }
, { Sig_usr1,   "user signal 1" }
, { Sig_usr2,   "user signal 2" }
, { -1,  NULL }
};


const char* signal_description( enum signal_t sig )
{
  static char buf[MAXSTR];
  struct sig_info* info;
  for (info = signal_infos; info->desc != NULL; info++)
  {
    if (info->sig == sig) return info->desc;
  }

  snprintf( buf, MAXSTR, "system signal %i", convert_to_system_signal(sig) );
  return buf;
}
