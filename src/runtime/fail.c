/**----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. This file is distributed under the terms
  of the GNU Library General Public License. This file is based on the
  original Objective Caml source copyrighted by INRIA Rocquencourt.
----------------------------------------------------------------------**/

/* $Id$ */

#include <stdio.h>      /* snprintf */
#include "mlvalues.h"
#include "memory.h"
#include "alloc.h"
#include "fail.h"
#include "print.h"    /* Bstring_of_bsize */

static void raise_exception( value exn ) Noreturn;
static void raise_exn_1( enum exn_tag tag, value v ) Noreturn;

static void raise_indirect_exn( enum exn_tag tag, int indtag ) Noreturn;
static void raise_indirect_exn_1( enum exn_tag tag, int indtag, value v ) Noreturn;
static void raise_indirect_exn_2( enum exn_tag tag, int indtag, value v, value w ) Noreturn;
static void raise_system_exn( enum exn_system tag ) Noreturn;

/*----------------------------------------------------------------------
  print an exception
----------------------------------------------------------------------*/
struct exn_info {
  con_tag_t tag;
  long      arg_count;
  char*     msg;
};

static struct exn_info exn_infos[] = {
  { Exn_async_heap_overflow,    0, "heap overflow" },
  { Exn_async_stack_overflow,   1, "stack overflow (%s)" },
  { Exn_async_signal,           1, "signal received: %s" },
  { Exn_invalid_arg,      1, "invalid argument: %s" },
  { Exn_assert,           1, "assertion failed: %s" },
  { Exn_not_found,        0, "object not found" },
  { Exn_user,             1, "%s" },
  { -1, 0, "unknown exception" }
};

static struct exn_info exn_runtime_infos[] = {
  { Exn_failed_pattern,   1, "pattern match failed at %s" },
  { Exn_blackhole,        1, "infinite value recursion at %s" },
  { Exn_out_of_bounds,    1, "bounds check failed at %s" },
  { Exn_exit,             1, "exit %i" },
  { Exn_invalid_opcode,   1, "invalid opcode (%li)" },
  { Exn_load_error,       2, "could not load \"%s\":\n  %s" },
  { Exn_runtime_error,    1, "runtime error: %s" },
  { -1, 0, "unknown runtime exception" }
};

static struct exn_info exn_system_infos[] = {
  { Exn_eof,              0, "end of file reached" },
  { Exn_system_blocked_io,0, "reading on blocked I/O channel" },
  { Exn_system_error,     2, "system error (%li): %s" },
  { -1, 0, "unknown system exception" }
};

static struct exn_info exn_arith_infos[] = {
  { Int_zerodivide,       0, "integer division by zero" },
  { Int_overflow,         0, "integer overflow" },
  { Int_underflow,        0, "integer underflow" },
  { Fpe_invalid,          0, "invalid numeric operation" },
  { Fpe_zerodivide,       0, "floating point division by zero" },
  { Fpe_overflow,         0, "floating point overflow" },
  { Fpe_underflow,        0, "floating point underflow" },
  { Fpe_inexact,          0, "numeric result is inexact" },
  { Fpe_unemulated,       0, "numeric operation can not be emulated" },
  { Fpe_sqrtneg,          0, "square root of a negative number" },
  { Fpe_overflow,         0, "floating point hardware stack overflow"  },
  { Fpe_underflow,        0, "floating point hardware stack underflow" },
  { -1, 0, "unknown arithmetic exception" }
};

bool is_async_exception( enum exn_tag tag )
{
  return (tag == Exn_async_heap_overflow ||
          tag == Exn_async_stack_overflow ||
          tag == Exn_async_signal);
}

#define Exn_field(exn,i)  (Is_long(Field(exn,i)) ? Long_val(Field(exn,i)) : (long)String_val(Field(exn,i)))

void fatal_uncaught_exception( value exn )
{
  char   buf[MAXSTR];
  const char*  prefix = "exception";
  const char*  msg    = NULL;
  const char*  inside = NULL;
  struct thread_state* thread;
  wsize_t arg_count;

  value  subexn = exn;

  /* find exception information */
  if (Is_block(exn)) {
    struct exn_info* info;
    int subtag;

    switch(Tag_val(exn)) {
    case Exn_runtime:     info = exn_runtime_infos; subexn = Field(exn,0); break;
    case Exn_arithmetic:  info = exn_arith_infos;   subexn = Field(exn,0); break;
    case Exn_system:      info = exn_system_infos;  subexn = Field(exn,0); break;
    default:              info = exn_infos;         break;
    }
    subtag = Tag_val(subexn);

    while( info->tag != -1 && info->tag != subtag) { info++; }

    msg = info->msg;
    arg_count = info->arg_count;
  }
  else {
    msg       = "invalid exception value!";
    arg_count = 0;
  }


  if (arg_count != 0 && arg_count > Wosize_val(subexn)) {
     arg_count = Wosize_val(subexn);
  }

  /* format exception message */
  if (Tag_val(exn) == Exn_async_stack_overflow)
    snprintf( buf, MAXSTR, msg, Bstring_of_bsize(Exn_field(exn,Field_exn_val1)) );
  else if (Tag_val(exn) == Exn_async_signal)
    snprintf( buf, MAXSTR, msg, signal_description(Exn_field(exn,Field_exn_val1)) );
  else {
    switch (arg_count) {
    case 0:  snprintf( buf, MAXSTR, msg ); break;
    case 1:  snprintf( buf, MAXSTR, msg, Exn_field(subexn,Field_exn_val1) ); break;
    case 2:
    default: snprintf( buf, MAXSTR, msg, Exn_field(subexn,Field_exn_val1), Exn_field(subexn,Field_exn_val2) ); break;
    }
  }


  /* format the location */
  thread = get_current_thread();
  if (thread != NULL && thread->code_exn != 0)
  {
    inside = find_name_of_code( thread->module, thread->code_exn );
    thread->code_exn = 0;
  }

  /* give a fatal error */
  if (inside != NULL)
    print("%s at \"%s\":\n  %s.\n", prefix, inside, buf);
  else
    print("%s: %s.\n", prefix, buf);

  if (thread && !is_async_exception(Tag_val(exn)))
    print_stack_trace( thread->module, thread->exn_fp, 10 );

  fatal_error("");
}


/*----------------------------------------------------------------------
  (synchronous) exceptions
----------------------------------------------------------------------*/
struct exception_frame* global_exn_frame = NULL;

static void raise_exception( value exn )
{
  CAMLparam1(exn);
  struct exception_frame* exn_frame;
  struct thread_state*    thread;

  thread = get_current_thread();
  if (thread != NULL) {
    thread->code_exn = thread->code;
    exn_frame = thread->exn_frame;
  }
  else {
    exn_frame = global_exn_frame;
  }

  if (exn_frame == NULL)
    fatal_uncaught_exception(exn);
  else {
    exn_frame->_exn = exn;
    siglongjmp(exn_frame->_jmp.buf, 1);
  }

  /* CAMLreturn0; */
}


void raise_exn( enum exn_tag tag )
{
  raise_exception(Atom(tag));
}

static void raise_exn_1( enum exn_tag tag, value v )
{
  CAMLparam1(v);
  CAMLlocal1(exn);
  exn = alloc_small(1,tag);
  Store_field( exn, Field_exn_val1, v );
  raise_exception(exn);
/*  CAMLreturn0; */
}

void raise_exn_str( enum exn_tag tag, const char* msg )
{
  CAMLparam0();
  CAMLlocal1(vmsg);
  vmsg = copy_string(msg);
  raise_exn_1( tag, vmsg );
/*  CAMLreturn0; */
}

void raise_indirect_exn( enum exn_tag tag, int indtag )
{
  raise_exn_1( tag, Atom(indtag) );
}

void raise_indirect_exn_1( enum exn_tag tag, int indtag, value v )
{
  CAMLparam0();
  CAMLlocal1(exn);
  exn = alloc_small(1,indtag);
  Field(exn,0) = v;
  raise_exn_1( tag, exn );
  /* CAMLreturn0; */
}

void raise_indirect_exn_2( enum exn_tag tag, int indtag, value v, value w )
{
  CAMLparam0();
  CAMLlocal1(exn);
  exn = alloc_small(2,indtag);
  Field(exn,0) = v;
  Field(exn,1) = w;
  raise_exn_1( tag, exn );
  /* CAMLreturn0; */
}


void raise_arithmetic_exn( enum exn_arithmetic tag )
{
  raise_indirect_exn( Exn_arithmetic, tag );
}

void raise_system_exn( enum exn_system tag )
{
  raise_indirect_exn( Exn_system, tag );
}

void raise_runtime_exn( enum exn_runtime tag )
{
  raise_indirect_exn( Exn_runtime, tag );
}

void raise_runtime_exn_1( enum exn_runtime tag, value v )
{
  raise_indirect_exn_1( Exn_runtime, tag, v );
}

/*----------------------------------------------------------------------
  wrappers for "raise_exception"
----------------------------------------------------------------------*/
void FUN_VAR_ARGS1(raise_user, const char *, fmt, args)
{
  char buf[MAXSTR];
  vsnprintf( buf, MAXSTR, fmt, args );
  raise_exn_str( Exn_user, buf );
}
END_ARGS(args)

void FUN_VAR_ARGS1(raise_internal, const char *, fmt, args)
{
  char buf[MAXSTR];
  vsnprintf( buf, MAXSTR, fmt, args );
  raise_indirect_exn_1( Exn_runtime, Exn_runtime_error, copy_string(buf) );
}
END_ARGS(args)

void FUN_VAR_ARGS2(raise_module, const char*, mname, const char*, msg, args )
{
  CAMLparam0();
  CAMLlocal2(vname,vmsg);
  char buf[MAXSTR];
  vsnprintf( buf, MAXSTR, msg, args );

  vname = copy_string(mname);
  vmsg = copy_string(buf);
  raise_indirect_exn_2( Exn_runtime, Exn_load_error, vname, vmsg );
/*  CAMLreturn0; */
}
END_ARGS(args)

void raise_invalid_argument (const char *msg)
{
  raise_exn_str( Exn_invalid_arg, msg );
}

void raise_out_of_memory (unsigned long heap_size)
{
  /* unfortunately, we can not allocate memory for a decent exception... */
  raise_exception( Atom(Exn_async_heap_overflow) );
}


void raise_stack_overflow (unsigned long size)
{
  raise_exn_1( Exn_async_stack_overflow, Val_long(size) );
}

void raise_signal( int sig )
{
  raise_exn_1( Exn_async_signal, Val_int(sig) );
}


void raise_sys_blocked_io( void )
{
  raise_system_exn(Exn_system_blocked_io);
}

void raise_invalid_opcode( long opcode )
{
  raise_indirect_exn_1( Exn_runtime, Exn_invalid_opcode, Val_long(opcode) );
}


void raise_sys_error( int err, const char* msg )
{
  raise_indirect_exn_2( Exn_system, Exn_system_error, Val_long(err), copy_string(msg) );
  /* CAMLreturn0;  */
}
