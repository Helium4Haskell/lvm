/**----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. This file is distributed under the terms
  of the GNU Library General Public License. This file is based on the
  original Objective Caml source copyrighted by INRIA Rocquencourt.
----------------------------------------------------------------------**/

/* $Id$ */

#include <string.h>
#include "mlvalues.h"
#include "alloc.h"  /* alloc_small */
#include "fail.h"
#include "print.h"

#include "module.h"
#include "thread.h"
#include "instr.h"
#include "ccall.h"
#include "stack.h"
#include "evaluator.h"

#ifdef DEBUG
#define TRACE_TRACE
#define TRACE_INSTR
#define TRACE_STACK
#endif


#ifdef LVM_EAGER_LIMITS
extern unsigned long max_eager_stack;
extern unsigned long min_eager_stack;
extern unsigned long max_eager_heap;
#endif


/*----------------------------------------------------------------------
  macros for in the evaluator:

  variables:
  thread      the evaluation thread  (struct thread_state*)
  code        the current code block (opcode_t*)

  registers:
  pc          the program counter
  sp          the stack pointer  -- cached from thread->stack_sp
  fp          the frame pointer  -- cached from thread->stack_fp
  instr_base  the address of the first instruction label -- cached instr_first
              (this is used to fit 64bit instruction adresses into a 32bit instruction offset)
----------------------------------------------------------------------*/

/* stack macros */
#define Push(v)               {sp[-1] = (v); sp--;}
#define Pop()                 (sp++)
#define Popx()                (*sp++)

#define Push_n(n)             {sp-=n;}
#define Pop_n(n)              {sp+=n;}

#define Push_code_fixup(c)    Push(Val_fixup(c))
#define Push_caf_fixup(p)     Push( Field(*Valptr_fixup(p),Field_value_fun) )


#define Push_frame(f)         { Push_n(2); \
                                sp[0] = (f); \
                                sp[1] = Val_long( fp - sp ); \
                                fp = sp; \
                              }

#define Push_frame_val(f,v)   { Push_n(3); \
                                sp[0] = (f); \
                                sp[1] = Val_long( fp - sp ); \
                                sp[2] = (v); \
                                fp = sp; \
                              }


/*----------------------------------------------------------------------
  Save registers
----------------------------------------------------------------------*/
#define Setup_for_exn()       { thread->stack_sp = sp;  \
                                thread->stack_fp = fp;  \
                                thread->code     = Val_code(pc);  \
                              }

#define Restore_after_exn()   { fp = thread->stack_fp;  \
                                sp = thread->stack_sp;  \
                                /* pc = Code_val(thread->code); */ \
                                Set_instr_base; \
                              }

#ifdef LVM_EAGER_LIMITS
#define Setup_for_gc          { thread->eager_heap_lim = young_start; /* reset & force suspend */ \
                                lazy_blackhole(fp); \
                                Setup_for_exn(); \
                              }
#else
#define Setup_for_gc          { Setup_for_exn(); \
                                lazy_blackhole(fp); \
                              }
#endif

#define Restore_after_gc      { Restore_after_exn(); }


/*----------------------------------------------------------------------
  safe points
----------------------------------------------------------------------*/
#define Safe_check_()         Safe_checkx(sp[0] = sp[0] /* nothing */)
#define Safe_check(v)         Safe_checkx((*--sp = (v) ) /* == Push(v) */ )

#define Safe_checkx(v)        { Safe_signal_check((v)); \
                                Safe_stack_check((v)); \
                                Safe_heap_check((v)); \
                              }

#define Safe_signal_check(v)  { if (pending_signal()) { (v); Return(Thread_yield); }}

#ifdef LVM_EAGER_LIMITS
#define Safe_stack_check(v)   { if (sp < thread->stack_lim) { \
                                  if (thread->eager_top != NULL) { \
                                    (v); \
                                    Trace_value2( "stack check: suspend eager", v, Frame_value(thread->eager_top) ); \
                                    Setup_for_exn(); \
                                    recover_eager(thread); \
                                    Restore_after_exn(); \
                                    pc = Code_val(thread->code); \
                                    Next; \
                                  } else { \
                                    Setup_for_gc;       \
                                    thread_grow_stack(thread);  \
                                    Restore_after_gc;  } \
                                } \
                              }
#else
#define Safe_stack_check(v)   { if (sp < thread->stack_lim) { \
                                  Setup_for_gc;       \
                                  thread_grow_stack(thread);  \
                                  Restore_after_gc;  \
                                } \
                              }
#endif

#ifdef LVM_EAGER_LIMITS
#define Safe_heap_check(v)    { if (thread->eager_top != NULL && young_ptr > thread->eager_heap_lim) { \
                                  (v); \
                                  Trace_value2( "heap check: suspend eager", v, Frame_value(thread->eager_top) ); \
                                  Setup_for_exn(); \
                                  recover_eager(thread); \
                                  Restore_after_exn(); \
                                  pc = Code_val(Pop()); \
                                  Next; \
                                }; \
                              }
#else
#define Safe_heap_check(v)
#endif



/*----------------------------------------------------------------------
  machine actions
----------------------------------------------------------------------*/
#define Return(r)         { Setup_for_gc; \
                            Restore_exception_handler(exn_frame,thread); \
                            thread->result = (r); \
                            return; }

#define Raise_runtime_exn(exn)    { Setup_for_exn(); raise_runtime_exn_1( exn, copy_string(find_name_of_code( thread->module, thread->code )) ); }
#define Raise_arithmetic_exn(exn) { Setup_for_exn(); raise_arithmetic_exn( exn ); }

#define Allocate(v,sz,t)  { if (sz == 0) { (v) = Atom(t); } \
                            else if (sz < Max_young_wosize) { Alloc_small(v,sz,t); } \
                            else { Alloc_large(v,sz,t); } \
                          }

#define Alloc_large(v,sz,t)  { Setup_for_gc; \
                               (v) = alloc_shr(sz,t); \
                               Restore_after_gc; \
                             }

#define Alloc_con(v,sz,t) { if (t >= Con_max_tag) {\
                               Allocate(v,sz+1,Con_max_tag); \
                               Field(v,sz) = Val_long(t); \
                            } else { \
                               Allocate(v,sz,t); \
                            } \
                          }

#ifdef DEBUG
#define Debug_pcstart(pc)  pcstart = pc;
#else
#define Debug_pcstart(pc)
#endif

#ifdef LVM_EAGER_LIMITS
#define Eager_restore()   { if (fp > thread->eager_top) { \
                              thread->stack_lim = thread->eager_stack_lim; \
                              thread->eager_top = NULL; \
                          }}
#else
#define Eager_restore()
#endif


/*----------------------------------------------------------------------
  macros for debugging
----------------------------------------------------------------------*/
#define todo(msg)     fatal_error("todo: %s\n", msg)
#define todoInstr(i)  Instr(i): { todo( #i ); }

#define Require(p)   Assert(p)

#ifdef TRACE_TRACE
# define Trace(msg)             { print(msg); print("\n"); }
# define Trace_value(msg,x)     { print( msg ); print( " -- " ); print_value(thread->module,x); print("\n"); }
# define Trace_value2(msg,x,y)  { print( msg ); print( " -- " ); print_value(thread->module,x); print(" -- "); print_value(thread->module,y); print("\n"); }
# define Trace_i_i(msg, i, j)   { print( msg ); print(" %i %i\n", i, j); }
#else
# define Trace(msg)
# define Trace_value(msg,x)
# define Trace_value2(msg,x,y)
# define Trace_i_i(msg, i, j)
#endif

#define Trace_enter(msg,x)      Trace_value("enter: " msg,x)
#define Trace_entercon(msg,x)   Trace_value("enter con: frame: " msg,x)
#define Trace_argchk(msg,x)     Trace_value( "argchk: " msg, x )
#define Trace_raise(msg,x)      Trace_value( "raise: " msg, x )

#ifdef TRACE_STACK
#define Trace_stack(msg)        { print( "\n--" msg "---------\n" ); print_stack(thread->module,sp,fp); print( "---------\n" ); }
#else
#define Trace_stack(msg)
#endif

#ifdef DEBUG
static value* Frame_limit( value* fp )
{
  while (Frame_frame(fp) == frame_cont || Frame_frame(fp) == frame_catch || Frame_frame(fp) == frame_eager) {
          fp = Frame_next(fp);
  }
  return fp;
}
#endif



/*----------------------------------------------------------------------
  Low level optimizations, only available with GNU C at the moment
----------------------------------------------------------------------*/

/* instruction dispatch:
   THREADED_CODE makes nfib almost twice as fast on a pentium!
*/
#ifdef THREADED_CODE
 #define Instr(name)  label_##name
 #ifdef THREADED_OFFSET
   #define Next              goto *(void*)(instr_base + *pc++)
   #define Set_instr_base    {instr_base = instr_first;}
   char*   instr_first;
 #else
   #define Next              goto *(void*)(*pc++)
 #endif
 char** instr_table;
#else
 #define Instr(name)  case name
 #define Next         break
#endif

#if !defined(Set_instr_base)
# define Set_instr_base
#endif


#ifdef THREADED_CODE
  #ifdef THREADED_OFFSET
    #define Val_instr(i)    (value)(instr_table[i] - instr_base)
  #else
    #define Val_instr(i)    (value)(instr_table[i])
  #endif
#else
  #define Val_instr(i)      (value)(i)
#endif

/* register optimization:
   this makes a big difference, nfib is 1.5 times faster on a pentium
*/
#if defined(__GNUC__) && !defined(DEBUG)
#ifdef __i386__
# define PC_REG asm("%esi")
# define SP_REG asm("%edi")
# define FP_REG
#endif
#ifdef __mips__
#define PC_REG asm("$16")
#define SP_REG asm("$17")
#define FP_REG asm("$18")
#endif
#ifdef __sparc__
#define PC_REG asm("%l0")
#define SP_REG asm("%l1")
#define FP_REG asm("%l2")
#endif
#ifdef __alpha__
#ifdef __CRAY__
#define PC_REG asm("r9")
#define SP_REG asm("r10")
#define FP_REG asm("r11")
#define INSTR_BASE_REG asm("r12")
#else
#define PC_REG asm("$9")
#define SP_REG asm("$10")
#define FP_REG asm("$11")
#define INSTR_BASE_REG asm("$12")
#endif
#endif
#if defined(PPC) || defined(_POWER) || defined(_IBMR2)
#define PC_REG asm("26")
#define SP_REG asm("27")
#define FP_REG asm("28")
#endif
#ifdef __hppa__
#define PC_REG asm("%r18")
#define SP_REG asm("%r17")
#define FP_REG asm("%r16")
#endif
#ifdef __mc68000__
#define PC_REG asm("a5")
#define SP_REG asm("a4")
#define FP_REG asm("d7")
#endif
#ifdef __arm__
#define PC_REG asm("r9")
#define SP_REG asm("r8")
#define FP_REG asm("r7")
#endif
#ifdef __ia64__
#define PC_REG asm("36")
#define SP_REG asm("37")
#define FP_REG asm("38")
#define INSTR_BASE_REG asm("39")
#endif
#endif  /* GNUC & DEBUG */

#ifndef PC_REG
# define PC_REG
#endif
#ifndef SP_REG
# define SP_REG
#endif
#ifndef FP_REG
# define FP_REG
#endif
#ifndef INSTR_BASE_REG
# define INSTR_BASE_REG
#endif



/*----------------------------------------------------------------------
  the evaluator
----------------------------------------------------------------------*/
void init_evaluator(void)
{
  evaluate(NULL);
}


void evaluate( struct thread_state* thread )
{
  /* 'registers' of the virtual machine */
  register opcode_t* pc PC_REG;
  register value*    sp SP_REG;
  register value*    fp FP_REG;
#ifdef THREADED_OFFSET
  register char*     instr_base INSTR_BASE_REG;
#endif

#ifdef DEBUG
  opcode_t* pcstart = NULL;
#endif

  /* exception handling */
  struct exception_frame exn_frame;
  value  exn = 0;

  /* initialise */
  if (thread == NULL)
  {
    /* let instr_table point to the jumptable (since gcc insists that jumptable must be local) */
#ifdef THREADED_CODE
# define Ins(name,args)  &&label_##name
    static void* jumptable[] = { INSTRLIST };
# undef Ins

    instr_table = (char**)jumptable;
    #ifdef THREADED_OFFSET
    instr_first = (char*)&&label_instr_first;
    #endif
#endif
    return;
  }

  /* check if this is a runnable thread */
  if (thread == NULL || thread->result != Thread_yield) return;


  /* set the instruction basic offset for 64bit machines */
  Set_instr_base;


  /* install the exception handler */
  Setup_exception_handler(exn_frame,thread,exn, \
                          { Restore_after_exn(); \
                            pc = 0;
                            Push(exn); \
                            goto raise_exception; \
                          });

  /* cache part of the thread state in local 'registers' */
  pc      = 0;
  sp      = thread->stack_sp;
  fp      = thread->stack_fp;
  if (sp >= fp) fatal_error( "fatal error: corrupted stack -- (%sp >= %fp) on enter" );


  /* start execution by entering the value on top of the stack */
  goto enter;

  while(1)
  {
#if defined(DEBUG)
 #ifdef TRACE_INSTR
    print( "%4i: ", (char*)pc - (char*)pcstart ); print_instr( thread->module, sp, pc );
 #endif
    /* Setup_for_gc;    debug_gc();    Restore_after_gc;   */
#endif

#if !defined(THREADED_CODE)
  switch (*pc++)
#endif
  {
#if defined(THREADED_OFFSET)
    label_instr_first:
#endif


/*----------------------------------------------------------------------
  Optimized Enter instructions
----------------------------------------------------------------------*/
    Instr(ENTERCODE): {
      Trace ("ENTERCODE");
      pc = Code_fixup(*pc);
      Debug_pcstart(pc);
      Safe_check(Val_code(pc));
      Trace_enter( "direct code", Val_code(pc) );
      Require( Is_block(Val_code(pc)) && Tag_val(Val_code(pc)) == Code_tag );
      pc += 2;      /* skip ARGCHK */
      Next;
    }


    Instr(EVALVAR): {
      value v     = sp[*pc++];
      Trace ("EVALVAR");

      Assert( v != 0x4 || Is_long(v) || Is_heap_val(v) || Tag_val(v) == Code_tag || Is_atom(v) );
      if ((Is_long(v) || Tag_val(v) <= Con_max_tag) /* && !pending_signal() */) {
        Trace( "evalvar: already evaluated var" );
        Push(v);
        Next;
      }

      Trace_value( "evalvar: failed direct evaluation", v );
      Push_frame_val(frame_cont,Val_code(pc));
      Push(v);
      /* fall through to enter */
    }


/*----------------------------------------------------------------------
  Enter instruction: the most complicated of all :-)
----------------------------------------------------------------------*/
enter:
    Instr(ENTER): {
      register value accu = sp[0]; /* accu reflects sp[0] */
      /* Safe_signal_check(accu);  -- it will find an ARGCHK sooner or later */

enterloop:
      Trace_stack("ENTER");

      Require( sp > thread->stack );
      Require( sp < thread->stack_top );
      Require( sp < fp );

      if (Is_long(accu) || Tag_val(accu) <= Con_max_tag || Tag_val(accu) > Abstract_tag) {
        goto return_enter;
      }

      Assert( Is_heap_val(accu) || Tag_val(accu) == Code_tag);
      switch(Tag_val(accu)) {
        case Code_tag: {
          /* jump to the code */
          Trace_enter( "code", accu );
          pc = Code_val(accu);
          Debug_pcstart(pc);
          Pop();
          Next;
        }

        case Ap_tag: {
          /* push update frame and arguments and continue */
          nat n    = Wosize_val(accu);
          Require( n > 0 );
          Trace_enter( "ap node", accu);
          Push_frame(frame_update);
          Push_n(n);
          while( n > 0 ) { n--; sp[n] = Field(accu,n); }
          accu = sp[0];
          Safe_check_(); /* a program might loop locally on an Ap, ie. "let x = x in x" */
          goto enterloop;
        }

        case Nap_tag: {
          /* push arguments and continue */
          nat   n   = Wosize_val(accu);
          Require( n > 0 );
          Trace_enter( "nap node", accu );
          Push_n(n-1);  /* overwrite sp[0] */
          while( n > 0) { n--; sp[n] = Field(accu,n); }
          accu = sp[0];
          Safe_check_(); /* a program might loop locally on a Nap, ie. "let x = x in x". Is this True ?? */
          goto enterloop;
        }

        case Ind_tag: {
          /* continue with indirection */
          Trace_enter( "indirection", accu );
          accu = sp[0] = Field(accu,0);
          goto enterloop;
        }


        case Caf_tag: {
          /* push update frame and jump to caf */
          Trace_enter( "caf", accu );
          Tag_val(accu) = Inv_tag;       /* eagerly blackhole, a loop might not grow the stack */
          pc = Code_val(Field(accu,0));
          Debug_pcstart(pc);
          Push_frame(frame_update);
          Safe_check(Val_code(pc));
          Next;
        }

        case Inv_tag: {
          /* infinite loop */
          Trace_enter( "blackhole!", accu );
          Raise_runtime_exn( Exn_blackhole );
          Next;
        }

        case Raise_tag: {
          /* push exception and raise it */
          accu = sp[0] = Field( accu, 0 );
          goto raise_exception;
        }

        case Suspend_tag: {
          /* restore the stack from a suspension */
          nat   i;
          value susp  = Popx();
          nat   ssize = Wosize_val(susp) - Susp_info_wosize;
  #ifdef LVM_EAGER_LIMITS
          long  eager = Long_val(Field(susp,Field_susp_eager));
  #endif
          long  base  = Long_val(Field(susp,Field_susp_base));
          long  top   = Long_val(Field(susp,Field_susp_top));

          Require(ssize > Frame_size);
          Trace_enter( "suspension", susp );

          /* push suspended stack values */
          Push_n(ssize);
          for( i = 0; i < ssize; i++) { sp[i] = Field(susp,i+Susp_info_wosize); }

          /* relink the frames */
          if (top >= base) {
            sp[top+1] = Val_long(fp - sp - top);
            fp        = sp + base;
          }

  #ifdef LVM_EAGER_LIMITS
          if (eager >= 0 && thread->eager_top == NULL) {
            Trace( "enter suspension: restore eager limit" );
            thread->eager_top = sp + eager;
            thread->eager_stack_lim = thread->stack_lim;
            thread->eager_heap_lim  = young_ptr + max_eager_heap;
            thread->stack_lim = thread->eager_top - max_eager_stack;
            if (thread->stack_lim < thread->eager_stack_lim) thread->stack_lim = thread->eager_stack_lim;
          }
  #endif
          accu = sp[0];
          goto enterloop;
        }

        default: {
          todo( "ENTER: invalid tag" );
        } /* default */
      } /* switch Tag_val(accu) */
      Next; /* for a non-threaded implementation */
    }

/*----------------------------------------------------------------------
  Argument check: rivals enter in complexity :-)
----------------------------------------------------------------------*/
    Instr(ARGCHK): {
      nat n = *pc++;
      Trace ("ARGCHK");
      Safe_check(Val_code(pc-2));
      Require( sp <= fp  );
      while (sp + n > fp) {  /* too few arguments? */
        nat args = fp-sp;
        switch (Frame_frame(fp)) {
        case frame_cont: {
          /* case on functional value */
          /* enter continuation with nap */
          value nap;
          if (args == 0)
            nap = Val_code((pc-2));
          else {
            nat i;
            Allocate(nap,args+1,Nap_tag);
            Field(nap,0) = Val_code((pc-2));
            for( i = 0; i < args; i++) { Field(nap,i+1) = sp[i]; }
            Pop_n(args);
          }
          Trace_argchk( "case on functional value (or eager functional result)", nap );

          /* goto continuation */
          pc = Code_val( Frame_value(fp) );
          Debug_pcstart(pc);

          /* restore the stack */
          sp    = fp + Frame_size - 1;
          fp    = Frame_next(fp);
          sp[0] = nap;
          Next;
        }

        case frame_update: {
          /* update with functional value */
          value upd  = Frame_value(fp);
          nat   i;
          fp = Frame_next(fp);
          if (args == 0) {
            Trace_argchk( "update with indirection", upd );
            Tag_val(upd) = Ind_tag;
            Store_field(upd,0,Val_code((pc-2)));
            for( i = 1; i < Wosize_val(upd); i++) { Store_field_0(upd,i); }
            }
          else {
            nat n = Wosize_val(upd);
            #ifdef LVM_UPDATE_INPLACE
            if (n > args) {
              Trace_argchk( "update in place", upd );
              Tag_val(upd) = Nap_tag;
              Field(upd,0) = Val_code((pc-2));
              for( i = args; i > 0; i--)   { value x = sp[i-1]; sp[i+2] = x; Store_field(upd,i,x); }
              for( i = args+1; i < n; i++) { Store_field_0(upd,i); }
            } else
            #endif
            { value nap;
              Trace_argchk( "update with indirect nap", upd );
              Allocate(nap,args+1,Nap_tag);
              Field(nap,0) = Val_code((pc-2));
              for( i = args; i > 0; i--) { Field(nap,i) = sp[i+2] = sp[i-1]; }
              Tag_val(upd) = Ind_tag;
              Store_field(upd,0,nap);
              for( i = 1; i < n; i++) { Store_field_0(upd,i); }
            }
          }
          Pop_n(Frame_size);
          break;
        }


#ifdef LVM_EAGER_FRAMES
        case frame_eager: { /* functional value as result */
          /* return a nap */
          value nap;
          if (args == 0)
            nap = Val_code((pc-2));
          else {
            nat i;
            Allocate(nap,args+1,Nap_tag);
            Field(nap,0) = Val_code((pc-2));
            for( i = 0; i < args; i++) { Field(nap,i+1) = sp[i]; }
            Pop_n(args);
          }
          Trace_argchk( "eager functional value", nap );

          /* push the value on the stack an continue */
          pc    = Code_val( Frame_value(fp) );
          fp    = Frame_next(fp);
          Pop_n(Frame_size-1);
          sp[0] = nap;
          Eager_restore();
          Next;
        }
#endif
        case frame_catch: {
          /* functional value without exceptions */
          /* zap the frame and things behind it */
          value* spnew;
          Trace_argchk( "zap catch frame", Frame_value(fp) );
          fp = Frame_next(fp);
          spnew = fp - args;
          while(args > 0) { args--; spnew[args] = sp[args]; }
          sp = spnew;
          break;
        }

        case frame_stop: {
          /* functional value as result */
          value nap;
          if (args == 0)
            nap = Val_code((pc-2));
          else {
            nat i;
            Allocate(nap,args+1,Nap_tag);
            Field(nap,0) = Val_code((pc-2));
            for( i = 0; i < args; i++) { Field(nap,i+1) = sp[i]; }
            Pop_n(args);
          }
          Push(nap);
          Trace_argchk( "functional value as result", nap );
          Return(Thread_complete);
        }

        default:
          todo( "ARGCHK: unknown frame" );
        } /* switch( Frame_frame(fp) ) */
      } /* while (too few arguments) */

      Next;
    }


/*----------------------------------------------------------------------
  Exceptions
----------------------------------------------------------------------*/
    Instr(CATCH): {
      Trace ("CATCH");
      Push_frame(frame_catch);
      Next;
    }

    Instr(RAISE): {
      value exn;
      Trace ("RAISE");

raise_exception:
      Require( sp < fp );
      exn = Popx();
      thread->exn_fp = fp;

      /* recover the stack */
      if (Tag_val(exn) == Exn_async_heap_overflow) {
        fp = recover_synchronous( fp, exn ); /* TODO: not semi??? */
      } else if (is_async_exception(Tag_val(exn))) {
        /* save context for eventual gc (optimisation: no blackholing) */
        Setup_for_exn();
        fp = recover_asynchronous( fp, sp );
      } else {
        fp = recover_synchronous( fp, exn );
      }

      switch (Frame_frame(fp)) {
        case frame_catch: {
          /* caught exception */
          value handler;
          Trace_raise( "exception caught", exn );
          handler = Frame_value(fp);
          fp = Frame_next(fp);
          sp = fp;  /* zap things behind the frame */
          Eager_restore();
          thread->code_exn = 0;
          Push(exn);
          Push(handler);
          goto enter;
        }

#ifdef LVM_EAGER_FRAMES
        case frame_eager: {
          value v;
          Trace( "raise: eager frame" );
          /* note: the exception is synchronous (since recover_asynchronous suspends eager frames)
          except for heap_overflow -- unfortunately, we lost
          the original application and can't restore the original situation
          but we can't allocate a suspension or raise node either
          ...uurgh, we just re-raise the exception :-( */
          if (Tag_val(exn) == Exn_async_heap_overflow) {
            fp = Frame_next(fp);
            sp = fp;
            Eager_restore();
            Push(exn);
            goto raise_exception;
          }

          /* restore the stack and pc */
          Trace_raise( "eager frame", exn );
          pc = Code_val( Frame_value(fp) );
          Debug_pcstart(pc);
          sp = fp;
          fp = Frame_next(fp);
          Pop_n(Frame_size-1);

          /* now allocate a "raise" block */
          Alloc_small(v,1,Raise_tag);
          Field(v,0) = exn;
          sp[0] = v;

          thread->code_exn = 0;

          /* and continue at the pc */
          Eager_restore();
          Next;
        }
#endif

        case frame_stop: {
          /* uncaught exception */
          Trace_raise( "uncaught exception", exn );
          sp = fp;
          Eager_restore();
          Push(exn);
          Return(Thread_exception);
        }

        default: {
          fatal_error( "fatal error: corrupted stack -- invalid stack frame after exception!" );
        }
      }; /* switch */
      Next;
    }


/*----------------------------------------------------------------------
  RETURNINT i == PUSHINT i; SLIDE 1 m; ENTER == PUSHINT i; RETURN
----------------------------------------------------------------------*/

/*----------------------------------------------------------------------
  RETURN: enter an int or constructor
----------------------------------------------------------------------*/
    Instr(RETURNCON0): {
      Trace ("RETURNCON0");
      Push(Atom(pc[0]));
      goto return_enter;
    }

    Instr(RETURNINT):{
      Trace ("RETURNINT");
      Push(Val_long(pc[0]));
      /* fall through */
    }

return_enter:
    Instr(RETURN):{
      /* enter a primitive value or constructor */
      register value accu;
      Trace ("RETURN");
      accu = Popx();
      Require(Is_long(accu) || Tag_val(accu) <= Con_max_tag || Tag_val(accu) > Abstract_tag);


returnloop:
      switch(Frame_frame(fp)) {
#if defined(LVM_EAGER_FRAMES) && !defined(LVM_EAGER_LIMITS)
        case frame_eager:
#endif
        case frame_cont: {
          /* jump to the continuation */
          pc = Code_val( Frame_value(fp) );
          Debug_pcstart(pc);
          Trace_entercon( "cont/eager", Val_code(pc) );
          /* restore the stack */
          sp    = fp + Frame_size - 1;
          fp    = Frame_next(fp);
          sp[0] = accu;
          Next;
        }

       case frame_update: {
          /* overwrite updated value with constructor */
          nat i;
          value upd = Frame_value(fp);
          Require( Is_block(upd) && Wosize_val(upd) > 0 && (Tag_val(upd) == Ap_tag || Tag_val(upd) == Inv_tag || Tag_val(upd) == Caf_tag || Tag_val(upd) == Ind_tag || Tag_val(upd) == Suspend_tag) );

          #ifdef LVM_UPDATE_INPLACE
          if (Is_block(accu) && Wosize_val(upd) >= Wosize_val(accu)) {
            /* update in place */
            Trace_value2( "enter con: frame update (inplace)", accu, upd );
            Tag_val(upd) = Tag_val(accu);
            i = 0;
            while (i < Wosize_val(accu)) { Store_field( upd,i,Field(accu,i)); i++; }
            while (i < Wosize_val(upd) ) { Store_field_0( upd, i ); i++; }
          } else
          #endif
          { /* update with indirection */
            Trace_value2( "enter con: frame update (with indirection)", accu, upd );
            Tag_val(upd) = Ind_tag;
            Store_field(upd,0,accu);
            for (i = 1; i < Wosize_val(upd); i++) { Store_field_0( upd, i ); }
          }
          fp = Frame_next(fp);
          goto returnloop;
        }

  #if defined(LVM_EAGER_FRAMES) && defined(LVM_EAGER_LIMITS)
        case frame_eager: {
          /* push the value on the stack an continue */
          pc = Code_val( Frame_value(fp) );
          Debug_pcstart(pc);
          sp  = fp + Frame_size - 1;
          fp  = Frame_next(fp);
          sp[0] = accu;
          Eager_restore();
          Trace_value2( "enter con: eager continue", Val_code(pc), accu );
          Next;
        }
  #endif

        case frame_catch: {
          /* ignore the frame */
          fp = Frame_next(fp);
          goto returnloop;
        }

        case frame_stop: {
          /* return with the constructor */
          Trace_entercon( "stop", accu );
          sp = fp;
          Push(accu);
          Return(Thread_complete);
        }

        default:
          todo( "enter con: unknown frame" );
      } /* switch( Frame_frame(fp) ) */
      Next; /* for a non-threaded build */
    }



/*----------------------------------------------------------------------
  RETURNCON t n == NEWCON t n; SLIDE 1 m; ENTER == NEWCON t n; RETURN
----------------------------------------------------------------------*/
#define AllocCon() \
    if (con==0) { \
      if (n==0) \
        con = Atom(tag); \
      else if (n <= Max_young_wosize) { \
        nat i; \
        Alloc_small(con,n,tag);\
        for( i = 0; i < n; i++) { Field(con,i) = sp[i]; }\
      } else {\
        nat i; \
        Alloc_large(con,n,tag);\
        for( i = 0; i < n; i++) { Init_field(con,i,sp[i]); }\
      }\
    }

    Instr(RETURNCON): {
      nat tag  = *pc++;
      nat n   = *pc++;
      value con = 0;

      Require( sp + n <= fp );
      Require( tag < Con_max_tag );
      Trace_stack("RETURNCON");

returncon:
      switch(Frame_frame(fp)) {
        case frame_cont: {
          /* jump to the continuation, hopefully without allocation */
          pc = Code_val(Frame_value(fp));

          if (*pc == Val_instr(SWITCHCON))
          {
            value* bp;
            /* hoeree, we can probably switch immediately without allocation */
            Trace_value( "returncon: continue into switch: ", Val_code(pc) );

            /* restore the stack */
            bp = fp + Frame_size - n;
            fp = Frame_next(fp);
            if (bp != sp) {
              nat i = n;
              while (i > 0) { i--; bp[i] = sp[i]; }
              sp  = bp;
            }

            /* interpret the SWITCHCON instruction */
            pc++;
            {
              nat count  = pc[0];
              nat ofs;

              if (tag >= count) {
                /* default case: we have to allocate */
                ofs = pc[1];
                if (ofs==0) { Raise_runtime_exn(Exn_failed_pattern); }
                pc += ofs;

                AllocCon();
                Pop_n(n);
                Push(con);
              }
              else {
                ofs = pc[tag+2];
                if (ofs==0) { Raise_runtime_exn(Exn_failed_pattern); }
                pc += ofs;
              }

              Next;
            }
          }
          else if (*pc == Val_instr(MATCHCON))
          {
            value* bp;
            /* hoeree, we can probably switch immediately without allocation */
            Trace_value( "returncon: continue into match: ", Val_code(pc) );

            /* restore the stack */
            bp = fp + Frame_size - n;
            fp = Frame_next(fp);
            if (bp != sp) {
              nat i = n;
              while (i > 0) { i--; bp[i] = sp[i]; }
              sp  = bp;
            }

            /* interpret the MATCHCON instruction */
            pc++;
            {
              long count = pc[0];
              long i;
              long ofs   = pc[1];

              for(i = 1; i <= count; i++) {
                if (pc[i*2] == tag) { ofs = pc[i*2+1]; break; }
              }

              if (ofs == 0) { Raise_runtime_exn(Exn_failed_pattern); }
              pc += ofs;

              if (i > count) {
                /* default case: we have to allocate :-( */
                AllocCon();
                Pop_n(n);
                Push(con);
              }

              Next;
            }
          }
          else {
            AllocCon();

            Trace_value( "returncon: failed unshared continue into", Val_code(pc) );
            Debug_pcstart(pc);
            sp    = fp + Frame_size - 1;
            fp    = Frame_next(fp);
            sp[0] = con;
            Next;
          }

          Next; /* for non-threaded applications */
        }

        case frame_update: {
          /* overwrite update value with the constructor */
          nat i;
          value upd = Frame_value(fp);

          /* and update */
          if (Wosize_val(upd) >= n) {
            /* update in place */
            Trace_value( "returncon: update frame in-place: ", upd );
            Tag_val(upd) = (char)tag;
            i = 0;
            while (i < n)               { Store_field( upd, i, sp[i] ); i++; }
            while (i < Wosize_val(upd)) { Store_field_0( upd, i ); i++; }
          } else {
            /* update with indirection */
            Trace_value( "returncon: update frame: ", upd );
            AllocCon();

            Tag_val(upd) = Ind_tag;
            Store_field( upd, 0, con );
            for( i = 1; i < Wosize_val(upd); i++) { Store_field_0( upd, i ); }
          }

          fp = Frame_next(fp);
          goto returncon;
        }

#ifdef LVM_EAGER_FRAMES
        case frame_eager: {
          AllocCon();

          pc = Code_val(Frame_value(fp));
          Trace_value( "returncon: failed unshared continue into", Val_code(pc) );
          Debug_pcstart(pc);
          sp    = fp + Frame_size - 1;
          fp    = Frame_next(fp);
          sp[0] = con;
          Eager_restore();
          Next;
        }
#endif

        case frame_catch: {
          /* ignore this frame */
          fp = Frame_next(fp);
          goto returncon;
        }

        case frame_stop: {
          /* return with this constructor */
          AllocCon();

          sp = fp;
          Push(con);
          Return(Thread_complete);
        }

        default: {
          todo( "return con: unknown frame" );
        }
      } /* switch Frame_frame(fp) */

      Next; /* for a non-threaded application */
    }


/*----------------------------------------------------------------------
  Matching
----------------------------------------------------------------------*/
    Instr(SWITCHCON): {
      nat tag = Tag_val(sp[0]);
      nat count = pc[0];
      nat ofs;
      Trace ("SWITCHCON");

      Require( sp < fp );
      Require( Is_long(sp[0]) || (Is_block(sp[0]) && Tag_val(sp[0]) < Con_max_tag ));

      if (tag >= count) {
        /* default */
        ofs = pc[1];
        if (ofs == 0) { Raise_runtime_exn(Exn_failed_pattern); }
        pc += ofs;
      }
      else {
        value con;
        nat j;

        ofs = pc[tag+2];
        if (ofs == 0) { Raise_runtime_exn(Exn_failed_pattern); }
        pc += ofs;

        /* unpack the constructor */
        con = Popx();
        j   = Wosize_val(con);
        Push_n(j);
        while( j > 0 ) { sp[j-1] = Field(con,j-1); j--; }
      }

      Next;
    }

    Instr(MATCHCON): {
      nat i;
      nat tag;
      nat n   = pc[0];
      nat ofs = pc[1];
      Trace ("MATCHCON");

      Require( sp < fp );
      Require( Is_long(sp[0]) || (Is_block(sp[0]) && Tag_val(sp[0]) < Con_max_tag ));

      tag = Tag_val(sp[0]);
      for( i = 1; i <= n; i++ ) {
        if (pc[i*2] == tag) {
          /* we have a match, unpack constructor to the stack */
          value con = Popx();
          nat j   = Wosize_val(con);
          ofs       = pc[i*2+1];
          Push_n(j);
          while (j > 0) { sp[j-1] = Field(con,j-1); j--; }
          break;
        }
      }

      if (ofs == 0) { Raise_runtime_exn(Exn_failed_pattern); }
      pc += ofs;
      Next;
    }


    Instr(MATCHINT): {
      long x   = Long_val(sp[0]);
      long n   = pc[0];
      long ofs = pc[1];
      long i;
      Trace ("MATCHINT");

      for( i = 1; i <= n; i++) {
        if ((long)pc[i*2] == x) { ofs = pc[i*2+1]; Pop(); break; }
      }

      if (ofs == 0) { Raise_runtime_exn(Exn_failed_pattern); }
      pc += ofs;
      Next;
    }


/*----------------------------------------------------------------------
  Stack manipulation
----------------------------------------------------------------------*/
    Instr(PUSHCODE): {
      Push_code_fixup(*pc); pc++; Next;
      Trace_stack ("PUSHCODE");
    }

    Instr(PUSHCAF): {
      Push_caf_fixup(*pc); pc++; Next;
      Trace_stack ("PUSHCAF");
    }

    Instr(PUSHCONT): {
      long ofs = *pc++;
      Push_frame_val( frame_cont, Val_code(pc + ofs) );
      Trace_stack ("PUSHCONT");
      Next;
    }

#ifdef LVM_EAGER_FRAMES
    Instr(PUSHEAGER): {
      long ofs = *pc++;
      Trace ("PUSHEAGER");
      Push_frame_val( frame_eager, Val_code(pc+ofs) );
      Next;
    }
#endif

    Instr(PUSHVAR): {
      Require( sp + *pc < Frame_limit(fp) );
      Push(sp[*pc++]);
      Trace_stack ("PUSHVAR");
      Next;
    }

    Instr(PUSHVAR0): {
      Require( sp < Frame_limit(fp) );
      Push(sp[0]);
      Trace_stack ("PUSHVAR0");
      Next;
    }

    Instr(PUSHVAR1): {
      Require( sp + 1 < Frame_limit(fp) );
      Push(sp[1]);
      Trace_stack ("PUSHVAR1");
      Next;
    }

    Instr(PUSHVAR2): {
      Require( sp + 2 < Frame_limit(fp) );
      Push(sp[2]);
      Trace_stack ("PUSHVAR2");
      Next;
    }

    Instr(PUSHVAR3): {
      Require( sp + 3 < Frame_limit(fp)  );
      Push(sp[3]);
      Trace_stack ("PUSHVAR3");
      Next;
    }

    Instr(PUSHVAR4): {
      Require( sp + 4 < Frame_limit(fp) );
      Push(sp[4]);
      Trace_stack ("PUSHVAR4");
      Next;
    }

    Instr(PUSHVARS2): {
      Require( sp + *pc < Frame_limit(fp) );
      Push(sp[*pc++]);
      Require( sp + *pc < Frame_limit(fp) );
      Push(sp[*pc++]);
      Trace_stack ("PUSHVARS2");
      Next;
    }

    todoInstr(PUSHVARS3)
    todoInstr(PUSHVARS4)

    Instr(PUSHINT): {
      Push(Val_long(*pc++));
      Trace_stack ("PUSHINT");
      Next;
    }

    todoInstr(PUSHDOUBLE)

    Instr(PUSHSTRING): {
      value decl = *(Valptr_fixup(*pc++));
      Require(Is_block(decl) && Tag_val(decl) == Rec_bytes);
      Push(Field(decl,Field_bytes_string));
      Trace_stack ("PUSHSTRING");
      Next;
    }

    Instr(SLIDE): {
      long n  = *pc++;
      long m  = *pc++; /* fp - sp - n; */
      Require( m >= 0 );
      while (n > 0) { sp[n+m-1] = sp[n-1]; n--; };
      Pop_n(m);
      Trace_i_i ("SLIDE", n, m)
      Trace_stack ("SLIDE");
      Next;
    }


    Instr(STUB): {
      long n = *pc++;
      Require( sp + n <= fp );
      sp[n] = 0;
      Trace_stack ("STUB");
      Next;
    }

/*----------------------------------------------------------------------
  Application nodes
----------------------------------------------------------------------*/
    Instr(ALLOCAP): {
      value ap;
      nat   size = *pc++;
      Trace ("ALLOCAP");
      Require( size > 0 );
      Allocate(ap,size,Inv_tag);
      while (size > 0) { size--; Field(ap, size) = 0; }
      Push(ap);
      Next;
    }

    Instr(PACKAP): {
      nat ofs = *pc++;
      nat n   = *pc++;
      nat i;
      value ap;
      Trace ("PACKAP");
      Require( sp + ofs <= fp );
      ap = sp[ofs];
      Require( Wosize_val(ap) == n && Tag_val(ap) == Inv_tag );
      Tag_val(ap) = Ap_tag;
      for( i = 0; i < n; i++ ) { Store_field( ap, i, sp[i]); }
      Pop_n(n);
      Next;
    }

    Instr(PACKNAP): {
      nat ofs = *pc++;
      nat n   = *pc++;
      nat i;
      value nap;
      Trace ("PACKNAP");
      Require( sp + ofs <= fp );
      nap = sp[ofs];
      Require( Wosize_val(nap) == n && Tag_val(nap) == Inv_tag );
      Tag_val(nap) = Nap_tag;
      for( i = 0; i < n; i++ ) { Store_field( nap, i, sp[i]); }
      Pop_n(n);
      Next;
    }

    Instr(NEWAP): {
      value ap;
      nat   n;
      nat   i;
      Trace ("NEWAP");
      #ifdef LVM_EAGER
      // if (sp - thread->stack_lim > min_eager_stack)
      goto eager_ap;
      #endif
      n = *pc++;
      Require( sp + n <= fp );
      Allocate(ap,n,Ap_tag);
      for (i = 0; i < n; i++) { Field(ap, i) = sp[i]; }
      sp[n-1] = ap;
      Pop_n(n-1);
      Next;
    }

    Instr(NEWNAP): {
      value ap;
      nat   n;
      nat   i;
      Trace ("NEWNAP");

      #ifdef LVM_EAGER
      // if (sp - thread->stack_lim > min_eager_stack)
      // goto eager_ap;
      #endif
      n = *pc++;
      Require( sp + n <= fp );
      Allocate(ap,n,Nap_tag);
      for (i = 0; i < n; i++) { Field(ap, i) = sp[i]; }
      sp[n-1] = ap;
      Pop_n(n-1);
      Next;
    }

#ifdef LVM_EAGER
    eager_ap: {
      value* bp;
      nat n = *pc++;
      nat i;

      Trace_value( "eager evaluation: ", Val_code(pc) );
      /* insert an eager frame */
      Require( sp + n <= fp );
      Push_n(Frame_size);
      for( i = 0; i < n; i++) { sp[i] = sp[i+Frame_size]; }
      bp = sp;
      sp = sp + Frame_size + n;
      Push_frame_val( frame_eager, Val_code(pc) );
      sp = bp;

#ifdef LVM_EAGER_LIMITS
      if (thread->eager_top == NULL) {
        /* set up a stack bound */
        thread->eager_top = fp;
        thread->eager_stack_lim = thread->stack_lim;
        thread->eager_heap_lim  = young_ptr + max_eager_heap;
        thread->stack_lim = fp - max_eager_stack;
        if (thread->stack_lim < thread->eager_stack_lim) thread->stack_lim = thread->eager_stack_lim;
      }
#endif
      goto enter;
    }
#endif

    Instr(NEWAP1): {
      value ap;
      Trace ("NEWAP1");
      Require( sp + 1 <= fp );
      Alloc_small(ap,1,Ap_tag);
      Field(ap,0) = sp[0];
      sp[0] = ap;
      Next;
    }

    Instr(NEWAP2): {
      value ap;
      Trace ("NEWAP2");
      Require( sp + 2 <= fp );
      Alloc_small(ap,2,Ap_tag);
      Field(ap,0) = sp[0];
      Field(ap,1) = sp[1];
      Pop();
      sp[0] = ap;
      Next;
    }

    Instr(NEWAP3): {
      value ap;
      Trace ("NEWAP3");
      Require( sp + 3 <= fp );
      Alloc_small(ap,3,Ap_tag);
      Field(ap,0) = sp[0];
      Field(ap,1) = sp[1];
      Field(ap,2) = sp[2];
      Pop_n(2);
      sp[0] = ap;
      Next;
    }

    Instr(NEWAP4): {
      value ap;
      Trace ("NEWAP4");
      Require( sp + 4 <= fp );
      Alloc_small(ap,4,Ap_tag);
      Field(ap,0) = sp[0];
      Field(ap,1) = sp[1];
      Field(ap,2) = sp[2];
      Field(ap,3) = sp[3];
      Pop_n(3);
      sp[0] = ap;
      Next;
    }


    Instr(NEWNAP1): {
      value nap;
      Trace ("NEWNAP1");
      Require( sp + 1 <= fp );
      Alloc_small(nap,1,Nap_tag);
      Field(nap,0) = sp[0];
      sp[0] = nap;
      Next;
    }

    Instr(NEWNAP2): {
      value nap;
      Trace ("NEWNAP2");
      Require( sp + 2 <= fp );
      Alloc_small(nap,2,Nap_tag);
      Field(nap,0) = sp[0];
      Field(nap,1) = sp[1];
      Pop();
      sp[0] = nap;
      Next;
    }

    Instr(NEWNAP3): {
      value nap;
      Trace ("NEWNAP3");
      Require( sp + 3 <= fp );
      Alloc_small(nap,3,Nap_tag);
      Field(nap,0) = sp[0];
      Field(nap,1) = sp[1];
      Field(nap,2) = sp[2];
      Pop_n(2);
      sp[0] = nap;
      Next;
    }

    Instr(NEWNAP4): {
      value nap;
      Trace ("NEWNAP4");
      Require( sp + 4 <= fp );
      Alloc_small(nap,4,Nap_tag);
      Field(nap,0) = sp[0];
      Field(nap,1) = sp[1];
      Field(nap,2) = sp[2];
      Field(nap,3) = sp[3];
      Pop_n(3);
      sp[0] = nap;
      Next;
    }


/*----------------------------------------------------------------------
  General sums and products
----------------------------------------------------------------------*/
    Instr(GETFIELD): {
      value v  = sp[0];
      long  i  = Long_val(sp[1]);
      long sz;
      Trace ("GETFIELD");
      Require( Is_block(v) && Is_long(sp[1]) );
      Con_size_val(sz,v);
      if (sz <= i) { Raise_runtime_exn( Exn_out_of_bounds ); }
      sp[1] = Field(v,i);
      Pop();
      Next;
    }

    Instr(SETFIELD): {
      value v  = sp[0];
      long  i  = Long_val(sp[1]);
      value x  = sp[2];
      long sz;
      Trace ("SETFIELD");
      Require( Is_block(v) && Is_long(sp[1]) );
      Con_size_val(sz,v);
      if (sz <= i) { Raise_runtime_exn( Exn_out_of_bounds ); }
      Store_field(v,i,x);
      Pop_n(3);
      Next;
    }

    Instr(ALLOC): {
      long tag  = Long_val(sp[0]);
      long size = Long_val(sp[1]);
      long i;
      value con;
      Trace ("ALLOC");
      if (size < 0) { Raise_runtime_exn( Exn_out_of_bounds ); }
      Alloc_con(con,(nat)size,tag);
      for( i = 0; i < size; i++ ) { Field(con,i) = 0; }
      sp[1] = con;
      Pop();
      Next;
    }

    Instr(NEW): {
      long size = *pc++;
      long tag  = Long_val(sp[0]);
      long i;
      value con;
      Trace ("NEW");
      Pop();
      if (size < 0) { Raise_runtime_exn( Exn_out_of_bounds ); }
      Alloc_con(con,(nat)size,tag);
      for( i = 0; i < size; i++ ) { Field(con,i) = sp[i]; }
      Pop_n(size);
      Push(con);
      Next;
    }

    Instr(GETTAG): {
      Trace ("GETTAG");
      Require( Is_block(sp[0]) );
      sp[0] = Val_long( Tag_val(sp[0]) );
      Next;
    }

    Instr(GETSIZE): {
      long sz;
      Trace ("GETSIZE");
      Require( Is_block(sp[0]) );
      Con_size_val(sz,sp[0]);
      sp[0] = Val_long( sz );
      Next;
    }

    Instr(PACK): {
      long  n = *pc++;
      value v = sp[0];
      long  sz;
      long  i;
      Trace ("PACK");
      Require( Is_block(v) );
      Con_size_val(sz,v);
      if (n >= sz) { Raise_runtime_exn( Exn_out_of_bounds ); }
      Pop();
      for( i = 0; i < n; i++) { Field(v,i) = sp[i]; }
      Pop_n(n);
      Next;
    }

    Instr(UNPACK): {
      long  n  = *pc++;
      value v  = sp[0];
      long  sz;
      long  i;
      Trace ("UNPACK");
      Require( Is_block(v) );
      Con_size_val(sz,v);
      if (n > sz) { Raise_runtime_exn( Exn_out_of_bounds ); }
      Pop();
      Push_n(n);
      for( i = 0; i < n; i++) { sp[i] = Field(v,i); }
      Next;
    }

/*----------------------------------------------------------------------
  Constructors
----------------------------------------------------------------------*/
    Instr(ALLOCCON): {
      value con;
      long  tag = *pc++;
      nat n   = *pc++;
      Trace ("ALLOCCON");
      if (n == 0) {
        con = Atom(tag);
      }
      else {
        nat i;
        Allocate(con,n,tag)
        for( i = 0; i < n; i++ ) { Field(con,i) = 0; }
      }
      Push(con);
      Next;
    }

    Instr(PACKCON): {
      long ofs  = *pc++;
      long n    = *pc++;
      long i;
      value con;
      Trace ("PACKCON");
      Require( sp + n <= fp && sp + ofs < fp);
      con = sp[ofs];
      Require( Is_block(con) && Tag_val(con) <= Con_max_tag );
      for( i = 0; i < n; i++) { Store_field(con,i,sp[i]); }
      Pop_n(n);
      Next;
    }

    Instr(NEWCON): {
      value con;
      nat tag = *pc++;
      nat n   = *pc++;
      nat i;
      Trace ("NEWCON");
      Require( sp + n <= fp );
      if (n == 0) con = Atom(tag);
             else Allocate(con,n,tag);
      for (i = 0; i < n; i++) { Field(con,i) = sp[i]; }
      sp[n-1] = con;
      Pop_n(n-1);
      Next;
    }

    Instr(NEWCON0): {
      nat tag = *pc++;
      Trace ("NEWCON0");
      Push(Atom(tag));
      Next;
    }

    Instr(NEWCON1): {
      nat tag = *pc++;
      value con;
      Trace ("NEWCON1");
      Alloc_small(con,1,tag);
      Field(con,0) = sp[0];
      sp[0] = con;
      Next;
    }

    Instr(NEWCON2): {
      nat tag = *pc++;
      value con;
      Trace ("NEWCON2");
      Alloc_small(con,2,tag);
      Field(con,0) = sp[0];
      Field(con,1) = sp[1];
      sp[1] = con;
      Pop();
      Next;
    }

    Instr(NEWCON3): {
      nat tag = *pc++;
      value con;
      Trace ("NEWCON3");
      Alloc_small(con,3,tag);
      Field(con,0) = sp[0];
      Field(con,1) = sp[1];
      Field(con,2) = sp[2];
      sp[2] = con;
      Pop_n(2);
      Next;
    }


    Instr(TESTCON): {
      nat tag   = *pc++;
      nat ofs   = *pc++;
      Trace ("TESTCON");
      Require( Is_block(sp[0]) && Tag_val(sp[0]) <= Con_max_tag );
      if (Tag_val(sp[0]) != tag) pc += ofs;
      Next;
    }

    Instr(UNPACKCON): {
      nat n     = *pc++;
      value con = sp[0];
      Trace ("UNPACKCON");
      Require( Is_block(con) && Tag_val(con) <= Con_max_tag && Wosize_val(con) == n );
      Push_n(n);
      while (n > 0) { n--; sp[n] = Field(con,n); }
      Next;
    }


/*----------------------------------------------------------------------
  Integer operations
----------------------------------------------------------------------*/
    Instr(TESTINT): {
      long i   = *pc++;
      nat ofs  = *pc++;
      Trace ("TESTINT");
      if (sp[0] != Val_long(i)) pc += ofs;
      Next;
    }

    Instr(ADDINT): {
    #if defined(LVM_CHECK_BOUNDS)
      long i;
    #endif
      Trace ("ADDINT");
      Require( Is_long(sp[0]) && Is_long(sp[1]) );
    #if defined(LVM_CHECK_BOUNDS)
      i = Long_val(sp[0]) + Long_val(sp[1]);
      Pop();
      if (i > Max_long) Raise_arithmetic_exn( Int_overflow );
      if (i < Min_long) Raise_arithmetic_exn( Int_underflow );
      sp[0] = Val_long(i);
    #else
      Require( Is_long(sp[0]) && Is_long(sp[1]) );
      sp[1] = (value)( (long)sp[0] + (long)sp[1] - 1 );
      Pop();
    #endif
      Next;
    }

    Instr(SUBINT): {
    #if defined(LVM_CHECK_BOUNDS)
      long i = Long_val(sp[0]) - Long_val(sp[1]);
      Trace ("SUBINT");
      Pop();
      if (i > Max_long) Raise_arithmetic_exn( Int_overflow );
      if (i < Min_long) Raise_arithmetic_exn( Int_underflow );
      sp[0] = Val_long(i);
    #else
      Trace ("SUBINT");
      sp[1] = (value)( (long)sp[0] - (long)sp[1] + 1 );
      Pop();
    #endif
      Next;
    }

    Instr(MULINT): {
    #if defined(LVM_CHECK_BOUNDS)
      #define Max_half_long  (Max_long >> (sizeof(value)*4))

      long x = Long_val(sp[0]);
      long y = Long_val(sp[1]);
      long r = x*y;
      Trace ("MULINT");
      Pop();
      /* has the result overflowed a long? */
      if (x != 0 && y != 0 &&  (x > Max_half_long || y > Max_half_long)) { /* cheap test */
        if (r/x != y || r/y != x) { /* real test */
          if ((x > 0 && y < 0) || (x < 0 && y > 0)) {
            Raise_arithmetic_exn( Int_underflow );
          } else {
            Raise_arithmetic_exn( Int_overflow );
          }
        }
      }
      /* has the result overflowed a value? */
      if (r > Max_long) { Raise_arithmetic_exn( Int_overflow ); }
      if (r < Min_long) { Raise_arithmetic_exn( Int_underflow ); }

      sp[0] = Val_long( r );
    #else
      Trace ("MULINT");
      sp[1] = Val_long( Long_val(sp[0]) * Long_val(sp[1]) );
      Pop();
    #endif
      Next;
    }

    /* QuotInt and RemInt use truncated division, ie.
       QuotInt D d = trunc(D/d)
       RemInt D d  = D - d*(QuotInt D d)
    */
    Instr(QUOTINT): {
      long divisor = Long_val(sp[1]);
      Trace ("QUOTINT");
      if (divisor == 0) Raise_arithmetic_exn( Int_zerodivide );
      sp[1] = Val_long( Long_val(sp[0]) / divisor );
      Pop();
      Next;
    }

    Instr(REMINT): {
      long divisor = Long_val(sp[1]);
      Trace ("REMINT");
      if (divisor == 0) Raise_arithmetic_exn( Int_zerodivide );
      sp[1] = Val_long( Long_val(sp[0]) % divisor );
      Pop();
      Next;
    }

    /* DivInt and ModInt use euclidean division, ie.
       the modulus is always positive.
    */
    Instr(DIVINT): {
      /* round towards negative infinity */
      long divisor = Long_val(sp[1]);
      long div;
      long mod;
      Trace ("DIVINT");

      if (divisor == 0) { Raise_arithmetic_exn( Int_zerodivide );}
      div = Long_val(sp[0]) / divisor;
      mod = Long_val(sp[0]) % divisor;

      /* floored division: if ((divisor < 0 && mod > 0) || (divisor > 0 && mod < 0)) div--; */
      if (mod < 0) {
       if (divisor > 0) div = div-1;
                   else div = div+1;
      }

      sp[1] = Val_long(div);
      Pop();
      Next;
    }

    Instr(MODINT): {
      /* modulo is always positive */
      long divisor = Long_val(sp[1]);
      long mod;
      Trace ("MODINT");

      if (divisor == 0) { Raise_arithmetic_exn( Int_zerodivide ); }
      mod = Long_val(sp[0]) % divisor;

      /* floored modulus: if ((divisor < 0 && mod > 0) || (divisor > 0 && mod < 0)) mod = mod + div; // was: divisor; */
      if (mod < 0) {
        if (divisor > 0) mod = mod + divisor;
                    else mod = mod - divisor;
      }

      sp[1] = Val_long(mod);
      Pop();
      Next;
    }

    Instr(NEGINT): {
    #if defined(LVM_CHECK_BOUNDS)
      long i = - Long_val(sp[0]);
      Trace ("NEGINT");
      if (i > Max_long) Raise_arithmetic_exn( Int_overflow );
      if (i < Min_long) Raise_arithmetic_exn( Int_underflow );
      sp[0] = Val_long(i);
    #else
      Trace ("NEGINT");
      sp[0] = (value)(2 - (long)sp[0]);
    #endif
      Next;
    }

    Instr(INCINT): {
    #if defined(LVM_CHECK_BOUNDS)
      long i = Long_val(sp[0]) + (long)(*pc++);
      Trace ("INCINT");
      if (i > Max_long) Raise_arithmetic_exn( Int_overflow );
      if (i < Min_long) Raise_arithmetic_exn( Int_underflow );
      sp[0] = Val_long(i);
    #else
      Trace ("INCINT");
      sp[0] = (value)( (long)sp[0] + 2*(long)(*pc++) );
    #endif
      Next;
    }

/*----------------------------------------------------------------------
  Bitwise integer operations
----------------------------------------------------------------------*/
    Instr(ANDINT): {
      Trace ("ANDINT");
      sp[1] = (value)( (long)sp[0] & (long)sp[1] );
      Pop();
      Next;
    }

    Instr(XORINT): {
      Trace ("XORINT");
      sp[1] = (value)( ((long)sp[0] ^ (long)sp[1]) | 1 );
      Pop();
      Next;
    }

    Instr(ORINT): {
      Trace ("ORINT");
      sp[1] = (value)( (long)sp[0] | (long)sp[1] );
      Pop();
      Next;
    }

    Instr(SHRINT): {
      Trace ("SHRINT");
      sp[1] = (value)( ((long)sp[0] >> Long_val(sp[1])) | 1);
      Pop();
      Next;
    }

    Instr(SHLINT): {
      Trace ("SHLINT");
      sp[1] = (value)( (((long)sp[0]-1) << Long_val(sp[1])) | 1);
      Pop();
      Next;
    }

    Instr(SHRNAT): {
      Trace ("SHRNAT");
      sp[1] = (value)( ((unsigned long)sp[0] >> Long_val(sp[1]))| 1);
      Pop();
      Next;
    }

/*----------------------------------------------------------------------
  Compare integer operations
----------------------------------------------------------------------*/
#define Compare_instr(sign,opname,tst) \
    Instr(opname): { \
      sp[1] = Val_bool((sign long)sp[0] tst (sign long)sp[1]); \
      Pop(); \
      Next; }

    Compare_instr(signed,EQINT,==)
    Compare_instr(signed,NEINT,!=)
    Compare_instr(signed,LTINT,<)
    Compare_instr(signed,GTINT,>)
    Compare_instr(signed,LEINT,<=)
    Compare_instr(signed,GEINT,>=)


/*----------------------------------------------------------------------
  Call External functions
----------------------------------------------------------------------*/
    Instr(CALL): {
      value v;
      value decl = *(Valptr_fixup(*pc++));
      nat n      = *pc++;
      Trace ("CALL");
      Require( Is_block(decl) && Tag_val(decl) == Rec_extern );

      /* check number of arguments */
      if (sp + n > fp) {
        raise_internal( "extern call: too few arguments" );
      }

      Setup_for_exn();
      v  = call_extern( sp, n                                     /* args & #args */
                      , Ptr_val(Field(decl,Field_extern_fun))     /* address */
                      , Int_val(Field(decl,Field_extern_call))    /* calling convention */
                      , Field(Field(decl,Field_extern_type),Field_extern_type_string) /* type string */
                      , Field(Field(decl,Field_name),Field_name_string) );  /* debug: the name */
      Restore_after_exn();

      Pop_n(n);
      Push(v);
      Next;
    }


/*----------------------------------------------------------------------
  Default
----------------------------------------------------------------------*/
    Instr(STOP):  /* fall through */
#if !defined(THREADED_CODE)
    default:
#endif
    {
      opcode_t opcode = pc[-1];

#if defined(THREADED_CODE)
      enum instruction i = ARGCHK;
      while (i <= STOP && instr_table[i] != Ptr_fixup(opcode)) i++;
      if (i <= STOP) opcode = i;
#endif
      if (opcode >= 0 && opcode <= STOP)
        todo( instr_name(opcode) );
      else
        raise_invalid_opcode( pc[-1] );
      Next;
    }
  } /* switch(*pc++) */
  } /* while(1) */

  Require(0);
  fatal_error( "fatal error: corrupted code -- execution out of the evaluation loop!" );
}
