/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <stdlib.h>
#include "mlvalues.h"
#include "fail.h"

#include "heap/heapfast.h"
#include "alloc.h"
#include "primfloat.h"
#include "math.h"

#ifdef HAS_FLOAT_H
#ifdef __MINGW32__
#include <../mingw/float.h>
#else
#include <float.h>
#endif
#endif

#ifdef HAS_IEEEFP_H
#include <ieeefp.h>
#endif

/*----------------------------------------------------------------------
-- portable functions
----------------------------------------------------------------------*/
#ifdef ARCH_ALIGN_DOUBLE

double Double_val(value val)
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.v[0] = Field(val, 0);
  buffer.v[1] = Field(val, 1);
  return buffer.d;
}

void Store_double_val(value val, double dbl)
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.d = dbl;
  Field(val, 0) = buffer.v[0];
  Field(val, 1) = buffer.v[1];
}

#endif

value copy_double(double d)
{
  value res;

#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

float_t float_of_int( long i )
{
  return (float_t)i;
}

float_t float_of_string( const char* s )
{
  return atof(s);
}

value string_of_float( float_t f, int prec, char type )
{
  char buffer[144];
  if (prec>128) prec = 128;

  switch (type) {
   case 'e': snprintf( buffer, 144, "%.*e", prec, f ); break;
   case 'E': snprintf( buffer, 144, "%.*E", prec, f ); break;
   case 'f': snprintf( buffer, 144, "%.*f", prec, f ); break;
   case 'F': snprintf( buffer, 144, "%.*F", prec, f ); break;
   case 'G': snprintf( buffer, 144, "%.*G", prec, f ); break;
   case 'g': 
   default : snprintf( buffer, 144, "%.*g", prec, f ); break;
  }

  return copy_string(buffer);
}


double fp_pow( double x, double y )
{
  return pow(x,y);
}

double fp_sqrt( double x )
{
  return sqrt(x);
}

double fp_exp( double x )
{
  return exp(x);
}

double fp_log( double x )
{
  return log(x);
}

double fp_sin( double x )
{
  return sin(x);
}

double fp_cos( double x )
{
  return cos(x);
}

double fp_tan( double x )
{
  return tan(x);
}



/*----------------------------------------------------------------------
-- Each platform defines an array that maps [fp_exception] to bit masks
----------------------------------------------------------------------*/
#define Fp_exn_count  (Fpe_denormal + 1)

static long fp_sticky_masks[Fp_exn_count];
static long fp_trap_masks[Fp_exn_count];
static long fp_round_masks[fp_round_count];

long  fp_sticky_mask( enum exn_arithmetic ex )
{
  if (ex < 0 || ex >= Fp_exn_count) {
    raise_invalid_argument( "fp_sticky_mask" ); return 0;
  }else
    return fp_sticky_masks[ex];
}

long  fp_trap_mask( enum exn_arithmetic ex )
{
  if (ex < 0 || ex >= Fp_exn_count) {
    raise_invalid_argument( "fp_trap_mask" ); return 0;
  }else
    return fp_trap_masks[ex];
}

long fp_round_mask( enum fp_round rnd )
{
  if (rnd < 0 || rnd >= fp_round_count) {
    raise_invalid_argument( "fp_round_mask" ); return 0;
  }else
    return fp_round_masks[rnd];
}

enum fp_round fp_round_unmask( long rnd )
{
  enum fp_round i;
  for( i = 0; i < fp_round_count; i++) {
    if (fp_round_masks[i] == rnd) return i;
  }
  return fp_round_near;
}

/*----------------------------------------------------------------------
  The IEEE conformance on i386 platforms is bizar:
  - Microsoft Visual C and Borland C provide no mechanism
    to set the sticky flags. On top of that, the MS functions are
    very inefficient since they convert all the bits to some other
    (DEC alpha?) format.
  - Mingw32 does have a MS conformant [float.h] but it conflicts with
    the default GNU [float.h].
  - Cygwin does have [ieefp.h] but no library that implements it.
  - FreeBSD [fpsetsticky] clears all the sticky flags instead of setting them.
  - NetBSD [fpsetsticky] sets the control word instead of the status word,
    effectively enabling SIG_FPE signals instead of setting sticky flags..

  Incredible, especially when we consider that the x87 was the driving
  force behind the IEEE 754 standard. Because of all these problems,
  We implement floating point support in assembly by default on the IA32 
  platforms. Surprisingly, it is quite easy to do which makes one wonder 
  why the implementations are so diverse.
----------------------------------------------------------------------*/
#if defined(ARCH_IA32)

#if defined(_MSC_VER)
# define FLOAT_ASM_IA32
# define asm_fnclex()     __asm{ fnclex }
# define asm_fstcw(cw)    __asm{ fstcw cw }
# define asm_fldcw(cw)    __asm{ fldcw cw }
# define asm_fstsw(sw)    __asm{ fstsw sw }
# define asm_fnstsw(sw)   __asm{ fnstsw sw }
# define asm_fldenv(env)  __asm{ fldenv env }
# define asm_fstenv(env)  __asm{ fstenv env }
# define asm_frndint(r,x) __asm{ fld x }; __asm{ frndint }; __asm{ fstp r }
# define asm_fist(r,x)    __asm{ fld x }; __asm{ fistp r }
#elif defined(__GNUC__)
# define FLOAT_ASM_IA32
# define asm_fnclex()     __asm __volatile("fnclex")
# define asm_fstcw(cw)    __asm __volatile("fstcw %0" : "=m" (cw))
# define asm_fldcw(cw)    __asm __volatile("fldcw %0" : : "m" (cw))
# define asm_fstsw(sw)    __asm __volatile("fstsw %0" : "=m" (sw))
# define asm_fnstsw(sw)   __asm __volatile("fnstsw %0" : "=m" (sw))
# define asm_fldenv(env)  __asm __volatile("fldenv %0" : : "m" (*(env)))
# define asm_fstenv(env)  __asm __volatile("fstenv %0" : "=m" (*(env)))
# define asm_frndint(r,x) __asm __volatile("frndint"  : "=t" (r) : "0" (x));
# define asm_fist(r,x)    __asm __volatile("fistl %0" : "=m" (r) : "t" (x));
#else
  /* no assembler support, use default implementations */
#endif

#endif


/*----------------------------------------------------------------------
-- Rounding: use asm on i386 platforms
----------------------------------------------------------------------*/
#if defined(FLOAT_ASM_IA32)

double fp_round( double x )
{
  volatile double r;
  asm_frndint(r,x);
  return r;
}

long fp_round_int( double x )
{
  volatile long r;
  asm_fist(r,x);
  return r;
}


#define fp_round_with(r,x,rnd) { enum fp_round save = fp_set_round( rnd ); \
                                 (r) = fp_round(x); \
                                 fp_set_round(save); \
                               }

double fp_floor( double x )
{
  double r;
  fp_round_with(r,x,fp_round_down);
  return r;
}

double fp_ceil( double x )
{
  double r;
  fp_round_with(r,x,fp_round_up);
  return r;
}

double fp_trunc( double x )
{
  double r;
  fp_round_with(r,x,fp_round_zero);
  return r;
}

double fp_nearest( double x )
{
  double r;
  fp_round_with(r,x,fp_round_near);
  return r;
}

long fp_trunc_int( double x )
{
  enum fp_round save = fp_set_round( fp_round_zero );
  long          r    = fp_round_int(x);
  fp_set_round(save);
  return r;
}

/*----------------------------------------------------------------------
-- Rounding: portable C
----------------------------------------------------------------------*/
#else

double fp_floor( double x )
{
  return floor(x);
}

double fp_ceil( double x )
{
  return ceil(x);
}

double fp_trunc( double x )
{
  double i;
  modf(x,&i);
  return i;
}

double fp_nearest( double x )
{
  double y = x+0.5;       /* x + 0.5 */
  double z = fp_floor(y); /* result  */
  if (y==z && (y/2.0)!=fp_floor(y/2.0)) z = z-1.0;
  return z;
}

double fp_round( double x )
{
  switch (fp_get_round()) {
    case fp_round_up:   return fp_ceil(x);
    case fp_round_down: return fp_floor(x);
    case fp_round_zero: return fp_trunc(x);
    case fp_round_near:
    default:            return fp_nearest(x);
  }
}

long fp_round_int( double x )
{
  return (long)fp_round(x);
}

long fp_trunc_int( double x )
{
  return (long)(x);
}

#endif

/*----------------------------------------------------------------------
-- IEEE floating point on i386 platforms
----------------------------------------------------------------------*/
#if defined(FLOAT_ASM_IA32)

#define _FP_STICKY_MASK 0x003f
#define _FP_TRAP_MASK   0x003f
#define _FP_ROUND_MASK  0x0c00

#define _FP_STATUS_REG  1
#define _FP_ENV_SIZE    10  /* actually 7, but just to be safe, we take a larger value */

#define _FP_X_INV  0x01
#define _FP_X_DNML 0x02
#define _FP_X_DZ   0x04
#define _FP_X_OFL  0x08
#define _FP_X_UFL  0x10
#define _FP_X_IMP  0x20

#define _FP_RN     0x0000
#define _FP_RM     0x0400
#define _FP_RP     0x0800
#define _FP_RZ     0x0c00

typedef unsigned int fp_reg;

static long fp_sticky_masks[Fp_exn_count] =
  { _FP_X_INV, _FP_X_DZ, _FP_X_OFL, _FP_X_UFL, _FP_X_IMP, _FP_X_DNML };

static long fp_trap_masks[Fp_exn_count] =
  { _FP_X_INV, _FP_X_DZ, _FP_X_OFL, _FP_X_UFL, _FP_X_IMP, _FP_X_DNML };

static long fp_round_masks[fp_round_count] =
  { _FP_RN, _FP_RP, _FP_RM, _FP_RZ };


long fp_get_sticky( void )
{
  volatile fp_reg sw;
  asm_fstsw(sw);
  return (sw & _FP_STICKY_MASK);
}

long fp_set_sticky( long sticky )
{
  volatile fp_reg sw;
  volatile fp_reg env[_FP_ENV_SIZE];
  asm_fstenv(env);
  sw                  = env[_FP_STATUS_REG];
  env[_FP_STATUS_REG] = (env[1] & ~_FP_STICKY_MASK) | (sticky & _FP_STICKY_MASK);
  if (sw != env[_FP_STATUS_REG]) { asm_fldenv(env); }
  return (sw & _FP_STICKY_MASK);
}


static fp_reg fp_control( fp_reg control, fp_reg mask, fp_reg resmask )
{
  volatile fp_reg cw;
  volatile fp_reg cwnew;
  asm_fstcw(cw);
  if (mask != 0) {
    cwnew = (cw & ~mask) | (control & mask);
    if (cwnew != cw) { asm_fldcw(cwnew); }
  }
  return (cw & resmask);
}

long fp_get_traps( void )
{
  return (~fp_control(0,0,_FP_TRAP_MASK) & _FP_TRAP_MASK);
}

long fp_set_traps( long traps )
{
  return (~fp_control((~traps & _FP_TRAP_MASK),_FP_TRAP_MASK,_FP_TRAP_MASK) & _FP_TRAP_MASK);
}

enum fp_round fp_get_round( void )
{
  return fp_round_unmask(fp_control(0,0,_FP_ROUND_MASK));
}

enum fp_round fp_set_round( enum fp_round rnd )
{
  return fp_round_unmask(fp_control(fp_round_mask(rnd),_FP_ROUND_MASK,_FP_ROUND_MASK));
}

long fp_clear(void)
{
  volatile fp_reg sw;
  asm_fnstsw(sw);
  asm_fnclex();
  return (sw & _FP_STICKY_MASK);
}

void fp_reset(void)
{
#ifdef HAS_CONTROLFP
  _fpreset();
#else
  fp_clear();
#endif
}


void fp_save( long* sticky, long* traps )
{
  Assert(sticky && traps);
  *traps  = fp_get_traps();
  *sticky = fp_get_sticky();
}

void fp_restore( long sticky, long traps )
{
  fp_set_traps(traps);
  fp_set_sticky(sticky);
}


/*----------------------------------------------------------------------
-- IEEE floating point on standard unix's
----------------------------------------------------------------------*/
#elif defined(HAS_IEEEFP_H)

#ifndef FP_X_DNML
#define FP_X_DNML 0
#endif

#ifndef FP_X_DZ
# ifdef FP_X_DX
#  define FP_X_DZ FP_X_DX
# else
#  define FP_X_DZ 0
# endif
#endif

static long fp_sticky_masks[Fp_exn_count] =
  { FP_X_INV, FP_X_DZ, FP_X_OFL, FP_X_UFL, FP_X_IMP, FP_X_DNML };

static long fp_trap_masks[Fp_exn_count] =
  { FP_X_INV, FP_X_DZ, FP_X_OFL, FP_X_UFL, FP_X_IMP, FP_X_DNML };

static long fp_round_masks[fp_round_count] =
  { FP_RN, FP_RP, FP_RM, FP_RZ };

void fp_reset( void )
{
  fp_clear();
}

long fp_clear()
{
  return fp_set_sticky(0);
}

/* sticky */
long fp_get_sticky(void)
{
  return fpgetsticky();
}

long fp_set_sticky( long sticky )
{
  return fpsetsticky( sticky );
}

/* traps */
long fp_get_traps(void)
{
  return fpgetmask();
}

long fp_set_traps( long traps )
{
  return fpsetmask(traps);
}

/* rounding */
enum fp_round fp_get_round( void )
{
  return fp_round_unmask( fpgetround() );
}

enum fp_round fp_set_round( enum fp_round rnd )
{
  return fp_round_unmask( fpsetround(fp_round_mask(rnd)) );
}


void fp_save( long* sticky, long* traps )
{
  Assert(sticky && traps);
  *traps  = fp_get_traps();
  *sticky = fp_get_sticky();
}

void fp_restore( long sticky, long traps )
{
  fp_set_traps(traps);
  fp_set_sticky(sticky);
}


/*----------------------------------------------------------------------
-- IEEE floating point on Visual C++/Mingw32/Borland systems
-- can't set the sticky flags :-(
----------------------------------------------------------------------*/
#elif defined(HAS_CONTROLFP)

static long fp_sticky_masks[Fp_exn_count] =
  { _EM_INVALID, _EM_ZERODIVIDE, _EM_OVERFLOW, _EM_UNDERFLOW, _EM_INEXACT, _EM_DENORMAL };

static long fp_trap_masks[Fp_exn_count] =
  { _EM_INVALID, _EM_ZERODIVIDE, _EM_OVERFLOW, _EM_UNDERFLOW, _EM_INEXACT, _EM_DENORMAL };

static long fp_round_masks[fp_round_count] =
  { _RC_NEAR, _RC_UP, _RC_DOWN, _RC_CHOP };

/* management */
void fp_reset( void )
{
  _fpreset();
}

long fp_clear( void )
{
  return _clearfp();
}

/* rounding */
enum fp_round fp_get_round( void )
{
  return fp_round_unmask( _controlfp(0,0) & _MCW_RC );
}

enum fp_round fp_set_round( enum fp_round rnd )
{
  return fp_round_unmask( _controlfp(fp_round_mask(rnd),_MCW_RC) & _MCW_RC );
}

/* traps */
long fp_get_traps( void )
{
  return ~(_controlfp(0,0) & _MCW_EM);
}

long fp_set_traps( long traps )
{
  return ~(_controlfp(~traps, _MCW_EM) & _MCW_EM);
}

/* sticky */
long fp_get_sticky( void )
{
  /* we could return it, but since we can't set the sticky bits,
     we can have indeterminate results with multiple threads
     through the [fp_save]/[fp_restore] calls. */
  raise_arithmetic_exn( Fpe_unemulated );
  return (_statusfp() & _MCW_EM);
}

/* sticky bits can only be set with assembly code :-( */
long fp_set_sticky( long sticky )
{
  raise_arithmetic_exn( Fpe_unemulated );
  return fp_get_sticky();
}


void fp_save( long* sticky, long* traps )
{
  Assert(sticky && traps);
  *traps = fp_get_traps();
}

void fp_restore( long sticky, long traps )
{
  fp_set_traps(traps);
}


/*----------------------------------------------------------------------
-- IEEE floating point unsupported
----------------------------------------------------------------------*/
#else
static long fp_sticky_masks[Fp_exn_count] =
  { 0,0,0,0,0,0 };

static long fp_trap_masks[Fp_exn_count] =
  { 0,0,0,0,0,0 };

static long fp_round_masks[fp_round_count] =
  { 0,0,0,0 };

static enum fp_round fp_reg_round = fp_round_near;

void fp_reset( void )
{
  return;
}

long fp_clear( void )
{
  return 0;
}

/* sticky */
long fp_get_sticky(void)
{
  raise_arithmetic_exn( Fpe_unemulated );
  return 0;
}

long fp_set_sticky( long sticky )
{
  raise_arithmetic_exn( Fpe_unemulated );
  return 0;
}

/* traps */
long fp_get_traps(void)
{
  raise_arithmetic_exn( Fpe_unemulated );
  return 0;
}

long fp_set_traps( long traps )
{
  raise_arithmetic_exn( Fpe_unemulated );
  return 0;
}

/* rounding */
enum fp_round fp_get_round( void )
{
  return fp_reg_round;
}

enum fp_round fp_set_round( enum fp_round rnd )
{
  enum fp_round old = fp_reg_round;
  fp_reg_round = rnd;
  return old;
}


void fp_save( long* sticky, long* traps )
{
  Assert(sticky && traps);
  /* nothing to do */
}

void fp_restore( long sticky, long traps )
{
  /* nothing to do */
}

#endif


/*
int main( int argc, char** argv )
{
  double x,y;
  long f1,f2,f3;
  fp_set_traps( fp_get_traps() | fp_trap_mask(fp_ex_inexact) );
  x = 1.0;
  y = 0.1;
  x = y + x;
  f1 = fp_get_sticky();
  f2 = fp_set_sticky( 0 );
  f3 = fp_get_sticky();
  printf( "%g, %x %x %x\n", x, f1, f2, f3 );

  return 0;
}
*/
