/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */
#ifndef _primfloat_h
#define _primfloat_h

/*----------------------------------------------------------------------
-- IEEE floating point interface
----------------------------------------------------------------------*/
enum fp_round {
  fp_round_near,
  fp_round_up,
  fp_round_down,
  fp_round_zero,

  fp_round_count
};

enum fp_round fp_get_round( void );
enum fp_round fp_set_round( enum fp_round rnd );


enum fp_exception {
  fp_ex_invalid,
  fp_ex_zerodivide,
  fp_ex_overflow,
  fp_ex_underflow,
  fp_ex_inexact,
  fp_ex_denormal,

  fp_ex_count
};

long fp_sticky_mask( enum fp_exception ex );
long fp_get_sticky( void );
long fp_set_sticky( long sticky );

long fp_trap_mask( enum fp_exception ex );
long fp_get_traps( void );
long fp_set_traps( long traps );

void fp_reset( void );

#endif /* _primfloat_h */