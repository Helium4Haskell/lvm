/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */
#ifndef _primfloat_h
#define _primfloat_h

float_t float_of_string( const char* s );

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

long fp_sticky_mask( enum exn_arithmetic ex );
long fp_get_sticky( void );
long fp_set_sticky( long sticky );

long fp_trap_mask( enum exn_arithmetic ex );
long fp_get_traps( void );
long fp_set_traps( long traps );

/* the following two functions can be called from
   a signal handler and should always succeed */
long fp_clear( void );
void fp_reset( void );
void fp_save( long* sticky, long* traps );
void fp_restore( long sticky, long traps );

#endif /* _primfloat_h */