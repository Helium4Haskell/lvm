/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _primitives_h
#define _primitives_h

#define PRIMS \
  /* floating point control */ \
  PRIM(float_of_string,"Fz") \
  PRIM(float_of_int,"FI") \
  PRIM(string_of_float,"aFic") \
  \
  PRIM(fp_pow,  "FFF") \
  PRIM(fp_sqrt, "FF") \
  PRIM(fp_exp,  "FF") \
  PRIM(fp_log,  "FF") \
  PRIM(fp_sin,  "FF") \
  PRIM(fp_cos,  "FF") \
  PRIM(fp_tan,  "FF") \
  PRIM(fp_ceil, "FF") \
  PRIM(fp_floor,"FF") \
  PRIM(fp_round,"FF") \
  PRIM(fp_trunc,"FF") \
  PRIM(fp_near, "FF")  \
  \
  PRIM(fp_round_int,"IF") \
  PRIM(fp_trunc_int,"IF") \
  PRIM(fp_near_int, "IF") \
  \
  PRIM(fp_reset       ,"v" ) \
  PRIM(fp_sticky_mask ,"li") \
  PRIM(fp_set_sticky  ,"ll") \
  PRIM(fp_get_sticky  ,"l" ) \
  PRIM(fp_trap_mask   ,"li") \
  PRIM(fp_set_traps   ,"ll") \
  PRIM(fp_get_traps   ,"l" ) \
  PRIM(fp_set_round   ,"ii") \
  PRIM(fp_get_round   ,"i") \
  \
  /* file handle's */ \
  PRIM(prim_flag_mask,"ll") \
  PRIM(prim_open,"lzll")     \
  PRIM(prim_close,"vl")     \
  \
  PRIM(prim_open_descriptor,"alb")  \
  PRIM(prim_close_channel,"va")     \
  PRIM(prim_set_binary_mode, "vab" )\
  PRIM(prim_flush_partial, "ba" )   \
  PRIM(prim_flush, "va" )           \
  PRIM(prim_output_char, "vac" )    \
  PRIM(prim_output,"vazll")         \
  PRIM(prim_input_char, "la" )      \
  \
  /* string/list conversion */      \
  PRIM(prim_string_of_chars, "ala" )\
  PRIM(prim_chars_of_string, "aa" ) \
  PRIM(prim_string_length, "la" )

#endif
