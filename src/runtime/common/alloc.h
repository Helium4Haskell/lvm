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

/***---------------------------------------------------------------------
  Modified and adapted for the Lazy Virtual Machine by Daan Leijen.
  Modifications copyright 2001, Daan Leijen. This (modified) file is
  distributed under the terms of the GNU Library General Public License.
---------------------------------------------------------------------***/

/* $Id$ */

#ifndef _alloc_
#define _alloc_

#include "mlvalues.h"

value alloc (mlsize_t, tag_t);
value alloc_small (mlsize_t, tag_t);
value alloc_tuple (mlsize_t);
value alloc_string (mlsize_t);
value copy_string (const char *);
value copy_string_array (const char **);
value copy_double (double);
value copy_int32 (int32);       /* defined in [ints.c] */
value copy_int64 (int64);       /* defined in [ints.c] */
value copy_nativeint (long);    /* defined in [ints.c] */
value alloc_array (value (*funct) (const char *), const char ** array);

typedef void (*final_fun)(value);
value alloc_final (mlsize_t /*size in words*/,
                   final_fun, /*finalization function*/
                   mlsize_t, /*resources consumed*/
                   mlsize_t  /*max resources*/);

int convert_flag_list (value, int *);

#endif /* _alloc_ */