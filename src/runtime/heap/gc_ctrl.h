/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
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

#ifndef _gc_ctrl_
#define _gc_ctrl_

#include "misc.h"

/* LVM: changed [long] to [unsigned long] */
extern unsigned long
     stat_minor_words,
     stat_promoted_words,
     stat_major_words,
     stat_minor_collections,
     stat_major_collections,
     stat_heap_size,
     stat_compactions,
     stat_max_heap_size,
     stat_peak_heap_bsize;

void gc_full_major(void);
void done_gc (void);
void init_gc (unsigned long, unsigned long, unsigned long,
              unsigned long, unsigned long, unsigned long, unsigned long );

#ifdef DEBUG
void heap_check (void);
#endif

#endif /* _gc_ctrl_ */