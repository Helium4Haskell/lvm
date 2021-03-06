/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
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

#include "roots.h"

void final_update (void);
void final_do_calls (void);
void final_do_strong_roots (scanning_action f);
void final_do_weak_roots (scanning_action f);
void final_do_young_roots (scanning_action f);
void final_empty_young (void);
value final_register (value f, value v);
