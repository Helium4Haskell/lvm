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

/* Id: roots.c,v 1.1 2001/04/11 14:00:09 daan Exp $ */

/* To walk the memory roots for garbage collection */

#include "mlvalues.h"
#include "finalise.h"
#include "heap.h"
#include "memory.h"
#include "roots.h"
#include "fixed.h"
#include "core/thread.h"

/* declared in "fixed.h" */
extern struct fixed_block* fixed_blocks;

struct caml__roots_block *local_roots = NULL;

/* FIXME turn this into a table and synchronise with asmrun/roots.c */
struct global_root {
  value * root;
  struct global_root * next;
};

static struct global_root * global_roots = NULL;

void (*scan_roots_hook) (scanning_action f) = NULL;

/* Register a global C root */

void register_global_root(value *r)
{
  struct global_root * gr;

  Assert (((long) r & 3) == 0);  /* compact.c demands this (for now) */
  gr = (struct global_root *) stat_alloc(sizeof(struct global_root));
  gr->root = r;
  gr->next = global_roots;
  global_roots = gr;
}

/* Un-register a global C root */

void remove_global_root(value *r)
{
  struct global_root ** gp, * gr;
  for (gp = &global_roots; *gp != NULL; gp = &(*gp)->next) {
    gr = *gp;
    if (gr->root == r) {
      *gp = gr->next;
      stat_free(gr);
      return;
    }
  }
}

/* FIXME rename to [oldify_young_roots] and synchronise with asmrun/roots.c */
/* Call [oldify] on (at least) all the roots that point to the minor heap. */
void oldify_local_roots (void)
{
  register value * sp;
  register value * vp;
  struct global_root * gr;
  struct caml__roots_block *lr;
  struct thread_state* thread;
  struct fixed_block*  fixed;
  long i, j;

  /* The stack */
  /*
  for (sp = extern_sp; sp < stack_high; sp++) {
    oldify (*sp, sp);
  }
  */

  /* The threads */
  for( thread = threads; thread != NULL; thread = thread->next ) {
    oldify(thread->module,&(thread->module));
    for (sp = thread->stack_sp; sp < thread->stack_top; sp++) {
      oldify( *sp, sp );
    }
  }

  /* The fixed memory blocks */
  for (fixed = fixed_blocks; fixed != NULL; fixed = fixed->next ) {
    for( vp = fixed->data; vp < fixed->data + fixed->size; vp++ ) {
      oldify( *vp, vp );
    }
  }

  /* Local C roots */  /* FIXME do the old-frame trick ? */
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        oldify (*sp, sp);
      }
    }
  }
  /* Global C roots */
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    oldify(*(gr->root), gr->root);
  }
  /* Finalised values */
  final_do_young_roots (&oldify);
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(&oldify);
}

/* Call [darken] on all roots */

void darken_all_roots (void)
{
  do_roots (darken);
}

void do_roots (scanning_action f)
{
  struct global_root * gr;

  /* Global variables */
  /* f(global_data, &global_data); */

  /* The threads, fixed blocks and the local C roots */
  do_local_roots(f, threads, fixed_blocks, local_roots);

  /* Global C roots */
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    f (*(gr->root), gr->root);
  }
  /* Finalised values */
  final_do_strong_roots (f);
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(f);
}

void do_local_roots (scanning_action f,
                     struct thread_state* threads,
                     struct fixed_block*  fixed_blocks,
                     struct caml__roots_block *local_roots)
{
  register value * sp;
  register value * vp;
  struct caml__roots_block *lr;
  struct thread_state*  thread;
  struct fixed_block*   fixed;
  int i, j;

  /*
  for (sp = stack_low; sp < stack_high; sp++) {
    f (*sp, sp);
  }
  */
  /* The threads */
  for( thread = threads; thread != NULL; thread = thread->next ) {
    f(thread->module,&(thread->module));
    for (sp = thread->stack_sp; sp < thread->stack_top; sp++) {
      f( *sp, sp );
    }
  }

  /* The fixed memory blocks */
  for (fixed = fixed_blocks; fixed != NULL; fixed = fixed->next ) {
    for( vp = fixed->data; vp < fixed->data + fixed->size; vp++ ) {
      f( *vp, vp );
    }
  }


  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        f (*sp, sp);
      }
    }
  }
}
