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

/* Allocation macros and functions */

#ifndef _memory_
#define _memory_

#include "mlvalues.h"

value alloc_shr (mlsize_t, tag_t);
void adjust_gc_speed (mlsize_t, mlsize_t);
void modify (value *, value);
void initialize (value *, value);
value check_urgent_gc (value);
void * stat_alloc (asize_t);              /* Size in bytes. */
void stat_free (void *);
void * stat_resize (void *, asize_t);     /* Size in bytes. */
char *alloc_for_heap (asize_t request);   /* Size in bytes. */
void free_for_heap (char *mem);
int add_to_heap (char *mem);
color_t allocation_color (void *hp);
void check_heap_size(void);
/* void shrink_heap (char *);        Only used in compact.c */

#ifdef NATIVE_CODE
#define Garbage_collection_function garbage_collection
#else
#define Garbage_collection_function minor_collection
#endif

#ifdef DEBUG
#define DEBUG_clear(result, wosize) { \
  wsize_t __DEBUG_i; \
  for (__DEBUG_i = 0; __DEBUG_i < wosize; ++ __DEBUG_i){ \
    Field (result, __DEBUG_i) = Debug_uninit_minor; \
  } \
}
#else
#define DEBUG_clear(result, wosize)
#endif

/*---------------------------------------------------------
   Convenience macros
---------------------------------------------------------*/
#define Store_field(block, offset, val) modify (&Field (block, offset), val)
#define Init_field(block, offset, val)  initialize( &Field(block,offset), val )


/*---------------------------------------------------------
   is_heap_val: is this a heap value?
   indirect: overwrite [v] with an indirection to [w]
---------------------------------------------------------*/
bool is_heap_val(value v);
void indirect(value v, value w);

/*---------------------------------------------------------
   Invalid fields ("inv")
---------------------------------------------------------*/
struct inv_block_t;   /* declared in memory.c */
extern struct inv_block_t inv_block;

#define Inv                     Val_hp(&inv_block)
#define Store_field_inv(v,i)    Store_field(v,i,Inv)
#define Init_field_inv(v,i)     Field(v,i) = Inv


/* roots */
struct caml__roots_block {
  struct caml__roots_block *next;
  long ntables;
  long nitems;
  value *tables [5];
};

extern struct caml__roots_block *local_roots;  /* defined in roots.c */

/* The following macros are used to declare C local variables and
   function parameters of type [value].

   The function body must start with one of the [CAMLparam] macros.
   If the function has no parameter of type [value], use [CAMLparam0].
   If the function has 1 to 5 [value] parameters, use the corresponding
   [CAMLparam] with the parameters as arguments.
   If the function has more than 5 [value] parameters, use [CAMLparam5]
   for the first 5 parameters, and one or more calls to the [CAMLxparam]
   macros for the others.
   If the function takes an array of [value]s as argument, use
   [CAMLparamN] to declare it (or [CAMLxparamN] if you already have a
   call to [CAMLparam] for some other arguments).

   If you need local variables of type [value], declare them with one
   or more calls to the [CAMLlocal] macros.
   Use [CAMLlocalN] to declare an array of [value]s.

   Your function may raise and exception or return a [value] with the
   [CAMLreturn1] macro.  Its argument is simply the [value] returned by
   your function.  Do NOT directly return a [value] with the [return]
   keyword.  If your function returns void, use [CAMLreturn0].

   All the identifiers beginning with "caml__" are reserved by Caml.
   Do not use them for anything (local or global variables, struct or
   union tags, macros, etc.)
*/

#define CAMLparam0() \
  struct caml__roots_block * UNUSED caml__frame = local_roots

#define CAMLparam1(x) \
  CAMLparam0 (); \
  CAMLxparam1 (x)

#define CAMLparam2(x, y) \
  CAMLparam0 (); \
  CAMLxparam2 (x, y)

#define CAMLparam3(x, y, z) \
  CAMLparam0 (); \
  CAMLxparam3 (x, y, z)

#define CAMLparam4(x, y, z, t) \
  CAMLparam0 (); \
  CAMLxparam4 (x, y, z, t)

#define CAMLparam5(x, y, z, t, u) \
  CAMLparam0 (); \
  CAMLxparam5 (x, y, z, t, u)

#define CAMLparamN(x, size) \
  CAMLparam0 (); \
  CAMLxparamN (x, (size))


#define CAMLxparam1(x) \
  struct caml__roots_block caml__roots_##x; \
  int UNUSED caml__dummy_##x = ( \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 1), \
    (caml__roots_##x.tables [0] = &x), \
    0)

#define CAMLxparam2(x, y) \
  struct caml__roots_block caml__roots_##x; \
  int UNUSED caml__dummy_##x = ( \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 2), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    0)

#define CAMLxparam3(x, y, z) \
  struct caml__roots_block caml__roots_##x; \
  int UNUSED caml__dummy_##x = ( \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 3), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    (caml__roots_##x.tables [2] = &z), \
    0)

#define CAMLxparam4(x, y, z, t) \
  struct caml__roots_block caml__roots_##x; \
  int UNUSED caml__dummy_##x = ( \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 4), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    (caml__roots_##x.tables [2] = &z), \
    (caml__roots_##x.tables [3] = &t), \
    0)

#define CAMLxparam5(x, y, z, t, u) \
  struct caml__roots_block caml__roots_##x; \
  int UNUSED caml__dummy_##x = ( \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = 1), \
    (caml__roots_##x.ntables = 5), \
    (caml__roots_##x.tables [0] = &x), \
    (caml__roots_##x.tables [1] = &y), \
    (caml__roots_##x.tables [2] = &z), \
    (caml__roots_##x.tables [3] = &t), \
    (caml__roots_##x.tables [4] = &u), \
    0)

#define CAMLxparamN(x, size) \
  struct caml__roots_block caml__roots_##x; \
  int UNUSED caml__dummy_##x = ( \
    (caml__roots_##x.next = local_roots), \
    (local_roots = &caml__roots_##x), \
    (caml__roots_##x.nitems = (size)), \
    (caml__roots_##x.ntables = 1), \
    (caml__roots_##x.tables[0] = &(x[0])), \
    0)

#define CAMLlocal1(x) \
  value x = 0; \
  CAMLxparam1 (x)

#define CAMLlocal2(x, y) \
  value x = 0, y = 0; \
  CAMLxparam2 (x, y)

#define CAMLlocal3(x, y, z) \
  value x = 0, y = 0, z = 0; \
  CAMLxparam3 (x, y, z)

#define CAMLlocal4(x, y, z, t) \
  value x = 0, y = 0, z = 0, t = 0; \
  CAMLxparam4 (x, y, z, t)

#define CAMLlocal5(x, y, z, t, u) \
  value x = 0, y = 0, z = 0, t = 0, u = 0; \
  CAMLxparam5 (x, y, z, t, u)

#define CAMLlocalN(x, size) \
  value x [(size)] = { 0, /* 0, 0, ... */ }; \
  CAMLxparamN (x, (size))


#define CAMLreturn0 do{ \
  local_roots = caml__frame; \
  return; \
}while (0)

#define CAMLreturn(result) do{ \
  local_roots = caml__frame; \
  return (result); \
}while(0)

/*
   NOTE: [Begin_roots] and [End_roots] are superseded by [CAMLparam]*,
   [CAMLxparam]*, [CAMLlocal]*, [CAMLreturn].

   [Begin_roots] and [End_roots] are used for C variables that are GC roots.
   It must contain all values in C local variables and function parameters
   at the time the minor GC is called.
   Usage:
   After initialising your local variables to legal Caml values, but before
   calling allocation functions, insert [Begin_roots_n(v1, ... vn)], where
   v1 ... vn are your variables of type [value] that you want to be updated
   across allocations.
   At the end, insert [End_roots()].

   Note that [Begin_roots] opens a new block, and [End_roots] closes it.
   Thus they must occur in matching pairs at the same brace nesting level.

   You can use [Val_unit] as a dummy initial value for your variables.
*/

#define Begin_root Begin_roots1

#define Begin_roots1(r0) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = &(r0);

#define Begin_roots2(r0, r1) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 2; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1);

#define Begin_roots3(r0, r1, r2) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 3; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2);

#define Begin_roots4(r0, r1, r2, r3) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 4; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3);

#define Begin_roots5(r0, r1, r2, r3, r4) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 5; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3); \
  caml__roots_block.tables[4] = &(r4);

#define Begin_roots_block(table, size) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = (size); \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = (table);

#define End_roots() local_roots = caml__roots_block.next; }


/* [register_global_root] registers a global C variable as a memory root
   for the duration of the program, or until [remove_global_root] is
   called. */

void register_global_root (value *);

/* [remove_global_root] removes a memory root registered on a global C
   variable with [register_global_root]. */

void remove_global_root (value *);


#endif /* _memory_ */
