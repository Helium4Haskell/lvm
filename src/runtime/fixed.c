/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

/*---------------------------------------------------------------------
  Fixed memory blocks: tracked by gc but never moved.
----------------------------------------------------------------------*/
#include "mlvalues.h"
#include "custom.h"
#include "memory.h"
#include "fail.h"
#include "fixed.h"

/* the global list of fixed blocks -- used as roots */
struct fixed_block* fixed_blocks = NULL;

/*----------------------------------------------------------------------
  fixed block custom operations
----------------------------------------------------------------------*/
static void fixed_block_finalize( value v )
{
  struct fixed_block* fixedb, *prev, *next;
  gc_message( 8,"finalise fixed_block\n", 0 );
  fixedb = Fixed_block_val(v);
  if (fixedb == NULL) return;

  /* unlink this block from the fixed_blocks list */
  for( prev = NULL, next = fixed_blocks;
       next != NULL && next != fixedb;
       prev = next, next = next->next ) {}
  if (next == fixedb && next != NULL) {
    if (prev == NULL) fixed_blocks = fixedb->next;
                 else prev->next = fixedb->next;
  }

  /* and free the block */
  stat_free(fixedb);
  return;
}

static int fixed_block_compare(value v1, value v2)
{
  raise_internal( "comparing abstract fixed_block" );
  return 0;
}

static long fixed_block_hash(value v)
{
  return (long)(Fixed_block_val(v));
}

static void fixed_block_serialize(value v, unsigned long * wsize_32,
                            unsigned long * wsize_64)
{
  raise_internal( "serializing fixed_block" );
}

static unsigned long fixed_block_deserialize(void * dst)
{
  raise_internal( "deserializing fixed_block" );
  return 0;
}


struct custom_operations fixed_block_ops = {
  "_fixed_block",
  fixed_block_finalize,
  fixed_block_compare,
  fixed_block_hash,
  fixed_block_serialize,
  fixed_block_deserialize
};

/*----------------------------------------------------------------------
  fixed block allocation
----------------------------------------------------------------------*/
value alloc_fixed( size_t size )
{
  CAMLparam0();
  CAMLlocal1(v);
  v = alloc_custom( &fixed_block_ops, sizeof(struct fixed_block*), 0, 1 );
  if (size == 0) {
    Fixed_block_val(v) = NULL;
  }
  else {
    size_t i;
    struct fixed_block* fixedb = stat_alloc( sizeof(struct fixed_block) + (size-1)*sizeof(value) );
    fixedb->size = size;
    for( i = 0; i < size; i++) { fixedb->data[i] = 0; }
    fixedb->next = fixed_blocks;
    fixed_blocks = fixedb;
    Fixed_block_val(v) = fixedb;
  }

  CAMLreturn(v);
}
