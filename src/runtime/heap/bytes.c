/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include "mlvalues.h"
#include "custom.h"
#include "memory.h"
#include "bytes.h"
#include "misc.h"
#include "fail.h"

static void bytes_finalize( value v )
{
  void* mem;
  gc_message( 8,"finalise bytes value\n", 0 );
  mem = Void_bytes_val(v);
  stat_free(mem);
  Void_bytes_val(v) = 0;
}

static int bytes_compare(value v1, value v2)
{
  raise_user( "can not compare abstract bytes" );
  return 0;
}

static long bytes_hash(value v)
{
  return (long)(Bytes_val(v));
}

static void bytes_serialize(value v, unsigned long * wsize_32,
                            unsigned long * wsize_64)
{
  raise_user( "can not serialize bytes" );
}

static unsigned long bytes_deserialize(void * dst)
{
  raise_user( "can not deserialize bytes" );
  return 0;
}


struct custom_operations bytes_ops = {
  "_bytes",
  bytes_finalize,
  bytes_compare,
  bytes_hash,
  bytes_serialize,
  bytes_deserialize
};


value alloc_bytes( asize_t size )
{
  CAMLparam0();
  CAMLlocal1(v);
  void* mem;
  v = alloc_custom( &bytes_ops, sizeof(void*), 0, 1 );
  mem = stat_alloc( size );
  Store_field( v, 1, (value)mem );
  CAMLreturn(v);
}