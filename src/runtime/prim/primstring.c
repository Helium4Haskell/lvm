/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include "mlvalues.h"
#include "memory.h"
#include "alloc.h"
#include "fail.h"
#include "primstring.h"

value prim_string_of_chars( long len, value chars )
{
  CAMLparam1(chars);
  CAMLlocal1(str);
  char* s;
  long i;

  str = alloc_string(len);
  s   = String_val(str);

  for( i = 0; i < len; i++ ) {
    if (Is_atom(chars)) break;
    if (!is_heap_val(chars) || Tag_val(chars) != 1) break;
    if (!Is_long(Field(chars,0))) break;
    s[i] = (char)Long_val(Field(chars,0));
    chars = Field(chars,1);
  }
  if (i < len) s[i] = 0;
  CAMLreturn(str);
}

value prim_chars_of_string( value str )
{
  CAMLparam1(str);
  CAMLlocal2(chars,cons);
  char* s;
  long  i;
  long len;

  chars = Atom(0);
  if (!is_heap_val(str) || Tag_val(str) != String_tag)
    raise_invalid_argument( "prim_chars_of_string" );

  len = string_length(str);
  s   = String_val(str);

  for( i = len-1; i >= 0; i-- ) {
    cons = alloc_small(2,1);
    Field(cons,0) = Val_long(s[i]);
    Field(cons,1) = chars;
    chars = cons;
  }

  CAMLreturn(chars);
}

long prim_string_length( value str )
{
  if (!is_heap_val(str) || Tag_val(str) != String_tag)
    raise_invalid_argument( "prim_string_length" );
  return string_length(str);
}