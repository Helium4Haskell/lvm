/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _print_
#define _print_

#include "mlvalues.h"
#include "module.h"

const char* String_of_size( wsize_t size, const char* modifier );
#define Bstring_of_bsize(sz) String_of_size(sz,"")
#define Wstring_of_wsize(sz) String_of_size(sz,"w")
#define Bstring_of_wsize(sz) Bstring_of_bsize(Bsize_wsize(sz))
#define Wstring_of_bsize(sz) Wstring_of_wsize(Wsize_bsize(sz))

void print( const char* msg, ... );

void print_value( value module, value v );
void print_instr( value module, value* sp, opcode_t* code );
void print_stack( value module, const value* sp, const value* fp );
void print_stack_trace( value module, const value* fp, int maxdepth );
#endif /* _print_ */