/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _bytes_
#define _bytes_

#include "mlvalues.h"
#include "custom.h"

#define Bytes_val(v)      (((char**)(Data_custom_val(v)))[0])
#define Void_bytes_val(v) (((void**)(Data_custom_val(v)))[0])

value alloc_bytes( asize_t size );
value bytes_of_string( value s );
struct custom_operations bytes_ops;

#endif /* _bytes_ */