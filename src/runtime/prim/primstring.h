/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef primstring_h
#define primstring_h

value prim_string_of_chars( long len, value chars );
value prim_chars_of_string( value str );
long  prim_string_length( value str );

#endif
