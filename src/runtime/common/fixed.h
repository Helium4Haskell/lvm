/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id: */

#ifndef _fixed_h
#define _fixed_h

/*----------------------------------------------------------------------
  Fixed blocks.
  These blocks are traced by the garbage collector but can not be moved.
  They are implemented with custom blocks.
----------------------------------------------------------------------*/
struct fixed_block {
  struct fixed_block* next;
  size_t size;
  value  data[1];
};

value alloc_fixed( size_t size );

#define Fixed_block_val(v)  (((struct fixed_block**)Data_custom_val(v))[0])
#define Field_fixed(v,i)    (Fixed_block_val(v)->data[i])
#define Wosize_fixed(v)     (Fixed_block_val(v)->size)

#endif /* _fixed_h */
