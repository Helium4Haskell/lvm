/**----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. This file is distributed under the terms
  of the GNU Library General Public License. This file is based on the
  original Objective Caml source copyrighted by INRIA Rocquencourt.
----------------------------------------------------------------------**/

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

/* $Id$ */

#ifndef _mlvalues_
#define _mlvalues_

#include "config.h"
#include "misc.h"

/* Definitions

  word: Four bytes on 32 and 16 bit architectures,
        eight bytes on 64 bit architectures.
  long: A C long integer.
  val: The ML representation of something.  A long or a block or a pointer
       outside the heap.  If it is a block, it is the (encoded) address
       of an object.  If it is a long, it is encoded as well.
  block: Something allocated.  It always has a header and some
          fields or some number of bytes (a multiple of the word size).
  field: A word-sized val which is part of a block.
  bp: Pointer to the first byte of a block.  (a char *)
  op: Pointer to the first field of a block.  (a value *)
  hp: Pointer to the header of a block.  (a char *)
  int32: Four bytes on all architectures.
  int64: Eight bytes on all architectures.

  Remark: A block size is always a multiple of the word size, and at least
          one word plus the header.

  bosize: Size (in bytes) of the "bytes" part.
  wosize: Size (in words) of the "fields" part.
  bhsize: Size (in bytes) of the block with its header.
  whsize: Size (in words) of the block with its header.

  hd: A header.
  tag: The value of the tag field of the header.
  color: The value of the color field of the header.
         This is for use only by the GC.
*/

typedef unsigned long word;
typedef unsigned long nat;

typedef long   value;
typedef long   con_tag_t;
typedef double float_t;

typedef word wsize_t;
typedef word header_t;
typedef unsigned int tag_t;             /* Actually, an unsigned char */
typedef word color_t;
typedef word mark_t;

/* for compatibility with original o'caml code */
typedef wsize_t     mlsize_t;
typedef wsize_t     asize_t;

/* lvm binary files use signed 32 bit int's */
typedef int32       opcode_t;
typedef opcode_t    word_t;
typedef opcode_t *  code_t;


/* Longs vs blocks. */
#define Is_long(x)   (((x) & 1) != 0)
#define Is_block(x)  (((x) & 1) == 0)

/* Conversion macro names are always of the form  "to_from". */
/* Example: Val_long as in "Val from long" or "Val of long". */
#define Val_long(x)     (((long)(x) << 1) + 1)
#define Long_val(x)     ((x) >> 1)
#define Max_long        ((1L << (8 * (long)sizeof(value) - 2)) - 1)
#define Min_long        (-(1L << (8 * (long)sizeof(value) - 2)))
#define Val_int         Val_long
#define Int_val(x)      ((int) Long_val(x))

#define Min_word_t      (1L << (8*sizeof(word_t) - 1))
#define Max_word_t      (~0 ^ Min_word_t)

/* Foreign (C) pointers */
#define Val_ptr(p)  ((value)(p))
#define Ptr_val(v)  ((void*)(v))


#define Code_val(v)   ((word_t*)(Ptr_val(v)))
#define Val_code(c)   Val_ptr(c)

/* Structure of the header:

For 16-bit and 32-bit architectures:
     +--------+-------+-----+
     | wosize | color | tag |
     +--------+-------+-----+
bits  31    10 9     8 7   0

For 64-bit architectures:

     +--------+-------+-----+
     | wosize | color | tag |
     +--------+-------+-----+
bits  63    10 9     8 7   0

*/

#define Tag_hd(hd) ((tag_t) ((hd) & 0xFF))
#define Wosize_hd(hd) ((wsize_t) ((hd) >> 10))

#define Hd_val(val) (((header_t *) (val)) [-1])        /* Also an l-value. */
#define Hd_op(op) (Hd_val (op))                        /* Also an l-value. */
#define Hd_bp(bp) (Hd_val (bp))                        /* Also an l-value. */
#define Hd_hp(hp) (* ((header_t *) (hp)))              /* Also an l-value. */
#define Hp_val(val) ((char *) (((header_t *) (val)) - 1))
#define Hp_op(op) (Hp_val (op))
#define Hp_bp(bp) (Hp_val (bp))
#define Val_op(op) ((value) (op))
#define Val_hp(hp) ((value) (((header_t *) (hp)) + 1))
#define Op_hp(hp) ((value *) Val_hp (hp))
#define Bp_hp(hp) ((char *) Val_hp (hp))

#define Num_tags (1 << 8)
#ifdef ARCH_SIXTYFOUR
#define Max_wosize ((1L << 54) - 1)
#else
#define Max_wosize ((1 << 22) - 1)
#endif

#define Wosize_val(val) (Wosize_hd (Hd_val (val)))
#define Wosize_op(op) (Wosize_val (op))
#define Wosize_bp(bp) (Wosize_val (bp))
#define Wosize_hp(hp) (Wosize_hd (Hd_hp (hp)))
#define Whsize_wosize(sz) ((sz) + 1)
#define Wosize_whsize(sz) ((sz) - 1)
#define Wosize_bhsize(sz) ((sz) / sizeof (value) - 1)
#define Bsize_wsize(sz) ((sz) * sizeof (value))
#define Wsize_bsize(sz) ((sz) / sizeof (value))
#define Bhsize_wosize(sz) (Bsize_wsize (Whsize_wosize (sz)))
#define Bhsize_bosize(sz) ((sz) + sizeof (header_t))
#define Bosize_val(val) (Bsize_wsize (Wosize_val (val)))
#define Bosize_op(op) (Bosize_val (Val_op (op)))
#define Bosize_bp(bp) (Bosize_val (Val_bp (bp)))
#define Bosize_hd(hd) (Bsize_wsize (Wosize_hd (hd)))
#define Whsize_hp(hp) (Whsize_wosize (Wosize_hp (hp)))
#define Whsize_val(val) (Whsize_hp (Hp_val (val)))
#define Whsize_bp(bp) (Whsize_val (Val_bp (bp)))
#define Whsize_hd(hd) (Whsize_wosize (Wosize_hd (hd)))
#define Bhsize_hp(hp) (Bsize_wsize (Whsize_hp (hp)))
#define Bhsize_hd(hd) (Bsize_wsize (Whsize_hd (hd)))

#define Wosize_bsize(sz) Wsize_bsize(sz)
#define Bsize_wosize(sz) Bsize_wsize(sz)

#ifdef ARCH_BIG_ENDIAN
#define Tag_val(val) (((unsigned char *) (val)) [-1])
                                                 /* Also an l-value. */
#define Tag_hp(hp) (((unsigned char *) (hp)) [sizeof(value)-1])
                                                 /* Also an l-value. */
#else
/* LVM: added [signed] conversion */
#define Tag_val(val) (((unsigned char *) (val)) [-((signed)sizeof(value))])
                                                 /* Also an l-value. */
#define Tag_hp(hp) (((unsigned char *) (hp)) [0])
                                                 /* Also an l-value. */
#endif


/* The lowest tag for blocks containing no value. */
#define No_scan_tag 251


/* 1- If tag < No_scan_tag : a tuple of fields.  */

/* Pointer to the first field. */
#define Op_val(x) ((value *) (x))
/* Fields are numbered from 0. */
#define Field(x, i) (((value *)(x)) [i])           /* Also an l-value. */


/* NOTE: [Forward_tag] and [Infix_tag] must be just under
   [No_scan_tag], with [Infix_tag] the lower one.
   See [oldify_one] in minor_gc.c for more details.
*/
#define Forward_tag 250
#define Forward_val(v) Field(v, 0)

/* If tag == Infix_tag : an infix header inside a closure */
/* Infix_tag must be odd so that the infix header is scanned as an integer */
/* Infix_tag must be 1 modulo 4 and infix headers can only occur in blocks
   with tag Closure_tag (see compact.c). */

#define Infix_tag 249
#define Infix_offset_hd(hd) (Bosize_hd(hd))
#define Infix_offset_val(v) Infix_offset_hd(Hd_val(v))

/* Another special case: objects */
#define Object_tag 248
#define Class_val(val) Field(val, 0)
#define Oid_val(val) Long_val(Field(val, 1))

/* Special case of tuples of fields: closures */
#define Closure_tag 247


/* Special LVM objects */

#define Code_tag    Abstract_tag      /* only exist statically */

#define Ap_tag      246
#define Nap_tag     245
#define Caf_tag     244
#define Inv_tag     243   /* invalid or blackhole */
#define Raise_tag   242
#define Suspend_tag 241

#define Ind_tag     Forward_tag   /* indirection */

#define Con_max_tag 240

#define Con_tag_val(t,v)  { (t) = Tag_val(v); if ((t) == Con_max_tag) { (t) = Long_val(Field(v,Wosize_val(v)-1)); }}
#define Val_con_tag(t)    Val_long(t)

#define Con_tag_size(tag,size,contag,consize) { \
    if (contag >= Con_max_tag) { tag = Con_max_tag; size = consize+1; } \
                          else { tag = contag; size = consize; } \
  }

/* fsize: fields size, doesn't include possible extended tag value at the end */
#define Fsize_val(v)        (Tag_val(v) == Con_max_tag ? Wosize_val(v)-1 : Wosize_val(v))
#define Wosize_fsize(t,n)   (t >= Con_max_tag ? n+1 : n)


/* Another special case: variants */
extern value hash_variant(char * tag);

/* 2- If tag >= No_scan_tag : a sequence of bytes. */

/* Pointer to the first byte */
#define Bp_val(v) ((char *) (v))
#define Val_bp(p) ((value) (p))
/* Bytes are numbered from 0. */
#define Byte(x, i) (((char *) (x)) [i])            /* Also an l-value. */
#define Byte_u(x, i) (((unsigned char *) (x)) [i]) /* Also an l-value. */

/* Abstract things.  Their contents is not traced by the GC; therefore they
   must not contain any [value].
*/
#define Abstract_tag 251

/* Strings. */
#define String_tag 252
#define String_val(x) ((char *) Bp_val(x))
mlsize_t string_length (value);

/* Floating-point numbers. */
#define Double_tag 253
#define Double_wosize ((sizeof(double) / sizeof(value)))
#ifndef ARCH_ALIGN_DOUBLE
#define Double_val(v) (* (double *)(v))
#define Store_double_val(v,d) (* (double *)(v) = (d))
#else
double Double_val (value);
void Store_double_val (value,double);
#endif

/* Arrays of floating-point numbers. */
#define Double_array_tag 254
#define Double_field(v,i) Double_val((value)((double *)(v) + (i)))
#define Store_double_field(v,i,d) \
  Store_double_val((value)((double *)(v) + (i)),d)

/* Custom blocks.  They contain a pointer to a "method suite"
   of functions (for finalization, comparison, hashing, etc)
   followed by raw data.  The contents of custom blocks is not traced by
   the GC; therefore, they must not contain any [value].
   See [custom.h] for operations on method suites. */
#define Custom_tag 255
#define Data_custom_val(v) ((void *) &Field(v, 1))
struct custom_operations;       /* defined in [custom.h] */

/* Int32.t, Int64.t and Nativeint.t are represented as custom blocks. */

#define Int32_val(v) (*((int32 *) Data_custom_val(v)))
#define Nativeint_val(v) (*((long *) Data_custom_val(v)))
#ifndef ARCH_ALIGN_INT64
#define Int64_val(v) (*((int64 *) Data_custom_val(v)))
#else
extern int64 Int64_val(value v);
#endif

/* 3- Atoms are 0-tuples.  They are statically allocated once and for all. */

extern header_t atom_table[];
#define Atom(tag) (Val_hp (&(atom_table [tag])))

/* Is_atom tests whether a well-formed block is statically allocated
   outside the heap. For the bytecode system, only zero-sized block (Atoms)
   fall in this class. For the native-code generator, data
   emitted by the code generator (as described in the table
   caml_data_segments) are also atoms. */

#ifndef NATIVE_CODE
#define Is_atom(v) ((v) >= Atom(0) && (v) <= Atom(255))
#else
extern char * static_data_start, * static_data_end;
#define Is_atom(v) \
  ((((char *)(v) >= static_data_start && (char *)(v) < static_data_end) || \
   ((v) >= Atom(0) && (v) <= Atom(255))))
#endif

/* Booleans are atoms 0 or 1 */

#define Val_bool(x) ((x) == 0 ? Val_false : Val_true)
#define Bool_val(x) ((x) == Val_true)
#define Val_false (Atom(0))
#define Val_true  (Atom(1))
#define Val_not(x) ((x) == Val_false ? Val_true : Val_false)

/* The unit value is 0 (tagged) */

#define Val_unit (Val_long(0))

/* The table of global identifiers */

extern value global_data;



#endif /* _mlvalues_ */