/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id: */

#ifndef _heapfast_h
#define _heapfast_h

#include "heap.h"

/* convenience macros */
#undef Store_field
#undef Init_field
#define Store_field(block, offset, val) Modify (&Field (block, offset), val)
#define Init_field(block, offset, val)  Initialize( &Field(block,offset), val )

/*---------------------------------------------------------
   Alloc_small, Modify, Initialize
---------------------------------------------------------*/
#define Alloc_small(result, wosize, tag) {        Assert (wosize >= 1); \
                                            Assert ((tag_t) tag < 256); \
  young_ptr -= Bhsize_wosize (wosize);                                      \
  if (young_ptr < young_limit){                                             \
    Setup_for_gc;                                                           \
    Garbage_collection_function ();                                         \
    Restore_after_gc;                                                       \
    young_ptr -= Bhsize_wosize (wosize);                                    \
  }                                                                         \
  Hd_hp (young_ptr) = Make_header ((wosize), (tag), Caml_black);            \
  (result) = Val_hp (young_ptr);                                            \
  DEBUG_clear (result, wosize);                                             \
}

/* You must use [Modify] to change a field of an existing shared block,
   unless you are sure the value being overwritten is not a shared block and
   the value being written is not a young block. */
/* [Modify] never calls the GC. */

#define Modify(fp, val) {                                                   \
  value _old_ = *(fp);                                                      \
  *(fp) = (val);                                                            \
  if (Is_in_heap (fp)){                                                     \
    if (gc_phase == Phase_mark) darken (_old_, NULL);                       \
    if (Is_block (val) && Is_young (val)                                    \
        && ! (Is_block (_old_) && Is_young (_old_))){                       \
      *ref_table_ptr++ = (fp);                                              \
      if (ref_table_ptr >= ref_table_limit){                                \
        Assert (ref_table_ptr == ref_table_limit);                      \
        realloc_ref_table ();                                               \
      }                                                                     \
    }                                                                       \
  }                                                                         \
}                                                                           \


/* You must use [initialize] to store the initial value in a field of
   a shared block, unless you are sure the value is not a young block.
   A block value [v] is a shared block if and only if [Is_in_heap (v)]
   is true.
*/
/* [initialize] never calls the GC, so you may call it while an object is
   unfinished (i.e. just after a call to [alloc_shr].) */
#define Initialize(fp,val) { \
  *(fp) = (val); \
  if (Is_in_heap (fp) && Is_block (val) && Is_young (val)){ \
    *ref_table_ptr++ = (fp); \
    if (ref_table_ptr >= ref_table_limit){ \
      realloc_ref_table (); \
    } \
  } \
}


/*---------------------------------------------------------
   General allocation
---------------------------------------------------------*/
#define Allocate(v,_sz,t) { wsize_t sz = _sz; /* sharing */ \
                            if (sz == 0) { (v) = Atom(t); } \
                            else if (sz < Max_young_wosize) { Alloc_small(v,sz,t); } \
                            else { Alloc_large(v,sz,t); } \
                          }

#define Alloc_large(v,sz,t)  { Setup_for_gc; \
                               (v) = alloc_shr(sz,t); \
                               Restore_after_gc; \
                             }

#define Alloc_con(v,consize,contag) { \
    wsize_t size;  \
    tag_t   tag;   \
    if (contag >= Con_max_tag) { tag = Con_max_tag; size = consize+1; } \
                          else { tag = contag; size = consize; } \
    Allocate(v,size,tag); \
    if (contag >= Con_max_tag) { Field(v,consize) = Val_long(contag); } \
  }
/*
                            if (t >= Con_max_tag) {\
                               Allocate(v,sz+1,Con_max_tag); \
                               Field(v,sz) = Val_long(t); \
                            } else { \
                               Allocate(v,sz,t); \
                            } \
                          }
*/

/*---------------------------------------------------------
   Indirections
---------------------------------------------------------*/

/* Indirect(v,w): overwrite [v] with an indirection to [w] */
#define Indirect(v,w) { \
    wsize_t vsz; \
    Assert( Is_block(v) && Wosize_val(v) > 0); \
    vsz = Wosize_val(v); \
    Indirect_size(v,vsz,w); \
  }

#define Indirect_size(v,sz,w) { \
    /* wsize_t i; */ \
    Assert( Is_block(v) && Wosize_val(v) == sz && sz > 0 ); \
    Store_field(v,0,w); \
    Downsize(v,sz,1,Ind_tag);  \
    /* Tag_val(v) = Ind_tag; \
    for( i = 1; i < sz; i++) Store_field_inv(v,i); */ \
  }


/*---------------------------------------------------------
   Updates
---------------------------------------------------------*/

/* Truncate(v,sz,newsz,newtag): truncate block [v] with size [sz] to [newsz] and set its
   tag to [newtag]. The [newsz] should always be smaller than [sz] and greater than zero. */
#define Truncate(v,sz,newsz,newtag) { \
    wsize_t i; \
    Assert(Is_block(v) &&  Wosize_val(v) == sz && newsz > 0 && newsz < sz); \
    /* Assert(sz != newsz+1 || Is_young(v)); // triggers gc bug */ \
    /* erase trailing values explicitly for the GC */ \
    for( i = newsz; i < sz; i++) { Store_field_inv(v,i); } \
    /* create a trailing fragment */ \
    Field(v,newsz) = Make_header (Wosize_whsize(sz-newsz), 0, Caml_white); \
    /* set the new size & tag */ \
    Hd_val(v) = Make_header( newsz, newtag, Color_val(v) ); \
  }

#define Downsize(v,sz,newsz,newtag) { \
    Assert(Is_block(v) && Wosize_val(v) == sz && newsz > 0 && newsz <= sz); \
    if (sz == newsz) { Tag_val(v) = newtag; } \
                else { Truncate(v,sz,newsz,newtag); } \
    }



/* Update(v,w,sz,tag) => update [v] with a block [w] of size [sz] and tag [tag] */
#ifdef LVM_UPDATE_INPLACE
# define Update_alloc(v,w,_newsz,newtag) { \
    wsize_t sz    = Wosize_val(v); \
    wsize_t newsz = _newsz; /* sharing */ \
    Assert( Is_block(v) && Wosize_val(v) > 0); \
    if (sz == newsz) { \
      w = v; \
      Tag_val(v) = newtag; \
    } else if (sz > newsz) { \
      w = v; \
      Truncate(v,sz,newsz,newtag); \
    } else { \
      Allocate(w,newsz,newtag); \
      Indirect(v,w); \
    } \
  }

#else
#define Update_alloc(v,w,newsz,newtag) { \
    Allocate(w,newsz,newtag); \
    Indirect(v,w); \
  }
#endif

#ifdef LVM_UPDATE_INPLACE
# define Update_alloc_con(v,w,consize,contag) { \
    wsize_t size; \
    tag_t   tag;  \
    if (contag >= Con_max_tag) { tag = Con_max_tag; size = consize + 1; } \
                          else { tag = contag; size = consize; } \
    Update_alloc(v,w,size,tag); \
    if (contag >= Con_max_tag) { Store_field(w,consize,Val_long(contag)); } \
  }
#else
# define Update_alloc_con(v,w,size,tag) { \
    Alloc_con(w,size,tag); \
    Indirect(v,w); \
  }
#endif

#if defined(LVM_UPDATE_INPLACE) && defined(LVM_UPDATE_CON_INPLACE)
#define Update(v,w) { \
    wsize_t sz = Wosize_val(v); \
    wsize_t newsz; \
    if (Is_block(w) && (newsz = Wosize_val(w)) <= sz && newsz > 0) { \
      wsize_t i; \
      Downsize(v,sz,newsz,Tag_val(w)); \
      for( i = 0; i < newsz; i++ ) { Store_field(v,i,Field(w,i)); } \
    } else { \
      Indirect_size(v,sz,w); \
    } \
  }
#else
#define Update(v,w) { \
    Indirect(v,w); \
  }
#endif

#endif /* _heapfast_h */
