/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id: */

#ifndef _lvmmemory_h
#define _lvmmemory_h

#include "memory.h"

/*---------------------------------------------------------
   Invalid fields ("inv")
---------------------------------------------------------*/
struct inv_block_t;   /* declared in memory.c */
extern struct inv_block_t inv_block;

#define Inv                     Val_hp(&inv_block)
#define Store_field_inv(v,i)    Store_field(v,i,Inv)
#define Init_field_inv(v,i)     Field(v,i) = Inv


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

#define Alloc_con(v,sz,t) { unsigned long consize, contag; \
                            if (t >= Con_max_tag) { contag = Con_max_tag; consize = sz+1; } \
                                             else { contag = t; consize = sz; } \
                            Allocate(v,consize,contag); \
                            if (t >= Con_max_tag) { Field(v,sz) = Val_long(t); } \
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
# define UpdateAlloc(v,w,_newsz,newtag) { \
    wsize_t sz    = Wosize_val(v); \
    wsize_t newsz = _newsz; /* sharing */ \
    Assert( Is_block(v) && Wosize_val(v) > 0); \
    if (sz == newsz) { \
      w = v; \
      Tag_val(v) = newtag; \
    } else if (sz < newsz \
               /* || (newsz+1 == sz && !Is_young(v))   // avoid gc bug */ \
              ) { \
      Allocate(w,newsz,newtag); \
      Indirect(v,w); \
    } else { \
      w = v; \
      Assert(sz != newsz+1); \
      Truncate(v,sz,newsz,newtag); \
    } \
  }
#else
#define UpdateAlloc(v,w,newsz,newtag) { \
    Allocate(w,newsz,newtag); \
    Indirect(v,w); \
  }
#endif

#if defined(LVM_UPDATE_INPLACE) && defined(LVM_UPDATE_CON_INPLACE)
#define Update(v,w) { \
    wsize_t sz = Wosize_val(v); \
    wsize_t newsz; \
    if (Is_block(w) && (newsz = Wosize_val(w)) <= sz && newsz > 0 \
       /* && (newsz+1 != sz || Is_young(v))   // avoid gc bug */ \
       ) { \
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

#endif /* _lvmmemory_h */
