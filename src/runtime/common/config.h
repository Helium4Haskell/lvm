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

#ifndef _config_
#define _config_

#include <m.h>
#include <s.h>
#include <stddef.h>
#include <stdlib.h>

#define in  const
#define out
#define inout

/* typedef short int16; */            /* FIXME -- not true on the Cray T3E */
/* typedef unsigned short uint16; */  /* FIXME -- not true on the Cray T3E */

#if SIZEOF_INT == 4
typedef int int32;
typedef unsigned int uint32;
#elif SIZEOF_LONG == 4
typedef long int32;
typedef unsigned long uint32;
#elif SIZEOF_SHORT == 4
typedef short int32;
typedef unsigned short uint32;
#endif

#if defined(ARCH_INT64) && defined(ARCH_UINT64)
typedef ARCH_INT64  int64;
typedef ARCH_UINT64 uint64;
#else
/* Int64.t will not be supported, and operations over it are not defined,
   but we must define the types int64 and uint64 as 64-bit placeholders. */
typedef struct { uint32 a, b; } uint64;
typedef uint64 int64;
#endif


/* Code fixups */
#if defined(ARCH64) && !defined(ARCH_CODE32)
 #define FIXUP_OFFSET
#endif


#if defined(FIXUP_OFFSET)
 extern char* fixup_base;
 #define Ptr_fixup(f)      (fixup_base + (f))
#else
 #define Ptr_fixup(f)      ((char*)(f))
#endif

#define Fixup_ptr(p)      ((char*)(p) - fixup_base)
#define Valptr_fixup(f)   ((value*)(Ptr_fixup(f)))
#define Val_fixup(f)      ((value)(Ptr_fixup(f)))
#define Code_fixup(f)     ((opcode_t*)(Ptr_fixup(f)))

/* Booleans */

#if !defined(HAS_BOOL)
typedef int bool;
#define true  1
#define false 0
#endif


/* Library dependencies */

#ifdef HAS_MEMMOVE
#undef bcopy
#define bcopy(src,dst,len) memmove((dst), (src), (len))
#else
#ifdef HAS_BCOPY
/* Nothing to do */
#else
#undef bcopy
#define bcopy(src,dst,len) memmov((dst), (src), (len))
#define USING_MEMMOV
#endif
#endif

/* We use threaded code interpretation if the compiler provides labels
   as first-class values (GCC 2.x).
*/

#if defined(HAS_LABEL_VALUES) && !defined(DEBUG)
  #define THREADED_CODE
  #if defined(ARCH64) && !defined(ARCH_CODE32)
    #define THREADED_OFFSET
  #endif
#endif

#ifdef __GNUC__
#define UNUSED __attribute__((unused))
#else
#define UNUSED
#endif

/* units: use 1000 instead of 1024 to avoid possibly bad effects on direct-mapped cache */
#define Kilo  (1000L)
#define Mega  (Kilo*Kilo)
#define Giga  (Kilo*Mega)


/* Maximal string buffer size on stack */
#define MAXSTR  512
#define MAXVAR  MAXSTR
#define MAXNAME 128

/* Maximal path length */
#if defined(_MAX_PATH)
# define MAXPATH _MAX_PATH
#elif defined(MAX_PATH)
# define MAXPATH MAX_PATH
#elif defined(MAXPATHLEN)
# define MAXPATH MAXPATHLEN
#elif defined(PATH_MAX)
# define MAXPATH PATH_MAX
#else
# define MAXPATH 512
#endif

/* File and path seperators */
#if defined(OS_WINDOWS)
# define FILESEP '\\'
# define PATHSEP ';'
#elif defined(OS_CYGWIN)
# define FILESEP '/'
# define PATHSEP ';'
#else
# define FILESEP '/'
# define PATHSEP ':'
#endif

/* Do not change this definition. */
#define Page_bsize (1 << Page_log)

/* Memory model parameters */

/* The size of a page for memory management (in bytes) is [1 << Page_log].
   It must be a multiple of [sizeof (long)]. */
#define Page_log 12             /* A page is 4 kilobytes. */

/* Maximum size of a block allocated in the young generation (words). */
/* Must be > 4 */
#define Max_young_wosize 256

/* Minimum size of the minor zone (words).
   This must be at least [Max_young_wosize + 1]. */
#define Minor_heap_min_wsize (2*Max_young_wosize)

/* Maximum size of the minor zone (words).
   Must be greater than or equal to [Minor_heap_min].
*/
#define Minor_heap_max_wsize (1 << 28)

/* Minimum size increment when growing the heap (words).
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_min_wsize (Wsize_bsize(Page_bsize))

/* Maximum size of a contiguous piece of the heap (words).
   Must be greater than or equal to [Heap_chunk_min].
   Must be greater than or equal to [Bhsize_wosize (Max_wosize)]. */
#define Heap_chunk_max_wsize (Bhsize_wosize (Max_wosize))


/*----------------------------------------------------------------------
  LVM evaluator configuration options
----------------------------------------------------------------------*/
#define LVM_UPDATE_INPLACE      /* try to update in place     */
#define LVM_UPDATE_CON_INPLACE  /* even try to copy constructors to update in place (instead of indirection) */
#define LVM_CHECK_BOUNDS        /* check bounds on operations */


#endif /* _config_ */