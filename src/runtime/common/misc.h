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

/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_


#include "config.h"

/* Standard header files */
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

/* Basic types and constants */

#ifndef NULL
#define NULL 0
#endif

typedef char * addr;

#ifdef __GNUC__
/* Works only in GCC 2.5 and later */
#define Noreturn __attribute ((noreturn))
#else
#define Noreturn
#endif

/* Assertions */
#ifdef DEBUG
#define Assert(x) if (!(x)) _failed_assert ( #x , __FILE__, __LINE__)
void _failed_assert (char *, char *, int) Noreturn;
#else
#define Assert(x)
#endif


/* Variable arguments */
#ifdef HAS_STDARG_H
# include <stdarg.h>
# define FUN_VAR_ARGS1(name,tp,fst,args)  name ( tp fst, ... ) { va_list args; va_start(args,fst);
# define FUN_VAR_ARGS2(name,tpfst,fst,tpsnd,snd,args)  name ( tpfst fst, tpsnd snd, ... ) { va_list args; va_start(args,snd);
# define FUN_VAR_ARGS3(name,tpfst,fst,tpsnd,snd,tpthd,thd,args)  name ( tpfst fst, tpsnd snd, tpthd thd, ... ) { va_list args; va_start(args,thd);
#else
# include <varargs.h>
# define FUN_VAR_ARGS1(name,tp,fst,args)  name ( fst, va_alist ) tp fst; va_dcl { va_list args; va_start(args);
# define FUN_VAR_ARGS2(name,tpfst,fst,tpsnd,snd,args)  name ( fst, snd, va_alist ) tpfst fst; tpsnd snd; va_dcl { va_list args; va_start(args);
# define FUN_VAR_ARGS3(name,tpfst,fst,tpsnd,snd,tpthd, thd, args)  name ( fst, snd, thd, va_alist ) tpfst fst; tpsnd snd; tpthd thd; va_dcl { va_list args; va_start(args);
#endif
#define END_ARGS(args)          va_end(args); }


/* checked sprintf */
#if !defined(HAS_SNPRINTF)
# if defined HAS__SNPRINTF
#  define snprintf  _snprintf
# else
   void snprintf( char* buf, long size, const char* fmt, ... );
# endif
#endif

#if !defined(HAS_VSNPRINTF)
# if defined HAS__VSNPRINTF
#  define vsnprintf _vsnprintf
# else
   void vsnprintf( char* buf, long size, const char* fmt, va_list args );
# endif
#endif

void garbage_collect(void);

void sys_exit( int code ) Noreturn;
void shut_down( int code ) Noreturn;

/* messages */
void vmessage( const char* fmt, va_list args );
void error_vmessage( const char* fmt, va_list args );

void message( const char* fmt, ... );
void error_message( const char* fmt, ... );
void fatal_error (const char* fmt, ... ) Noreturn;


#ifdef DEBUG
void _debug_gc(void);
#define debug_gc() _debug_gc()
#else
#define debug_gc()
#endif

/* GC flags and messages */
extern int verb_gc;
void gc_message (int level, const char* msg, unsigned long);

/* ansi routines */
#if !defined(HAS_STRICMP)
int stricmp( const char* s, const char* t );
#endif

/* string routines */
void str_copy( char* dest, const char* src, long size );
void str_cat ( char* dest, const char* src, long size );
long str_len ( const char* src );

/* Memory routines */
void memmov (char *, char *, unsigned long);
char *aligned_malloc (size_t, int, void **);

#ifdef DEBUG
#ifdef ARCH_64
#define Debug_tag(x) (0xD700D7D7D700D6D7ul \
                      | ((unsigned long) (x) << 16) \
                      | ((unsigned long) (x) << 48))
#else
#define Debug_tag(x) (0xD700D6D7ul | ((unsigned long) (x) << 16))
#endif

/*
  00 -> free words in minor heap
  01 -> fields of free list blocks in major heap
  03 -> heap chunks deallocated by heap shrinking
  04 -> fields deallocated by obj_truncate
  10 -> uninitialised fields of minor objects
  11 -> uninitialised fields of major objects
  12 -> uninitialised words of stat_alloc blocks
  15 -> uninitialised words of aligned_malloc blocks
  85 -> filler bytes of aligned_malloc
*/
#define Debug_free_minor     Debug_tag (0x00)
#define Debug_free_major     Debug_tag (0x01)
#define Debug_free_shrink    Debug_tag (0x03)
#define Debug_free_truncate  Debug_tag (0x04)
#define Debug_uninit_minor   Debug_tag (0x10)
#define Debug_uninit_major   Debug_tag (0x11)
#define Debug_uninit_stat    Debug_tag (0x12)
#define Debug_uninit_align   Debug_tag (0x15)
#define Debug_filler_align   Debug_tag (0x85)
#endif

#endif /* _misc_ */