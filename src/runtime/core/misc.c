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

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "mlvalues.h"
#include "heap/heap.h"
#include "dynamic.h"
#include "signals.h"
#include "options.h"

/*---------------------------------------------------------
-- messages
---------------------------------------------------------*/
void vmessage( const char* fmt, va_list args )
{
  vprintf( fmt, args );
}

void error_vmessage( const char* fmt, va_list args )
{
  vfprintf( stderr, fmt, args );
}


void FUN_VAR_ARGS1(message, const char*, fmt,args)
{
  vmessage( fmt, args );
}
END_ARGS(args)

void FUN_VAR_ARGS1(error_message, const char*, fmt,args)
{
  error_vmessage( fmt, args );
  va_end(args);
}
END_ARGS(args)


/*---------------------------------------------------------
-- fatal
---------------------------------------------------------*/
void Noreturn sys_exit( int code )
{
  exit(code);
}

void Noreturn shut_down(int code)
{
  done_options(false);
  sys_exit(code);
}

void Noreturn FUN_VAR_ARGS1(fatal_error, const char *, fmt, args)
{
  error_vmessage( fmt, args );
  shut_down(2);
}
END_ARGS(args)


#ifdef DEBUG
void _failed_assert (char * expr, char * file, int line)
{
  error_message("file %s; line %d ### Assertion failed: %s\n", file, line, expr);
  sys_exit(1);
}
#endif


/*---------------------------------------------------------
-- GC
---------------------------------------------------------*/
int verb_gc;

void gc_message (int level, const char *msg, unsigned long arg)
{
  if (level < 0 || (verb_gc & level) != 0){
    error_message( msg, arg );
  }
}


/*---------------------------------------------------------
-- Debug
---------------------------------------------------------*/
void _debug_gc(void)
{
  gc_full_major();
}


/*---------------------------------------------------------
-- stricmp
---------------------------------------------------------*/
#if !defined(HAS_STRICMP)
int stricmp( const char* s, const char* t )
{
  const char* p;
  const char* q;

  /* check for NULL */
  if (s == NULL && t == NULL) return 0;
  if (s == NULL) return -1;
  if (t == NULL) return 1;

  /* walk through the strings */
  for( p = s, q = t; (*p != 0) && (*q != 0); p++, q++) {
    int i,j;
    i = tolower(*p);
    j = tolower(*q);
    if (i < j) return -1;
    if (i > j) return 1;
  }

  /* check for the end of each string */
  if (*p != 0) return 1;
  if (*q != 0) return -1;
  return 0;
}
#endif

/*---------------------------------------------------------
-- string routines
---------------------------------------------------------*/
void str_cpy( char* dest, const char* src, long size )
{
  Assert(dest);
  Assert(size >= 0);
  if (dest == NULL || size <= 0) return;

  if (src == NULL) {
    *dest = 0;
  }
  else {
    char* p        = dest;
    const char* cp = src;
    while (*cp != 0 && p-dest < size) {
      *p = *cp;
      p++;
      cp++;
    }
    if (p-dest < size) *p = 0;
                  else dest[size-1] = 0;
  }
}

void str_cat( char* dest, const char* src, long size )
{
  char* p;
  Assert(dest);
  Assert(size >= 0);
  if (dest == NULL || size == 0) return;

  p = dest;
  while (*p != 0 && p-dest < size) { p++; }
  if (p-dest < size) { str_cpy( p, src, size - (p-dest) ); }
}

long str_len( const char* src )
{
  if (src == NULL) return 0;
  return strlen(src);
}

/*---------------------------------------------------------
-- (v)snprintf
---------------------------------------------------------*/
#if !defined(HAS_SNPRINTF) && !defined(HAS__SNPRINTF)
void FUN_VAR_ARGS3( snprintf, char*, buf, long, size, const char*, fmt, args )
{
  vsprintf( buf, fmt, args );
}
END_ARGS(args)
#endif

#if !defined(HAS_VSNPRINTF) && !defined(HAS__VSNPRINTF)
void vsnprintf( char* buf, long size, const char* fmt, va_list args )
{
  vsprintf( buf, fmt, args );
}
#endif

/*---------------------------------------------------------
-- memmov
---------------------------------------------------------*/
#ifdef USING_MEMMOV

/* This should work on 64-bit machines as well as 32-bit machines.
   It assumes a long is the natural size for memory reads and writes.
*/
void memmov (char * dst, char * src, unsigned long length)
{
  unsigned long i;

  if ((unsigned long) dst <= (unsigned long) src){

      /* Copy in ascending order. */
    if (((unsigned long) src - (unsigned long) dst) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
           Copy byte by byte. */
      for (; length != 0; length--){
        *dst++ = *src++;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i != 0){
        i = sizeof (long) - i;              /* Number of bytes to copy. */
        if (i > length) i = length;         /* Never copy more than length.*/
        for (; i != 0; i--){
          *dst++ = *src++; --length;
        }
      }                    Assert ((unsigned long) dst % sizeof (long) == 0);
                           Assert ((unsigned long) src % sizeof (long) == 0);

      /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
        *(long *) dst = *(long *) src;
        dst += sizeof (long); src += sizeof (long);
      }

      /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
        *dst++ = *src++;
      }
    }
  }else{                                       /* Copy in descending order. */
    src += length; dst += length;
    if (((unsigned long) dst - (unsigned long) src) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
           Copy byte by byte. */
      for (; length > 0; length--){
        *--dst = *--src;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i > length) i = length;           /* Never copy more than length. */
      for (; i > 0; i--){
        *--dst = *--src; --length;
      }

        /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
        dst -= sizeof (long); src -= sizeof (long);
        *(long *) dst = *(long *) src;
      }

        /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
        *--dst = *--src;
      }
    }
  }
}

#endif /* USING_MEMMOV */

char *aligned_malloc (size_t size, int modulo, void **block)
{
  char *raw_mem;
  unsigned long aligned_mem;
  Assert (modulo < Page_bsize);
  raw_mem = (char *) malloc (size + Page_bsize);
  if (raw_mem == NULL) return NULL;
  *block = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_bsize + 1) * Page_bsize);
#ifdef DEBUG
  {
    unsigned long *p;
    unsigned long *p0 = (void *) *block,
                  *p1 = (void *) (aligned_mem - modulo),
                  *p2 = (void *) (aligned_mem - modulo + size),
                  *p3 = (void *) ((char *) *block + size + Page_bsize);

    for (p = p0; p < p1; p++) *p = Debug_filler_align;
    for (p = p1; p < p2; p++) *p = Debug_uninit_align;
    for (p = p2; p < p3; p++) *p = Debug_filler_align;
  }
#endif
  return (char *) (aligned_mem - modulo);
}