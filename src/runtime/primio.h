/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/***---------------------------------------------------------------------
  Modified and adapted for the Lazy Virtual Machine by Daan Leijen.
  Modifications copyright 2001, Daan Leijen. This (modified) file is
  distributed under the terms of the GNU Library General Public License.
---------------------------------------------------------------------***/

/* $Id$ */

/* Buffered input/output */

#ifndef _primio_h
#define _primio_h


#include "misc.h"
#include "mlvalues.h"

#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE Page_bsize
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  bool output;                  /* input or output channel? */
  long offset;                  /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
  };

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer, [max].
*/

/* Functions and macros that can be called from C.  Take arguments of
   type struct channel *.  No locking is performed. */

#define putch(channel, ch)                                                  \
  { if ((channel)->curr >= (channel)->end) flush_partial(channel);          \
    *((channel)->curr)++ = (ch); }

#define getch(channel)                                                      \
  ((channel)->curr >= (channel)->max                                        \
   ? refill(channel)                                                        \
   : (unsigned char) *((channel))->curr++)

struct channel * open_descriptor (int,bool output);
void close_channel (struct channel *);
int channel_binary_mode (struct channel *);

int flush_partial (struct channel *);
void flush (struct channel *);
void putword (struct channel *, uint32);
int putblock (struct channel *, const char *, long);
void really_putblock (struct channel *, const char *, long);

unsigned char refill (struct channel *);
uint32 getword (struct channel *);
int getblock (struct channel *, char *, long);
int really_getblock (struct channel *, char *, long);

/* Extract a struct channel * from the heap object representing it */

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

/* The locking machinery */

extern void (*channel_mutex_free) (struct channel *);
extern void (*channel_mutex_lock) (struct channel *);
extern void (*channel_mutex_unlock) (struct channel *);
extern void (*channel_mutex_unlock_exn) (void);

#define Lock(channel) \
  if (channel_mutex_lock != NULL) (*channel_mutex_lock)(channel)
#define Unlock(channel) \
  if (channel_mutex_unlock != NULL) (*channel_mutex_unlock)(channel)
#define Unlock_exn() \
  if (channel_mutex_unlock_exn != NULL) (*channel_mutex_unlock_exn)()

value prim_open_descriptor(long fd, bool out);
void prim_close_channel(value vchannel);
void prim_set_binary_mode(value vchannel, bool mode);
bool prim_flush_partial(value vchannel);
void prim_flush(value vchannel);
void prim_output_char(value vchannel, char ch);
value prim_output(value vchannel, const char* buff, long start, long length);
long prim_input_char(value vchannel);

#endif
