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
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _sys_
#define _sys_

#include "mlvalues.h"

#define MAX_PATH_LENGTH 1024

void sys_init (char **);

int file_open_binary( const char* name, int mode );
int file_open( const char* name, int mode );
int file_close( int handle );
int file_read( int handle, void* buffer, unsigned int count );
long file_skip( int handle, long count );
const char * searchpath (const char* path, const char * name, const char* ext );

ulong msecs_of_ticks(ulong ticks );
ulong get_msec_count(void);
ulong get_tick_count(void);
void  get_process_ticks( ulong* tick_total, ulong* tick_user, ulong* tick_system );


#endif /* _sys_ */
