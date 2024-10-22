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

void sys_init (char **);

int file_open_binary( const char* name, int mode );
int file_open( const char* name, int mode );
int file_close( int handle );
int file_read( int handle, void* buffer, unsigned int count );
long file_skip( int handle, long count );

const char* searchpath_dll( const char* name );
const char* searchpath_lvm( const char* name );
const char* searchpath(const char* path, const char * name, const char* ext );

bool is_pathsep( const char c );
bool is_filesep( const char c );
void normalize_path( char* path );

nat msecs_of_ticks(nat ticks );
nat get_msec_count(void);
nat get_tick_count(void);
void  get_process_ticks( nat* tick_total, nat* tick_user, nat* tick_system );

#endif /* _sys_ */
