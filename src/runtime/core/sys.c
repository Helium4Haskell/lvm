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

/* Basic system calls */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "mlvalues.h"

#if defined(HAS_WINDOWS_H) && defined(OS_WINDOWS)
#include <windows.h>
#endif

#if defined(HAS_IO_H) && defined(OS_WINDOWS)
# include <io.h>
# define open  _open
# define close _close
# define read  _read
#endif

#if defined(HAS_TIMES_H)
#include <times.h>
#endif

#if defined(HAS_SYS_TIMES_H)
#include <sys/times.h>
#endif

#if defined(HAS_TIME_H)
#include <time.h>
#endif

#if defined(HAS_UNISTD_H)
#include <unistd.h>
#endif

#ifdef HAS_FLOAT_H
#include <float.h>
#endif

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "signals.h"
#include "sys.h"
#include "options.h"

/*----------------------------------------------------------------------
-- file open/close/read
----------------------------------------------------------------------*/
#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif
#ifndef O_NONBLOCK
#ifdef O_NDELAY
#define O_NONBLOCK O_NDELAY
#else
#define O_NONBLOCK 0
#endif
#endif

int file_open_binary( const char* name, int mode )
{
  return file_open( name, mode | O_BINARY );
}

int file_open( const char* name, int mode )
{
  return open( name, mode );
}

int file_close( int handle )
{
  return close( handle );
}

int file_read( int handle, void* buffer, unsigned int count )
{
  return read( handle, buffer, count );
}

long file_skip( int handle, long count )
{
  return lseek(handle,count,SEEK_CUR);
}


/*----------------------------------------------------------------------
-- timers
----------------------------------------------------------------------*/
#if defined(CLK_TCK)              /* strangely, CLK_TCK is more trustable that CLOCKS_PER_SEC ?? */
#  define TICKS_PER_SEC CLK_TCK
#elif defined(CLOCKS_PER_SEC)
#  define TICKS_PER_SEC CLOCKS_PER_SEC
#elif defined(HZ)
#  define TICKS_PER_SEC HZ
#else
#  define TICKS_PER_SEC 60
#endif


nat msecs_of_ticks( nat ticks )
{
  return ((ticks * 1000) / TICKS_PER_SEC);
}

nat get_tick_count(void)
{
#if defined(HAS_TIME_H)
  /* clock() is standard ANSI C */
  return clock();

#elif defined(HAS_TIMES_H) || defined(HAS_SYS_TIMES_H)
  struct tms t;
  times(&t);
  return (t.tms_utime + t.tms_stime);
#else
# error "no clock on this system"
#endif
}

nat get_msec_count(void)
{
  return msecs_of_ticks( get_tick_count() );
}



void get_process_ticks( nat* tick_total, nat* tick_user, nat* tick_system )
{
#if defined(OS_WINDOWS)
  /* FILETIME's are in 100's of nanoseconds. */
# define TICKS_OF_TIME(t)  ((t) / (10000000L / TICKS_PER_SEC) )

  FILETIME creation_time, exit_time, kernel_time, user_time;

  if (GetProcessTimes( GetCurrentProcess(), &creation_time, &exit_time, &kernel_time, &user_time) == 0) {
    if (tick_total)  *tick_total = 0;
    if (tick_user)   *tick_user  = 0;
    if (tick_system) *tick_system= 0;
    return;
  }
  Assert(kernel_time.dwHighDateTime == 0);
  Assert(user_time.dwHighDateTime == 0);

  if (tick_total) {
    *tick_total = TICKS_OF_TIME(user_time.dwLowDateTime + kernel_time.dwLowDateTime);
  }
  if (tick_user) {
    *tick_user = TICKS_OF_TIME(user_time.dwLowDateTime);
  }
  if (tick_system) {
    *tick_system = TICKS_OF_TIME(kernel_time.dwLowDateTime);
  }
  return;

#elif defined(HAS_TIMES_H) || defined(HAS_SYS_TIMES_H)
  struct tms t;
  times(&t);

  if (tick_total) {
    *tick_total = t.tms_utime + t.tms_stime;
  }
  if (tick_user) {
    *tick_user = t.tms_utime;
  }
  if (tick_system) {
    *tick_system = t.tms_stime;
  }
  return;

#else
# error "no process ticks on this system"
#endif

}


/*----------------------------------------------------------------------
-- file and path names
----------------------------------------------------------------------*/
bool is_pathsep( const char c )
{
#if defined(OS_WINDOWS) || defined(OS_CYGWIN)
  return (c==';');
#else
  return (c==';' || c == ':');
#endif
}

bool is_filesep( const char c )
{
  return (c=='\\' || c == '/');
}

void normalize_path( char* path )
{
  char* p;
  if (path == NULL) return;

  for( p = path; *p != 0; p++ ) {
    if (is_filesep(*p)) {
      *p = FILESEP;
    }
    else if (is_pathsep(*p)) {
      *p = PATHSEP;
    }
  }
}

/*----------------------------------------------------------------------
-- searchpath
----------------------------------------------------------------------*/
const char* searchpath_lvm( const char* name )
{
  return searchpath( lvmpath, name, ".lvm" );
}

const char* searchpath_dll( const char* name )
{
  if (dllpath) {
    /* try the LVMDLLPATH first */
    const char* path = searchpath( dllpath, name, DLL );
    if (path) return path;
  }
  /* and also the system search path */
  return searchpath( NULL, name, DLL );
}


#if defined(OS_CYGWIN)
/* Cygwin needs special treatment because of the implicit ".exe" at the
   end of executable file names */
static bool file_exist(char * name)
{
  int fd;
  normalize_path(name);
  /* Cannot use stat() here because it adds ".exe" implicitly */
  fd = open(name, O_RDONLY);
  if (fd == -1) return false;
  close(fd);
  return true;
}

#else /* various unix's & windows */

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

static bool file_exist(char * name)
{
  struct stat st;
  normalize_path(name);
  return (stat(name, &st) == 0 && S_ISREG(st.st_mode));
}

#endif


#if defined(OS_WINDOWS)

const char * searchpath(const char* path, const char * name, const char* ext )
{
  static char fullname[MAXPATH];
  static char normname[MAXPATH];
  char* filepart;
  const char* cp;

  str_cpy(normname,name,MAXPATH);
  normalize_path(normname);

  /* check for absolute path name */
  for (cp = normname; *cp != 0; cp++) {
    if (is_filesep(*cp)) {
      str_cpy(fullname,normname,MAXPATH);
      if (file_exist(fullname)) return fullname;
      str_cat(fullname, ext, MAXPATH);
      if (file_exist(fullname)) return fullname;
      break;
    }
  }

  /* otherwise, search along the path (NULL == default path) */
  if (SearchPath(path,
                 normname,
                 ext,
                 MAXPATH,   /* size of buffer */
                 fullname,
                 &filepart))
    return fullname;
  else
    return NULL;
}

#else /* various unix's */



const char * searchpath(const char* path, const char * name, const char* ext )
{
  static char fullname[MAXPATH];
  static char normname[MAXPATH];
  const char * cp;

  if (name == NULL) return NULL;
  if (ext  == NULL) ext  = "";
  if (path == NULL) path = getenv("PATH"); /* NULL == default system path */

  str_cpy(normname,name,MAXPATH);
  normalize_path(normname);

  /* Check for absolute path name */
  for (cp = normname; *cp != 0; cp++) {
    if (is_filesep(*cp)) {
      str_cpy(fullname,normname,MAXPATH);
      if (file_exist(fullname)) return fullname;
      str_cat(fullname, ext, MAXPATH);
      if (file_exist(fullname)) return fullname;
      break;
    }
  }

  /* Search in path */
  while(path && *path != 0) {
    long len;
    char filesep[2];
    filesep[0] = FILESEP;
    filesep[1] = 0;

    for( len = 0; path[len] != 0 && !is_pathsep(path[len]); len++) { /* nothing */ }
    if (len > 0 && len < MAXPATH) {
      str_cpy( fullname, path, len+1 );
      normalize_path(fullname);
      if (len > 0) str_cat( fullname, filesep, MAXPATH );
      str_cat( fullname, normname, MAXPATH );
      if (file_exist(fullname)) return fullname;
      str_cat(fullname, ext, MAXPATH);
      if (file_exist(fullname)) return fullname;
    }
    path = path+len;
    if (*path != 0) path++; /* skip seperator */
  }

  return NULL;
}


#endif
