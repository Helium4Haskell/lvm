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

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "signals.h"
#include "sys.h"

/*----------------------------------------------------------------------
-- system errors
----------------------------------------------------------------------*/
/*
extern int errno;

#ifdef HAS_STRERROR
  extern char * strerror(int);

  char * strerror_message(void)
  {
    return strerror(errno);
  }
#else
  extern int sys_nerr;
  extern char * sys_errlist [];

  char * strerror_message(void)
  {
    if (errno < 0 || errno >= sys_nerr)
      return "unknown error";
    else
      return sys_errlist[errno];
  }
#endif

#ifndef EAGAIN
# define EAGAIN (-1)
#endif

#ifndef EWOULDBLOCK
# define EWOULDBLOCK (-1)
#endif

mlsize_t string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}

void sys_error(value arg)
{
  CAMLparam1 (arg);
  char* err;
  char buf[MAXSTR];

  if (errno == EAGAIN || errno == EWOULDBLOCK) {
    raise_sys_blocked_io();
  } else {
    err = strerror_message();
    if (arg != NO_ARG) {
      snprintf( buf, MAXSTR, "%s: %s", String_val(arg), err );
      err = buf;
    }
    raise_sys_error(errno,err);
  }
}

*/

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
-- searchpath
----------------------------------------------------------------------*/

#if defined(OS_WINDOWS)

const char * searchpath(const char* path, const char * name, const char* ext )
{
  static char fullname[MAX_PATH_LENGTH];
  char * filepart;

  if (SearchPath(path,
                 name,
                 ext,
                 MAX_PATH_LENGTH,   /* size of buffer */
                 fullname,
                 &filepart))
    return fullname;
  else
    return name;
}

#else

#if defined(OS_CYGWIN)
/* Cygwin needs special treatment because of the implicit ".exe" at the
   end of executable file names */

static bool searchpath_file_ok(const char * name)
{
  int fd;
  /* Cannot use stat() here because it adds ".exe" implicitly */
  fd = open(name, O_RDONLY);
  if (fd == -1) return false;
  close(fd);
  return true;
}

#else /* various unixes */

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

static bool searchpath_file_ok(const char * name)
{
  struct stat st;
  return (stat(name, &st) == 0 && S_ISREG(st.st_mode));
}


#endif


const char * searchpath(const char* path, const char * name, const char* ext )
{
  static char fullname[MAX_PATH_LENGTH];
  const char * cp;
  char* p;

  if (path == NULL) return name;

  /* fullname = malloc(strlen(name) + (path == NULL ? 0 : strlen(path)) + 6); */
  /* 6 = "/" plus ".exe" plus final "\0" */
  if (strlen(name) + (path == NULL ? 0 : strlen(path)) + 6 > MAX_PATH_LENGTH)
    return 0;

  if (fullname == NULL) return name;
  /* Check for absolute path name */
  for (cp = name; *cp != 0; cp++) {
    if (*cp == '/' || *cp == '\\') {
      if (searchpath_file_ok(name)) return name;
      strcpy(fullname, name);
      strcat(fullname, ext);
      if (searchpath_file_ok(name)) return fullname;
      return name;
    }
  }
  /* Search in path */
  if (path == NULL) return 0;
  while(1) {
    for (p = fullname; *path != 0 && *path != ':' && *path != ';'; p++, path++) *p = *path;
    if (p != fullname) *p++ = '/';
    strcpy(p, name);
    if (searchpath_file_ok(fullname)) return fullname;
    strcat(fullname, ext);
    if (searchpath_file_ok(fullname)) return fullname;
    if (*path == 0) break;
    path++;
  }
  return 0;
}


#endif
