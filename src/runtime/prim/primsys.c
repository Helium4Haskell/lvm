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

#if defined(HAS_DIRECT_H) && defined(OS_WINDOWS)
# include <direct.h>
# define chdir  _chdir
# define getcwd _getcwd
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
#include "primsys.h"

/*----------------------------------------------------------------------
-- system errors
----------------------------------------------------------------------*/
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
#endif /* HAS_STRERROR */

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

/*----------------------------------------------------------------------
-- File operations
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

static int sys_open_flags[] = {
  O_RDONLY, O_WRONLY, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
  O_BINARY, O_TEXT, O_NONBLOCK
};

long prim_flag_mask( enum open_flag flag )
{
  if (flag >= 0 && flag <= Open_nonblocking)
    return sys_open_flags[flag];
  else
    return 0;
}

long prim_open(const char* path, long flags, long perm)
{
  int ret;
  ret = open(String_val(path), flags
#if !macintosh
             , Int_val(perm)
#endif
                                       );
  if (ret == -1) sys_error(copy_string(path));
  return ret;
}

void prim_close(long fd)
{
  close(fd);
}

value sys_file_exists(value name)     /* ML */
{
#if macintosh
  int f;
  f = open (String_val (name), O_RDONLY);
  if (f == -1) return (Val_bool (0));
  close (f);
  return (Val_bool (1));
#else
  struct stat st;
  return Val_bool(stat(String_val(name), &st) == 0);
#endif
}

value sys_remove(value name)          /* ML */
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(name);
  return Val_unit;
}

value sys_rename(value oldname, value newname) /* ML */
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    sys_error(oldname);
  return Val_unit;
}

/*
value sys_chdir(value dirname)
{
  if (chdir(String_val(dirname)) != 0) sys_error(dirname);
  return Val_unit;
}

value sys_getcwd(value unit)
{
  char buff[4096];
#ifdef HAS_GETCWD
  if (getcwd(buff, sizeof(buff)) == 0) sys_error(NO_ARG);
#else
  if (getwd(buff) == 0) sys_error(NO_ARG);
#endif
  return copy_string(buff);
}
*/

/*
value sys_getenv(value var)
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) raise_not_found();
  return copy_string(res);
}


char ** caml_main_argv;

value sys_get_argv(value unit)
{
  return copy_string_array((char const **) caml_main_argv);
}

void sys_init(char **argv)
{
  caml_main_argv = argv;
}

*/

/*
#if !(defined(WIFEXITED) && defined(WEXITSTATUS))
*/ /* Assume old-style V7 status word */
/*
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#endif

#ifdef _WIN32
extern int win32_system(char * command);
#endif

value sys_system_command(value command)
{
  int status, retcode;

  enter_blocking_section ();
#ifndef _WIN32
  status = system(String_val(command));
  if (WIFEXITED(status))
    retcode = WEXITSTATUS(status);
  else
    retcode = 255;
#else
  status = retcode = win32_system(String_val(command));
#endif
  leave_blocking_section ();
  if (status == -1) sys_error(command);
  return Val_int(retcode);
}


value sys_time(value unit)
{
#ifdef HAS_TIMES
#ifndef CLK_TCK
#ifdef HZ
#define CLK_TCK HZ
#else
#define CLK_TCK 60
#endif
#endif
  struct tms t;
  times(&t);
  return copy_double((double)(t.tms_utime + t.tms_stime) / CLK_TCK);
#else
  return copy_double((double)clock() / CLOCKS_PER_SEC);
#endif
}

value sys_random_seed (value unit)
{
#ifdef HAS_GETTIMEOFDAY
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return Val_int(tv.tv_sec ^ tv.tv_usec);
#else
  return Val_int(time (NULL));
#endif
}


*/