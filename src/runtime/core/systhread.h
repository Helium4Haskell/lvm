/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _systhread_h
#define _systhread_h

#include "mlvalues.h"

#if defined(OS_WINDOWS)
# include <windows.h>
# define mutex  HANDLE
#elif defined(HAS_PTHREAD_H)
# include <pthread.h>
# define mutex  pthread_mutex_t
#else
# define mutex  int
#endif

void mutex_init( mutex* m );
void mutex_done( mutex* m );
void mutex_lock( mutex* m );
void mutex_unlock( mutex* m );


#endif /* systhread_h */