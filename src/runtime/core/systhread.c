/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include "mlvalues.h"
#include "fail.h"
#include "sys.h"
#include "systhread.h"

#if defined(OS_WINDOWS)
void mutex_init( mutex* m )
{
  *m = CreateMutex( NULL, FALSE, NULL );
  if (*m == NULL) raise_sys_error( 0, "couldn't create mutex" );
}

void mutex_done( mutex* m )
{
  CloseHandle( *m );
}

void mutex_lock( mutex* m )
{
  WaitForSingleObject( *m, INFINITE );
}

void mutex_unlock( mutex* m )
{
  ReleaseMutex( *m );
}

#elif defined(HAS_PTHREAD_H)
void mutex_init( mutex* m )
{
  int res = pthread_mutex_init( m, NULL );
  if (res != 0) raise_sys_error( 0, "couldn't create mutex" );
}

void mutex_done( mutex* m )
{
  pthread_mutex_destroy( m );
}

void mutex_lock( mutex* m )
{
  pthread_mutex_lock( m );
}

void mutex_unlock( mutex* m )
{
  pthread_mutex_unlock( m );
}


#else
void mutex_init( mutex* m )
{}

void mutex_done( mutex* m )
{}

void mutex_lock( mutex* m )
{}

void mutex_unlock( mutex* m )
{}

#endif
