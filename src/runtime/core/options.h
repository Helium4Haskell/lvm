/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id: */

#ifndef _options_h
#define _options_h

/* init/done */
const char** init_options( const char** args );
void done_options( bool showreports );
void show_options( void );

/* global options */
extern wsize_t stack_wsize_init;
extern wsize_t stack_wsize_max;
extern wsize_t stack_wsize_threshold;

extern wsize_t stack_wsize_total;
extern wsize_t stack_wsize_peak;

extern const char* lvmpath;
extern const char* dllpath;
extern const char* argv0;

#endif /* _options_h */
