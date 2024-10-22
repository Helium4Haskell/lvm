/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef prim_sys_h
#define prim_sys_h

enum open_flag {
  Open_readonly,
  Open_writeonly,
  Open_append,
  Open_create,
  Open_truncate,
  Open_exclusive,
  Open_binary,
  Open_text,
  Open_nonblocking
};

enum create_flag {
  Create_never,
  Create_ifnotexists,
  Create_exclusive,  
  Create_overwrite
};

long prim_input_flags( long astext );
long prim_output_flags( long astext, enum create_flag flag );


long prim_flag_mask( enum open_flag flag );
long prim_open(const char* path, long flags, long perm);
void prim_close(long fd);

#define NO_ARG Val_int(0)
void sys_error (value);

#endif
