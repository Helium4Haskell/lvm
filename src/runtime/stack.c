/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <string.h>
#include "mlvalues.h"
#include "fail.h"
#include "lvmmemory.h"
#include "alloc.h"
#include "thread.h"

/*----------------------------------------------------------------------
  lazy black holing
  walk through all the frames on the stack and blackhole all AP nodes.
----------------------------------------------------------------------*/
void lazy_blackhole( value* fp )
{
  while(1)
  {
    switch(Frame_frame(fp)) {
    case frame_update: {
      value upd = Frame_value(fp);
      tag_t tag = Tag_val(upd);
      Assert( Is_block(upd) );
      if (tag == Ap_tag) {
        wsize_t i;
        Tag_val(upd) = Inv_tag;
        for (i = 0; i < Wosize_val(upd); i++) { Store_field( upd, i, 0 ); }
      }
      else if (tag == Caf_tag) {
        Tag_val(upd) = Inv_tag;
      }
      else {
        Assert( tag == Inv_tag );
      }
      break;
    }
    case frame_catch:
    case frame_cont:
      break;
    case frame_stop:
      return;
    default:
      raise_internal( "invalid stack frame during black holing" );
    }

    fp = Frame_next(fp);
  }
}


/*----------------------------------------------------------------------
   recover from exceptions
   semisynchronous  == heap_overflow, no suspension and no update
----------------------------------------------------------------------*/
value* recover_semi_synchronous( value* fp )
{
  while(1) {
    switch(Frame_frame(fp)) {
    case frame_update:
    case frame_cont:
      break;
    default:
      return fp;
    }
    fp = Frame_next(fp);
  }
}

value* recover_synchronous( value* fp, value exn )
{
  while(1) {
    switch(Frame_frame(fp)) {
    case frame_update: {
      value upd = Frame_value(fp);
      if (Tag_val(upd) != Raise_tag) {
        wsize_t i;
        Tag_val(upd) = Raise_tag;
        Store_field(upd,0,exn);
        for (i = 1; i < Wosize_val(upd); i++) { Store_field(upd,i,0); }
      }
      break;
    }
    case frame_cont:
      break;
    default:
      return fp;
    }
    fp = Frame_next(fp);
  }
}

static value alloc_suspension( value* sp, wsize_t ssize, long base, long top )
{
  CAMLparam0();
  CAMLlocal1(susp);

  wsize_t size = ssize + Susp_info_wosize;
  wsize_t i;

  /* allocate a new suspension */
  if (size >  Max_young_wosize) susp = alloc_shr(size,Suspend_tag);
                         else   susp = alloc_small(size,Suspend_tag);
  Field(susp,Field_susp_base) = Val_long(base);
  Field(susp,Field_susp_top)  = Val_long(top);
  if (size >  Max_young_wosize) {
    for( i = 0; i < ssize; i++ ) { Init_field(susp,i+Susp_info_wosize,sp[i]); }
  } else {
    for( i = 0; i < ssize; i++ ) { Field(susp,i+Susp_info_wosize) = sp[i]; }
  }

  CAMLreturn(susp);
}

value* recover_asynchronous( value* fp, value* sp )
{
  CAMLparam0();
  CAMLlocal2(susp,upd);

  while(1) {
    long base;
    long top;

    top   = -1;
    base  = fp - sp;
    Assert( base > 0 );

    /* ignore continuation frames */
    while(Frame_frame(fp) == frame_cont) {
      fp = Frame_next(fp);
    }

    top = fp - sp;

    /* create a suspension if we hit an update frame */
    if(Frame_frame(fp) == frame_update) {
      /* update with a suspension */
      susp = alloc_suspension( sp, fp - sp + Frame_size, base, top );
      upd  = Frame_value(fp);
      Indirect(upd,susp);

      /* cut the stack & push the suspension
         this will change the stack but everything is still GC-safe and
         the stack is always cut at the catch or stop frame */
      sp = fp + Frame_size - 1;
      fp = Frame_next(fp);
      sp[0] = susp;
    }
    else {
      CAMLreturn(fp);
    }
  }
}
