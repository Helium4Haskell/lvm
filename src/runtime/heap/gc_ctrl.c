/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/***---------------------------------------------------------------------
  Modified and adapted for the Lazy Virtual Machine by Daan Leijen.
  Modifications copyright 2001, Daan Leijen. This (modified) file is
  distributed under the terms of the GNU Library General Public License.
---------------------------------------------------------------------***/

/* Id: gc_ctrl.c,v 1.1 2001/04/11 14:00:09 daan Exp $ */

#include "mlvalues.h"
#include "alloc.h"
#include "compact.h"
#include "custom.h"
#include "finalise.h"
#include "heap.h"
#include "core/thread.h"


/* LVM: changed [long] to [unsigned long] */
unsigned long
     stat_minor_words = 0,
     stat_promoted_words = 0,
     stat_major_words = 0,
     stat_minor_collections = 0,
     stat_major_collections = 0,
     stat_heap_size = 0,              /* bytes */
     stat_compactions = 0,
     stat_max_heap_size = 0,
     stat_peak_heap_bsize = 0;

extern asize_t major_heap_increment;  /* bytes; see major_gc.c */
extern unsigned long percent_free;    /*        see major_gc.c */
extern unsigned long percent_max;     /*        see compact.c */

#define Next(hp) ((hp) + Bhsize_hp (hp))

#ifdef DEBUG

/* Check that [v]'s header looks good.  [v] must be a block in the heap. */
static void check_head (value v)
{
  Assert (Is_block (v));
  Assert (Is_in_heap (v));

  Assert (Wosize_val (v) != 0);
  Assert (Color_hd (Hd_val (v)) != Caml_blue);
  Assert (Is_in_heap (v));
  if (Tag_val (v) == Infix_tag){
    int offset = Wsize_bsize (Infix_offset_val (v));
    value trueval = Val_op (&Field (v, -offset));
    Assert (Tag_val (trueval) == Closure_tag);
    Assert ((int)Wosize_val (trueval) > offset);
    Assert (Is_in_heap (&Field (trueval, Wosize_val (trueval) - 1)));
  }else{
    Assert (Is_in_heap (&Field (v, Wosize_val (v) - 1)));
  }
  if (Tag_val (v) ==  Double_tag){
    Assert (Wosize_val (v) == Double_wosize);
  }else if (Tag_val (v) == Double_array_tag){
    Assert (Wosize_val (v) % Double_wosize == 0);
  }
}

static void check_block (char *hp)
{
  mlsize_t nfields = Wosize_hp (hp);
  mlsize_t i;
  value v = Val_hp (hp);
  value f;

  check_head (v);
  switch (Tag_hp (hp)){
  case Abstract_tag: break;
  case String_tag: {
    /* not true when check_urgent_gc is called by alloc or alloc_string:
       mlsize_t lastbyte = Bosize_val (v) - 1;
       i = Byte (v, lastbyte);
       Assert (i >= 0);
       Assert (i < sizeof (value));
       Assert (Byte (v, lastbyte - i) == 0);
    */
    break;
    }
  case Double_tag:
    Assert (Wosize_val (v) == Double_wosize);
    break;
  case Double_array_tag:
    Assert (Wosize_val (v) % Double_wosize == 0);
    break;
  case Custom_tag:
    Assert (!Is_in_heap (Custom_ops_val (v)));
    break;

  case Infix_tag:
    Assert (0);
    break;

  default:
    Assert (Tag_hp (hp) < No_scan_tag);
    for (i = 0; i < Wosize_hp (hp); i++){
      f = Field (v, i);
      if (Is_block (f) && Is_in_heap (f)) check_head (f);
    }
  }
}

#endif /* DEBUG */

/* Check the heap structure (if compiled in debug mode) and
   gather statistics; return the stats if [returnstats] is true,
   otherwise return [Val_unit].
*/
static value heap_stats (int returnstats)
{
/* LVM: changed [long] to [unsigned long] */
  CAMLparam0();
  unsigned long live_words = 0, live_blocks = 0,
       free_words = 0, free_blocks = 0, largest_free = 0,
       fragments = 0, heap_chunks = 0;
  char *chunk = heap_start, *chunk_end;
  char *cur_hp, *prev_hp;
  header_t cur_hd;

#ifdef DEBUG
  gc_message (1, "### LVM runtime: heap check ###\n", 0);
#endif

  while (chunk != NULL){
    ++ heap_chunks;
    chunk_end = chunk + Chunk_size (chunk);
    prev_hp = NULL;
    cur_hp = chunk;
    while (cur_hp < chunk_end){
      cur_hd = Hd_hp (cur_hp);
                                           Assert (Next (cur_hp) <= chunk_end);
      switch (Color_hd (cur_hd)){
      case Caml_white:
        if (Wosize_hd (cur_hd) == 0){
        /*
          ++fragments;
          Assert (prev_hp == NULL
                  || (Color_hp (prev_hp) != Caml_blue
                      && Wosize_hp (prev_hp) > 0)
                  || cur_hp == gc_sweep_hp);

          Assert (Next (cur_hp) == chunk_end
                  || (// Color_hp (Next (cur_hp)) != Caml_blue && // result from Truncate
                      Wosize_hp (Next (cur_hp)) > 0
                      )
                  || Next (cur_hp) == gc_sweep_hp);
        }else{
          if (gc_phase == Phase_mark || gc_phase == Phase_idle){
        */
          ++fragments;
          Assert (prev_hp == NULL
                  || Color_hp (prev_hp) != Caml_blue
                  || cur_hp == gc_sweep_hp);
        }else{
          if (gc_phase == Phase_sweep && cur_hp >= gc_sweep_hp){
            ++ free_blocks;
            free_words += Whsize_hd (cur_hd);
            if (Whsize_hd (cur_hd) > largest_free){
              largest_free = Whsize_hd (cur_hd);
            }
        }else{
            ++ live_blocks;
            live_words += Whsize_hd (cur_hd);
#ifdef DEBUG
            check_block (cur_hp);
#endif
          }
        }
        break;
      case Caml_gray: case Caml_black:
        Assert (Wosize_hd (cur_hd) > 0);
        ++ live_blocks;
        live_words += Whsize_hd (cur_hd);
#ifdef DEBUG
        check_block (cur_hp);
#endif
        break;
      case Caml_blue:
        Assert (Wosize_hd (cur_hd) > 0);
        ++ free_blocks;
        free_words += Whsize_hd (cur_hd);
        if (Whsize_hd (cur_hd) > largest_free){
          largest_free = Whsize_hd (cur_hd);
        }
        Assert (prev_hp == NULL
                || ( Color_hp (prev_hp) != Caml_blue ) // && Wosize_hp (prev_hp) > 0) // result from Truncate
                || cur_hp == gc_sweep_hp
                );
        Assert (Next (cur_hp) == chunk_end
                || (Color_hp (Next (cur_hp)) != Caml_blue &&
                    Wosize_hp (Next (cur_hp)) > 0)
                || Next (cur_hp) == gc_sweep_hp);
        break;
      }
      prev_hp = cur_hp;
      cur_hp = Next (cur_hp);
    }                                          Assert (cur_hp == chunk_end);
    chunk = Chunk_next (chunk);
  }

  Assert (live_words + free_words + fragments == Wsize_bsize (stat_heap_size));

  if (returnstats){
    CAMLlocal1 (res);

    /* get a copy of these before allocating anything... */
    double minwords = stat_minor_words
                      + (double) Wsize_bsize (young_end - young_ptr);
    double prowords = stat_promoted_words;
    double majwords = stat_major_words + (double) allocated_words;
    long mincoll = stat_minor_collections;
    long majcoll = stat_major_collections;
    long heap_words = Wsize_bsize (stat_heap_size);
    long cpct = stat_compactions;
    long top_heap_words = Wsize_bsize (stat_peak_heap_bsize);

    res = alloc_tuple (15);
    Store_field (res, 0, copy_double (minwords));
    Store_field (res, 1, copy_double (prowords));
    Store_field (res, 2, copy_double (majwords));
    Store_field (res, 3, Val_long (mincoll));
    Store_field (res, 4, Val_long (majcoll));
    Store_field (res, 5, Val_long (heap_words));
    Store_field (res, 6, Val_long (heap_chunks));
    Store_field (res, 7, Val_long (live_words));
    Store_field (res, 8, Val_long (live_blocks));
    Store_field (res, 9, Val_long (free_words));
    Store_field (res, 10, Val_long (free_blocks));
    Store_field (res, 11, Val_long (largest_free));
    Store_field (res, 12, Val_long (fragments));
    Store_field (res, 13, Val_long (cpct));
    Store_field (res, 14, Val_long (top_heap_words));
    CAMLreturn (res);
  }else{
    CAMLreturn (Val_unit);
  }
}


#ifdef DEBUG
void heap_check (void)
{
  heap_stats (0);
}
#endif

value gc_stat(value v) /* ML */
{
  Assert (v == Val_unit);
  return heap_stats (1);
}

value gc_counters(value v) /* ML */
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  double minwords = stat_minor_words
                    + (double) Wsize_bsize (young_end - young_ptr);
  double prowords = stat_promoted_words;
  double majwords = stat_major_words + (double) allocated_words;

  res = alloc_tuple (3);
  Store_field (res, 0, copy_double (minwords));
  Store_field (res, 1, copy_double (prowords));
  Store_field (res, 2, copy_double (majwords));
  CAMLreturn (res);
}

value gc_get(value v) /* ML */
{
  value res;

  Assert (v == Val_unit);
  res = alloc_small (6, 0);
  Field (res, 0) = Wsize_bsize (Val_long (minor_heap_size));        /* s */
  Field (res, 1) = Wsize_bsize (Val_long (major_heap_increment));   /* i */
  Field (res, 2) = Val_long (percent_free);                         /* o */
  Field (res, 3) = Val_long (verb_gc);                              /* v */
  Field (res, 4) = Val_long (percent_max);                          /* O */
#ifndef NATIVE_CODE
  Field (res, 5) = thread_get_stack_max_wsize(get_current_thread());       /* l */
#else
  Field (res, 5) = 0;
#endif
  return res;
}

#define Max(x,y) ((x) < (y) ? (y) : (x))

static unsigned long norm_pfree (long unsigned int p)
{
  return Max (p, 1);
}

static unsigned long norm_pmax (long unsigned int p)
{
  return p;
}

static long norm_heapincr (long unsigned int i)
{
#define Psv (Wsize_bsize (Page_bsize))
  i = ((i + Psv - 1) / Psv) * Psv;
  if (i < Heap_chunk_min_wsize) i = Heap_chunk_min_wsize;
  if (i > Heap_chunk_max_wsize) i = Heap_chunk_max_wsize;
  return i;
}

static long norm_minsize (long int s)
{
  if (s < Minor_heap_min_wsize) s = Minor_heap_min_wsize;
  if (s > Minor_heap_max_wsize) s = Minor_heap_max_wsize;
  return s;
}

value gc_set(value v) /* ML */
{
  unsigned long newpf, newpm;
  asize_t newheapincr;
  asize_t newminsize;

  verb_gc = Long_val (Field (v, 3));

#ifndef NATIVE_CODE
  thread_set_stack_max_wsize(get_current_thread(), Wsize_bsize(Long_val (Field (v, 5))));
#endif

  newpf = norm_pfree (Long_val (Field (v, 2)));
  if (newpf != percent_free){
    percent_free = newpf;
    gc_message (0x20, "New space overhead: %d%%\n", percent_free);
  }

  newpm = norm_pmax (Long_val (Field (v, 4)));
  if (newpm != percent_max){
    percent_max = newpm;
    gc_message (0x20, "New max overhead: %d%%\n", percent_max);
  }

  newheapincr = norm_heapincr (Bsize_wsize (Long_val (Field (v, 1))));
  if (newheapincr != major_heap_increment){
    major_heap_increment = newheapincr;
    gc_message (0x20, "New heap increment size: %luk bytes\n",
                major_heap_increment/Kilo);
  }

    /* Minor heap size comes last because it will trigger a minor collection
       (thus invalidating [v]) and it can raise [Out_of_memory]. */
  newminsize = norm_minsize (Bsize_wsize (Long_val (Field (v, 0))));
  if (newminsize != minor_heap_size){
    gc_message (0x20, "New minor heap size: %luk bytes\n", newminsize/Kilo);
    set_minor_heap_size (newminsize);
  }
  return Val_unit;
}

value gc_minor(value v) /* ML */
{                                                    Assert (v == Val_unit);
  minor_collection ();
  return Val_unit;
}

value gc_major(value v) /* ML */
{                                                    Assert (v == Val_unit);
  empty_minor_heap ();
  finish_major_cycle ();
  final_do_calls ();
  return Val_unit;
}

void gc_full_major(void)
{
  empty_minor_heap ();
  finish_major_cycle ();
  final_do_calls ();
  empty_minor_heap ();
  finish_major_cycle ();
  final_do_calls ();
  return;
}

value gc_compaction(value v) /* ML */
{                                                    Assert (v == Val_unit);
  empty_minor_heap ();
  finish_major_cycle ();
  finish_major_cycle ();
  compact_heap ();
  return Val_unit;
}


static bool gc_initialised = false;

void done_gc(void)
{
  if (gc_initialised) {
    gc_full_major();
    gc_initialised = false;
  }
}

void init_gc (unsigned long minor_wosize, unsigned long major_wosize,
              unsigned long major_woincr, unsigned long percent_fr,
              unsigned long percent_m,  unsigned long verb,
              unsigned long max_wosize )
{
  unsigned long major_heap_size = Bsize_wsize (norm_heapincr (major_wosize));

#ifdef DEBUG
  gc_message (-1, "### LVM runtime: debug mode "
#ifdef CPU_TYPE_STRING
                               "(" CPU_TYPE_STRING ") "
#endif
                                                       "###\n", 0);
#endif

  verb_gc = verb;
  stat_max_heap_size = Bsize_wsize(max_wosize);

  set_minor_heap_size (Bsize_wsize (norm_minsize (minor_wosize)));
  major_heap_increment = Bsize_wsize (norm_heapincr (major_woincr));
  percent_free = norm_pfree (percent_fr);
  percent_max = norm_pmax (percent_m);
  init_major_heap (major_heap_size);

  gc_message (0x20, "Initial minor heap size: %luk bytes\n",
              minor_heap_size / Kilo);
  gc_message (0x20, "Initial major heap size: %luk bytes\n",
              major_heap_size / Kilo);
  gc_message (0x20, "Initial space overhead: %lu%%\n", percent_free);
  gc_message (0x20, "Initial max overhead: %lu%%\n", percent_max);
  gc_message (0x20, "Initial heap increment: %luk bytes\n",
              major_heap_increment / Kilo);

  gc_initialised = true;
}