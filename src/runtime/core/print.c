/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include "mlvalues.h"
#include "memory.h"
#include "module.h"
#include "instr.h"
#include "print.h"
#include "thread.h"

const char* String_of_size( wsize_t size, const char* mod )
{
static char buf[MAXSTR];
  if (size < Kilo)
    snprintf( buf, MAXSTR, "%lu%s", size, mod );
  else if (size < Mega)
    snprintf( buf, MAXSTR, "%lu%c%s", size / Kilo, 'k', mod );
  else if (size < 10*Mega && ((size % Mega) / (Kilo*(Kilo/10)) != 0))
    snprintf( buf, MAXSTR, "%lu.%lu%c%s", size / Mega, (size % Mega) / (Kilo*(Kilo/10)), 'm', mod );
  else if (size < Giga)
    snprintf( buf, MAXSTR, "%lu%c%s", size / Mega, 'm', mod );
  else if ((size % Giga) / (Mega*(Kilo/10)) != 0)
    snprintf( buf, MAXSTR, "%lu.%lu%c%s", size / Giga, (size % Giga) / (Mega*(Kilo/10)), 'g', mod );
  else
    snprintf( buf, MAXSTR, "%lu%c%s", size / Giga, 'g', mod );


  return buf;
}


void FUN_VAR_ARGS1( print, const char*, msg, args )
{
  vmessage(msg,args);
}
END_ARGS(args);



#define Max_level   4

static void _print_value( int level, value module, value v );
static void _print_block( int level, const char* name, value module, value v )
{
  wsize_t i;
  if (level > Max_level) { print("..."); return; }

  print( "[%s", name );
  for( i = 0; i < Wosize_val(v); i++ )
  {
    print( " " );
    _print_value( level+1, module, Field(v,i) );
  }
  print( "]" );
}

static void _print_value( int level, value module, value v )
{
  if (level > Max_level) { print( "..."); return; }

  if (v == 0)
  {
    print( "[inv]" );
  }
  else if (Is_long(v))
  {
    print( "[int %li]", Long_val(v) );
  }
  else if (Is_atom(v))
  {
    print( "[atom %li]", Tag_val(v) );
  }
  else if (is_heap_val(v))
  {
    switch (Tag_val(v))
    {
    case String_tag: print( "[string \"%s\"]", String_val(v)); break;
    case Ap_tag:     _print_block( level, "ap", module, v ); break;
    case Code_tag:   print( "%s", find_name_of_code( module, v )); break;
    case Nap_tag:    _print_block( level, "nap", module, v ); break;
    case Ind_tag:    _print_block( level, "ind", module, v ); break;
    case Caf_tag:    print( "[caf %s]", find_name_of_code( module, v )); break;
    case Inv_tag:    print( "[blackhole]" ); break;
    case Suspend_tag:print( "[suspension]" ); break;
    case Double_tag: print( "[float %g]", Double_val(v) ); break;
    default        : if (Wosize_val(v) <= 4) {
                       char buf[16];
                       snprintf( buf, 16, "tag %i", Tag_val(v) );
                       _print_block( level, buf, module, v );
                     } else
                      print( "[block size=%li tag=%li]", Wosize_val(v), Tag_val(v) );
                     break;
    }
  }
  else if (is_code_val(module,v)) {
    print( "%s", find_name_of_code(module,v));
  }
  else {
    print( "[ptr 0x%lx]", v );
  }

}

void print_value( value module, value v )
{
  _print_value( 0, module, v );
}

void print_block( const char* name, value module, value v )
{
  _print_block( 0, name, module, v );
}

void print_instr( value module, value* sp, opcode_t* code )
{
  switch (instr_arg_count( code[0] ))
  {
  case 2 : print( "%-10s %-4li %-4li", instr_name(code[0]), code[1], code[2] ); break;
  case 1 : print( "%-10s %-8li",       instr_name(code[0]), code[1] ); break;
  default: print( "%-10s         ",    instr_name(code[0]) ); break;
  }

  switch (code[0])
  {
  case PUSHCODE:  print( " -- %s", find_name_of_code( module, Val_fixup(code[1]) ) ); break;
  case ENTERCODE: print( " -- %s", find_name_of_code( module, Val_fixup(code[1]) ) ); break;
  case PUSHCONT:  print( " -- %s", find_name_of_code( module, Val_code(code + code[1]) ) );     break;
  case PUSHVAR:   print( " -- " ); print_value( module, sp[code[1]] ); break;
  case PUSHVAR0:  print( " -- " ); print_value( module, sp[0] ); break;
  case PUSHVAR1:  print( " -- " ); print_value( module, sp[1] ); break;
  case PUSHVAR2:  print( " -- " ); print_value( module, sp[2] ); break;
  case PUSHVAR3:  print( " -- " ); print_value( module, sp[3] ); break;
  case PUSHVAR4:  print( " -- " ); print_value( module, sp[4] ); break;
  case PUSHINT:   print( " -- " ); print_value( module, Val_long(code[1]) ); break;

  case TESTINT:   print( " -- " ); print_value( module, Val_long(code[1]) ); break;
  case EVALVAR:   print( " -- " ); print_value( module, sp[code[1]] ); break;

  case SWITCHCON:
  case MATCHCON:
  case MATCHINT:  print( " -- " ); print_value( module, sp[0] ); break;
  }

  print( "\n" );
}


/*----------------------------------------------------------------------
 debug dump of the stack
----------------------------------------------------------------------*/
void print_stack( value module, const value* sp, const value* fp )
{
  long local;

  Assert( fp >= sp );
  local = 0;
  while(1) {
    while (fp > sp) {
      print( "%4i -- ", local ); print_value( module, sp[0] ); print( "\n" );
      sp++;
      local++;
    }

    switch( Frame_frame(fp) ) {
    case frame_cont:    print( "continuation" ); break;
    case frame_update:  print( "update" ); break;
    case frame_catch:   print( "catch" ); break;
    case frame_stop:    print( "stop\n" ); return;
    default:            print( "unknown frame!" ); return;
    }

    print( " -- " ); print_value( module, Frame_value(fp) ); print( "\n" );
    local += Frame_size;
    sp    += Frame_size;
    fp     = Frame_next(fp);
  }
}


/*----------------------------------------------------------------------
 supposedly nice trace of the stack but still quite rough
 since Caf's and Update's are overwritten by the exception so
 we lose the trace
----------------------------------------------------------------------*/
void print_stack_trace( value module, const value* fp, int max_trace )
{
#define Max_trace 5
  value vs[Max_trace];    /* bottom traces */
  value ws[Max_trace];    /* top traces */
  int   trace_count;
  int   trace_total;
  int   i;

  /* find the traces on the stack */
  trace_count = 0;
  trace_total = 0;
  while(Frame_frame(fp) != frame_stop) {
    switch (Frame_frame(fp)) {
    case frame_cont:
      trace_total++;
      /* are we in the bottom traces */
      if (trace_count < Max_trace) {
        vs[trace_count] = Frame_value(fp);
        trace_count++;
      }
      /* are we in the top traces */
      else if (trace_count < (Max_trace*2)) {
        ws[trace_count-Max_trace] = Frame_value(fp);
        trace_count++;
      }
      /* the top traces are filled, shift one out to put a new one in */
      else {
        for( i = Max_trace-1; i > 0; i-- ) ws[i-1] = ws[i];
        ws[trace_count-Max_trace-1] = Frame_value(fp);
      }
      break;
    default:
      break;
    }
    fp = Frame_next(fp);
  }

  /* print the traces */
  if (trace_count == 0) return;
  print( "\ntrace:\n" );

  for( i = 0; i < trace_count && i < Max_trace; i++) {
    print( "  demanded from \"" ); print_value( module, vs[i] ); print("\"\n");
  }

  if (trace_count <= Max_trace)  return;
  if (trace_total > Max_trace*2) print( "  ...\n" );

  for( i = 0; i < (trace_count - Max_trace) && i < Max_trace; i++) {
    print( "  demanded from \"" ); print_value( module, ws[i] ); print("\"\n");
  }

  return;
}
