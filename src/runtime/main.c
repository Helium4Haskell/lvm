/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "mlvalues.h"
#include "memory.h"
#include "custom.h"
#include "gc_ctrl.h"

#include "signals.h"
#include "fail.h"
#include "print.h"
#include "module.h"

#include "loader.h"
#include "evaluator.h"
#include "schedule.h"

#include "stats.h"

/* Version string */

static const char* version(void)
{
  static const char* _version = "$Name$";
  static char version_buf[100];

  strncpy( version_buf, _version + 6, 100 );
  version_buf[strlen(version_buf)-1] = 0;
  return version_buf;
}


/* initial heap parameters */
/* Initial speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   heap size.  The rest of the heap is live objects. */
static nat heap_percent_free_init     = 42;
static nat heap_max_percent_free_init = 100; /* Initial setting for the compacter: off */

static nat heap_minor_wsize_init      = 32*Kilo; /* Initial size of the minor zone. (words)  */
static nat heap_chunk_wsize_init      = 64*Kilo; /* Initial size increment when growing the heap. Must be a multiple of [Page_size / sizeof (value)]. */

static nat heap_wsize_init            = 64*Kilo; /* Default initial size of the major heap (words); same constraints as for Heap_chunk_def. */
static nat heap_wsize_max_init        = Wsize_bsize(64*Mega); /* Default maximum size of the heap */

#ifdef DEBUG
static nat heap_verbose_init          = 3;
#else
static nat heap_verbose_init          = 0;
#endif

static bool heap_report               = false;
static bool timings_report            = false;

/* global options */
nat stack_wsize_init       = 4*Kilo;
nat stack_wsize_max        = Wsize_bsize(64*Mega);
nat stack_wsize_threshold  = Kilo;

nat stack_wsize_total = 0;
nat stack_wsize_peak  = 0;

#ifdef LVM_EAGER_LIMITS
nat eager_wsize_max_stack  = 32;
nat eager_wsize_min_stack  = 16;
nat eager_wsize_max_heap   = 32;
#endif

const char* lvmpath = NULL;


/* command line arguments */
static void show_options(void)
{
  const char* env;

  print( "version:%s %s\n", version(), __DATE__ );
  print( "usage:\n" );
  print( " lvmrun [lvm options] <file> [program options]\n" );
  print( "\n" );
  print( "options:\n" );
  print( " -h<size>     the initial heap size.  (%4s)\n", Bstring_of_wsize(heap_wsize_init) );
  print( " -H<size>     the maximal heap size.  (%4s)\n", Bstring_of_wsize(heap_wsize_max_init)  );
  print( " -s<size>     the initial stack size. (%4s)\n", Bstring_of_wsize(stack_wsize_init) );
  print( " -S<size>     the maximal stack size. (%4s)\n", Bstring_of_wsize(stack_wsize_max) );
  print( " -P<path>     the search path.\n" );
  print( " -t           print timing report on stderr after execution. (%s)\n", (timings_report ? "on" : "off" ) );
  print( " -?           show this help screen.\n" );
  print( "\n" );
  print( "advanced options:\n" );
  print( " -he<size>    the heap expansion size.   (%4s)\n", Bstring_of_wsize(heap_chunk_wsize_init) );
  print( " -hm<size>    the minor generation size. (%4s)\n", Bstring_of_wsize(heap_minor_wsize_init) );
  print( " -hf<percent> the percentage of free heap before a major collection. (%3li%%)\n", heap_percent_free_init );
  print( " -hF<percent> the percentage of free heap before a compaction.       (%3li%%)\n", heap_max_percent_free_init );
  print( " -hr          print heap report on stderr after execution.           (%4s)\n", (heap_report ? "on" : "off" ));
  print( " -hv<level>   the heap verbose level. (%lu)\n", heap_verbose_init );
  print( "              level is off (0), only major gc (1), every gc (2) or detailed (3)\n" );
  print( "\n" );
#ifdef LVM_EAGER_LIMITS
  print( " -es<size>    the minimal eager stack size.  (%4s)\n", Wstring_of_wsize(eager_wsize_min_stack)  );
  print( " -eS<size>    the maximal eager stack usage. (%4s)\n", Wstring_of_wsize(eager_wsize_max_stack) );
  print( " -eH<size>    the maximal eager heap usage.  (%4s)\n", Wstring_of_wsize(eager_wsize_max_heap) );
  print( "\n" );
#endif
  print( "values:\n" );

  print( " <size>       number with an optional scale and optional unit.\n" );
  print( "              available scales are kilo (k), mega (m) or giga (g).\n" );
  print( "              available units are machine words (w) or, by default, bytes (b).\n" );
  print( "              example: lvmrun -H64m -s4kw -S512kb <file>\n" );
  print( " <percent>    number between 0 and 100 followed by an optional '%%' character.\n" );
  print( "              example: lvmrun -hF88%% <file>\n" );
  print( " <path>       a list of directories seperated by ';' (or ':' on unix systems).\n" );
  print( "              start with a ';' (or ':') to insert the current path in front.\n" );
  print( "              example: lvmrun -P;/usr/lib/lvm <file>\n" );
  print( "\n" );
  print( "environment variables:\n" );
  print( " LVMPATH      the search path for lvm files (same as -P option)\n" );
  env = getenv( "LVMPATH" );
  print( "              current: \"%s\"\n", env != NULL ? env : "" );
  print( " LVMOPTIONS   default options for the lvm.\n" );
  env = getenv( "LVMOPTIONS" );
  print( "              current: \"%s\"\n", env != NULL ? env : "" );
  print( "\n" );
  print( "notes:\n" );
  print( " options are read from LVMPATH, LVMOPTIONS and finally the command line.\n" );
}


/* read command line arguments:
   lvmrun [lvm options] lvmfile [program options]
*/
void Noreturn FUN_VAR_ARGS1(options_fatal_error, const char *, fmt, args)
{
  error_message( "error in options: " );
  error_vmessage( fmt, args );
  error_message( "\n" );
  show_options();
  sys_exit(1);
}
END_ARGS(args)


static void options_out_of_memory(void)
{
  options_fatal_error( "out of memory while parsing options" );
}

static void parse_path( const char* path )
{
  int len = 0;
  if (path == NULL) return;

  if ((path[0] == ';' || path[0] == ':') && lvmpath != NULL)
  {
    len = strlen(lvmpath) + strlen(path);
    lvmpath = (const char*)realloc( (char*)lvmpath, len+1 );
    if (lvmpath == NULL) options_out_of_memory();
    strcat( (char*)lvmpath, path );
  }
  else {
    len = strlen(path);
    if (lvmpath != NULL) free((char*)lvmpath);
    lvmpath = (const char*)malloc( len+1 );
    if (lvmpath == NULL) options_out_of_memory();
    strcpy( (char*)lvmpath, path );
  }
}

static void parse_percent( const char* s, unsigned long* var )
{
  int res;
  if (s[0] == '=') res = sscanf( s, "=%lu", var );
              else res = sscanf( s, "%lu", var );
  if (res != 1)
    options_fatal_error( "illegal percentage format (%s)", (s[0] == 0 ? "<empty>" : s) );

  if (*var < 0)   *var = 0;
  if (*var > 100) *var = 100;
}

static void parse_size( const char* what
                      , const char* s, unsigned long min, unsigned long max
                      , unsigned long* var
                      , bool words )
{
#define assign(x,y) { nat _check = (y); if (_check < (x)) goto err_overflow; (x) = _check; }

  nat res;
  const char* p;

  /* skip leading '=' */
  p = s;
  if (*p == '=') p++;

  /* read number */
  res = 0;
  while (isdigit((int)(*p))) {
    assign(res,res*10 + (*p - '0'));
    p++;
  }
  if (res == 0) goto err_format;

  /* read scales */
  switch (*p) {
  case 'K':
  case 'k': assign(res,res*Kilo); p++; break;
  case 'M':
  case 'm': assign(res,res*Mega); p++; break;
  case 'G':
  case 'g': assign(res,res*Giga); p++; break;
  }

  /* read units */
  switch (*p) {
  case 'w': assign(res,res * sizeof(value)); p++; break;
  case 'b': p++; break;
  }

  if (*p != '\0') goto err_format;

  /* last adjustments */
  if (words) res = res / sizeof(value);

  /* check bounds */
  if (res < min)
    options_fatal_error( "%s must be at least %s", what, words ? Bstring_of_wsize(min) : Bstring_of_bsize(min) );

  if (res > max)
    options_fatal_error( "%s must be less than %s", what, words ? Bstring_of_wsize(max) : Bstring_of_bsize(max) );

  *var = res;
  return;

err_overflow:
  options_fatal_error( "%s overflow (%s)", what, s );
  return;

err_format:
  options_fatal_error( "illegal %s format (%s)", what, (s[0] == 0 ? "<empty>" : s) );
  return;
}


static void parse_option( const char* option )
{
  switch (option[0])
  {
  case 's': parse_size( "initial stack size", option+1, stack_wsize_threshold*2, Max_long, &stack_wsize_init, true );
            break;
  case 'S': parse_size( "maximal stack size", option+1, stack_wsize_threshold*2, Max_long, &stack_wsize_max, true );
            break;
  case 'P': parse_path( option+1 );
            break;
  case '?': show_options();
            exit(0);  /* not shut_down, heap and signals not initalised */
            break;
  case 't': timings_report = true;
            break;
  case 'H': parse_size( "maximal heap size", option+1, Heap_chunk_min_wsize, Heap_chunk_max_wsize
                      , &heap_wsize_max_init, true );
            break;
  case 'h': switch (option[1]) {
            case 'e': parse_size( "heap expansion size", option+2, Heap_chunk_min_wsize, Heap_chunk_max_wsize
                                , &heap_chunk_wsize_init, true );
                      break;
            case 'm': parse_size( "minor generation size", option+2, Minor_heap_min_wsize, Minor_heap_max_wsize
                                , &heap_minor_wsize_init, true );
                      break;
            case 'f': parse_percent( option+2, &heap_percent_free_init );
                      break;
            case 'F': parse_percent( option+2, &heap_max_percent_free_init );
                      break;
            case 'r': heap_report = true;
                      break;
            case 'v': { int level = option[2] - '0';
                      if (level < 0 || level > 3)
                        options_fatal_error( "illegal heap verbosity level (%s)", option );
                      else
                        heap_verbose_init = level;
                      break;
                      }

            default:  if (option[1] >= '0' && option[1] <= '9')
                        parse_size( "initial heap size", option+1, Minor_heap_min_wsize, Minor_heap_max_wsize
                                  , &heap_wsize_init, true );
                      else
                        options_fatal_error( "unrecognised option (%s)", option );
                      break;
            }
            break;
#ifdef LVM_EAGER
  case 'e': switch (option[1]) {
            case 's': parse_size( "minimal eager stack", option+2, 4, Max_long, &eager_wsize_min_stack, true );
                      break;
            case 'S': parse_size( "maximal eager stack", option+2, 4, Max_long, &eager_wsize_max_stack, true );
                      if (Bsize_wsize(eager_wsize_max_stack) > stack_bsize_threshold) stack_bsize_threshold = Bsize_wsize(eager_wsize_max_stack);
                      break;
            case 'H': parse_size( "maximal eager heap", option+2, 4, Max_long, &eager_wsize_max_heap, true );
                      break;
            default : options_fatal_error( "unrecognised option (%s)", option );
                      break;
            }
            break;
#endif
  default:  options_fatal_error( "unrecognised option (%s)", option );
            break;
  }
}


static const char** options_cmd_line( const char** argv )
{
  int i;
  for( i = 1; argv[i] != NULL && argv[i][0] == '-'; i++)
  {
    parse_option( &argv[i][1] );
  }
  return (argv+i);
}

static void options_env(void)
{
  const char* envoptions;
  const char* envpath;

  envpath    = getenv("LVMPATH");
  if (envpath != NULL) {
    parse_path( envpath );
  }
  envoptions = getenv("LVMOPTIONS");
  if (envoptions != NULL) {
    while (*envoptions != 0) {
      while (*envoptions != 0 && *envoptions != '-') envoptions++;
      if (*envoptions == '-') parse_option( envoptions+1 );
    }
  }
}

static void options_check(void)
{
  /* stack */
  if (stack_wsize_init > stack_wsize_max)
    options_fatal_error( "maximal stack size smaller than initial stack size" );

  /* heap */
  if (heap_wsize_init > heap_wsize_max_init)
    options_fatal_error( "maximal heap size smaller than initial heap size" );
  if (heap_chunk_wsize_init > heap_wsize_init)
    options_fatal_error( "initial heap smaller than heap expansion size" );

#ifdef LVM_EAGER
  if (eager_wsize_min_stack > eager_wsize_max_stack)
    options_fatal_error( "maximal eager stack smaller than minimal eager stack" );
  if (eager_wsize_max_heap > heap_minor_wsize_init)
    options_fatal_error( "maximal eager heap greater than minor heap size" );
#endif
}


/* Atom table */
header_t atom_table[256];

static void init_atoms(void)
{
  unsigned i;
  for(i = 0; i < 256; i++) atom_table[i] = Make_header(0, i, Caml_white);
}

void start_module( const char* name )
{
  CAMLparam0();
  CAMLlocal1(module);

  module = load_module( name );
  debug_gc();

  stat_end_init();
  evaluate_name( module, "main" );
  CAMLreturn0;
}

#if defined(FIXUP_OFFSET)
char* fixup_base;
#endif

int main( int argc, const char** argv )
{
  const char** args;
  nat          gc_verbose_init = 0;
#ifdef DEBUG
  /* debug_level       = 2; */
#endif

#if defined(FIXUP_OFFSET)
  fixup_base = (char*)malloc(sizeof(long));
  if (fixup_base) free(fixup_base);
#endif

  stat_start_init();
  options_env();
  args = options_cmd_line( argv );
  options_check();

  init_atoms();
  init_custom_operations();

  switch (heap_verbose_init) {
  case 0: gc_verbose_init = 0; break;
  case 1: gc_verbose_init = 1; break;
  case 2: gc_verbose_init = 15; break;
  case 3:
  default:gc_verbose_init = -1; break;
  }
  init_gc (heap_minor_wsize_init, heap_wsize_init, heap_chunk_wsize_init
          ,heap_percent_free_init, heap_max_percent_free_init
          ,gc_verbose_init, heap_wsize_max_init );
  init_signals();
  init_evaluator();
  debug_gc();

  if (args[0] == NULL) {
    show_options();
  }
  else {
    start_module( args[0] );
  }

  stat_start_done();
  done_signals();
  done_gc();
  stat_end_done();
  if (args[0] && timings_report) stat_timings_report();
  if (args[0] && heap_report)    stat_heap_report();
  sys_exit(0);
  return 0;
}
