/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id: */

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "mlvalues.h"
#include "memory.h"
#include "heap/heap.h"
#include "dynamic.h"
#include "custom.h"

#include "fail.h"
#include "print.h"
#include "module.h"
#include "loader.h"
#include "evaluator.h"
#include "schedule.h"
#include "stats.h"
#include "sys.h"  /* searchpath */

#ifdef OS_WINDOWS
# include <windows.h>
#endif

/* Version string */
static const char* version(void)
{
  const char* _version = "$Revision$";
  static char version_buf[40];

  strncpy( version_buf, _version + 2 + strlen("Revision"), 100 );
  version_buf[strlen(version_buf)-1] = 0;
  return version_buf;
}


/* initial heap parameters */
/* Initial speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   heap size.  The rest of the heap is live objects. */
static nat heap_percent_free_init     = 42;
static nat heap_max_percent_free_init = 100; /* Initial setting for the compacter: off */

static wsize_t heap_minor_wsize_init      = 32*Kilo; /* Initial size of the minor zone. (words)  */
static wsize_t heap_chunk_wsize_init      = 64*Kilo; /* Initial size increment when growing the heap. Must be a multiple of [Page_size / sizeof (value)]. */

static wsize_t heap_wsize_init            = 64*Kilo; /* Default initial size of the major heap (words); same constraints as for Heap_chunk_def. */
static wsize_t heap_wsize_max_init        = Wsize_bsize(64*Mega); /* Default maximum size of the heap */

#ifdef DEBUG
static nat heap_verbose_init          = 3;
#else
static nat heap_verbose_init          = 0;
#endif

static bool heap_report               = false;
static bool timings_report            = false;
static bool options_report            = false;

/* global options */
wsize_t stack_wsize_init       = 4*Kilo;
wsize_t stack_wsize_max        = Wsize_bsize(64*Mega);
wsize_t stack_wsize_threshold  = Kilo;

wsize_t stack_wsize_total = 0;
wsize_t stack_wsize_peak  = 0;

/* search paths */
const char* lvmpath = NULL; /* malloc'd */
const char* dllpath = NULL; /* malloc'd */
const char* argv0   = NULL;


/*----------------------------------------------------------------------
-- show options
----------------------------------------------------------------------*/
void show_options(void)
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
  print( "values:\n" );

  print( " <size>       number with an optional scale and optional unit.\n" );
  print( "              available scales are kilo (k), mega (m) or giga (g).\n" );
  print( "              available units are machine words (w) or, by default, bytes (b).\n" );
  print( "              example: lvmrun -H64m -s4kw -S512kb <file>\n" );
  print( " <percent>    number between 0 and 100 followed by an optional '%%' character.\n" );
  print( "              example: lvmrun -hF88%% <file>\n" );
  print( " <path>       a list of directories seperated by ';' (or ':' on unix systems).\n" );
  print( "              use $<name> or %%<name> to insert an environment variable.\n" );
  print( "              the current path value is available as $current.\n" );
  print( "              the lvm installation path is available as $lvmdir.\n" );
  print( "              example: lvmrun -P%%current:/usr/lib/lvm <file>\n" );
  print( "\n" );
  print( "environment variables:\n" );

  print( " LVMPATH      the search path for lvm files (same as -P option).\n" );
  env = getenv( "LVMPATH" );
  print( "              current : \"%s\"\n", env != NULL ? env : "" );
  
  print( " LVMOPTIONS   default options for the lvm.\n" );
  env = getenv( "LVMOPTIONS" );
  print( "              current: \"%s\"\n", env != NULL ? env : "" );
  
  print( " LVMDLLPATH   extra search path for shared (dynamic link) libraries (%s).\n", DLL );
  env = getenv( "LVMDLLPATH" );
  print( "              current: \"%s\"\n", env != NULL ? env : "" );

  print( "\n" );
  print( "notes:\n" );
  print( " options are read from LVMPATH, LVMOPTIONS and finally the command line.\n" );

  print( "\n" );
  print( "search paths:\n" );
  print( " lvm modules  \"%s\"\n", lvmpath );
  if (dllpath == NULL) {
  print( " shared libs  <system path>\n" );
  } else {
  print( " shared libs  \"%s%c<system path>\"\n", dllpath, PATHSEP );
  print( "\n" );
  }
}


/*----------------------------------------------------------------------
-- option errors: can not use [shutdown] since it calls
-- [done_options] which shouldn't be called in an uninitialized state.
----------------------------------------------------------------------*/
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


/*----------------------------------------------------------------------
-- (environment) variables.
--
-- [variable] takes a string that points to an (environment) variable,
-- the value of the [$current] variable. It returns a statically allocated
-- string with the value of the variable. It also returns the length [varlen]
-- of the variable name in the original string. For example, the length 
-- [$lvmdir] is 7. Returns NULL (and length 0) when the variable has no value.
-- [varlen] and [current] can be NULL.
----------------------------------------------------------------------*/
static bool is_variable( const char c )
{
  return (c=='$' || c == '%');
}

static const char* variable( const char* str, const char* current, long* varlen )
{
  static char var[MAXVAR];
  char        name[MAXNAME];
  const char* p;
  long        len;

  if (varlen)    *varlen = 0;
  if (str==NULL) return NULL;
  

  /* determine the length */
  p = str;
  if (is_variable(p[0]) && p[1] == '(') {
    /* parenthesized */
    p += 2;
    for( len = 0; p[len] != 0 && p[len] != ')'; len++ ) { /* nothing */ }
    if (varlen) *varlen = len+2;    
  } 
  else if (is_variable(p[0]) && p[1] == p[0]) {
    /* escaped */
    p  += 1;
    len = 1;
    if (varlen) *varlen = 2;    
  }
  else if (is_variable(p[0])) {
    /* normal */
    p += 1;
    for( len = 0; isalnum(p[len]); len++ ) { /* nothing */ }
    if (varlen) *varlen = len+1;    
  }
  else {
    /* no variable start character? */
    for( len = 0; isalnum(p[len]); len++ ) { /* nothing */ }
    if (varlen) *varlen = len;    
  }

  /* find the name */
  if (len == 0 || len >= MAXNAME) return NULL;
  str_cpy(name,p,len+1);

  if (len == 1 && is_variable(name[0])) {
    /* escaped */
    str_cpy(var,name,MAXVAR);
    return var;
  }
  else if (stricmp("CURRENT",name) == 0) {
    /* current string value */
    return current;
  } 
#ifdef OS_WINDOWS
  else if (stricmp("SYSTEMDIR",name) == 0) {
    if (GetSystemDirectory(var,MAXVAR) == 0) return NULL;
    return var;
  }
  else if (stricmp("WINDOWSDIR",name) == 0) {
    if (GetWindowsDirectory(var,MAXVAR) == 0) return NULL;
    return var;
  }
#endif
  else if (stricmp("LVMDIR",name) == 0 && getenv(name) == NULL) { /* environment takes precedence */
  #ifdef OS_WINDOWS
    /* use a standard system call */
    if (GetModuleFileName(NULL,var,MAXVAR) == 0) return NULL;
  #else
    /* find the path thru the command typed by the user: [argv0] */
    const char* path = searchpath( NULL /* system default path */, argv0, EXE );
    if (path == NULL) return NULL;
    str_cpy(var,path,MAXVAR);
  #endif
    /* remove file part */
    len = str_len(var);
    while (len > 0 && !is_filesep(var[len])) len--;
    var[len] = 0;
    return var;
  }
  else {
    return getenv(name);
  }    
}


/*----------------------------------------------------------------------
-- expand variables in a string
----------------------------------------------------------------------*/
static char* expand_string( const char* current, const char* str )
{
  const char* p;
  char* newstr;
  long  len,size;

  /* allocate a zero string on a null [str] */
  if (str == NULL) {
    newstr = (char*)malloc(1);
    if (newstr==NULL) return NULL;
    newstr[0] = 0;
    return newstr;
  }
  
  /* determine the total length needed */
  len = 0;
  p   = str;
  while (*p != 0) {
    if (is_variable(*p)) {
      long        varlen;
      const char* var = variable(p,current,&varlen);
      p   += varlen;
      len += str_len(var);
    } else {
      p   += 1;
      len += 1;
    }
  }

  /* allocate memory */
  size    = len+1;
  newstr = (char*)malloc(size);
  if (newstr == NULL) return NULL;
  
  /* and copy */  
  len = 0;
  p   = str;
  while (*p != 0 && len < size) {
    if (is_variable(*p)) {
      long        varlen;
      const char* var = variable(p,current,&varlen);
      newstr[len] = 0;
      str_cat(newstr,var,size);      
      p   += varlen;
      len += str_len(var);
    }
    else {
      newstr[len] = *p;
      p   += 1;
      len += 1;
    }
  }

  newstr[len] = 0;
  return newstr;
}


/*----------------------------------------------------------------------
-- parse option values: <path>, <percent> and <size>
----------------------------------------------------------------------*/
static void parse_malloc_path( const char** path, const char* newpath )
{
  char* p;
  Assert(path);
  p = expand_string( *path, newpath );
  if (p == NULL)     { options_out_of_memory(); } 
  normalize_path(p);
  if (*path != NULL) { free((char*)*path); }
  *path = p;
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
                      , const char* s, wsize_t min, wsize_t max
                      , wsize_t* var
                      , bool words )
{
#define assign(x,y) { wsize_t _check = (y); if (_check < (x)) goto err_overflow; (x) = _check; }

  wsize_t res;
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


/*----------------------------------------------------------------------
-- parse options from the commandline or the environment
----------------------------------------------------------------------*/
static void parse_option( const char* option )
{
  switch (option[0])
  {
  case 's': parse_size( "initial stack size", option+1, stack_wsize_threshold*2, Max_long, &stack_wsize_init, true );
            break;
  case 'S': parse_size( "maximal stack size", option+1, stack_wsize_threshold*2, Max_long, &stack_wsize_max, true );
            break;
  case 'P': parse_malloc_path( &lvmpath, option+1 );
            break;
  case '?': options_report = true;
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

  envpath = getenv("LVMPATH");
  if (envpath != NULL) { parse_malloc_path( &lvmpath, envpath ); }

  envpath = getenv("LVMDLLPATH");
  if (envpath != NULL) { parse_malloc_path( &dllpath, envpath ); }  

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
}

/*----------------------------------------------------------------------
-- static table of atoms: constructors without fields and a tag lower
-- than 256.
----------------------------------------------------------------------*/
header_t atom_table[256];

static void init_atoms(void)
{
  unsigned i;
  for(i = 0; i < 256; i++) atom_table[i] = Make_header(0, i, Caml_white);
}

/*----------------------------------------------------------------------
-- init/done options: initializes and shutsdown the entire lvm runtime.
----------------------------------------------------------------------*/
#if defined(FIXUP_OFFSET)
char* fixup_base;
#endif

const char** init_options( const char** argv )
{
  const char** args;
  nat          gc_verbose_init = 0;

  stat_start_total();
  stat_start_init();

#if defined(FIXUP_OFFSET)
  fixup_base = (char*)malloc(sizeof(long));
  if (fixup_base) free(fixup_base);
#endif
  
  argv0 = argv[0];
  options_env();
  args = options_cmd_line( argv );
  /* parse_malloc_path( &lvmpath, ".;$current" ); */              /* always search current directory first */
  options_check();
  if (options_report) { 
    show_options(); 
    sys_exit(0); 
  }
  
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
  init_dynamic();
  init_evaluator();
  
  if (args[0] == NULL) timings_report = heap_report = false;
  stat_end_init();
  return args;
}

void done_options(bool showreports)
{
  stat_start_done();
  done_signals();
  done_gc();
  done_dynamic(); /* must be after [done_gc] */
  if (lvmpath) free((char*)lvmpath);
  if (dllpath) free((char*)dllpath);
  stat_end_done();
  stat_end_total();

  if (showreports) {
    if (timings_report) stat_timings_report();
    if (heap_report)    stat_heap_report();
  }
}