/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

/*----------------------------------------------------------------------
  Platform independent loading of dynamic link libraries (.so or .dll files).
----------------------------------------------------------------------*/
#include <stdio.h>
#include <string.h>

#include "mlvalues.h"
#include "memory.h"
#include "fail.h"
#include "module.h"
#include "dynamic.h"
#include "sys.h"
#include "custom.h"

char* stat_alloc_string( const char* s )
{
  long  len;
  char* p;

  len = str_len(s);
  p   = (char*)stat_alloc( len+1 );
  str_copy( p, s, len+1 );
  return p;
}

/*----------------------------------------------------------------------
  Platform dependent includes + definition of a dynamic library handle
----------------------------------------------------------------------*/
#if defined(OS_WINDOWS)    /* eg. MINGW32, WINDOWS */
# include <windows.h>
  typedef HMODULE dynamic_handle;
#elif defined(HAS_DLFCN_H) /* eg CYGWIN, LINUX, SOLARIS, ULTRIX */
# include <dlfcn.h>
  typedef void* dynamic_handle;
#elif HAS_DL_H            /* eg HPUX */
# include <dl.h>
# define dynamic_handle shl_t
#else                     /* no dynamic linking */
  typedef void* dynamic_handle;
#endif

/*----------------------------------------------------------------------
 platform specific routines only return a status code.
----------------------------------------------------------------------*/
enum dynamic_err
{
  Err_success,
  Err_nodynamic,         /* no support for dynamic loading */
  Err_nolibrary,         /* can't load library */
  Err_noordinal,         /* no support for dynamic loading by ordinal */
  Err_nosymbol,          /* couldn't find symbol */
};

/*----------------------------------------------------------------------
  Platform specific routines (not thread-safe!)
----------------------------------------------------------------------*/
static void             sys_dynamic_close  ( dynamic_handle handle );
static enum dynamic_err sys_dynamic_open   ( const char* libpath, dynamic_handle* handle, const char** errinfo );
static enum dynamic_err sys_dynamic_symbol ( dynamic_handle* handle, const char* name, void** fun, const char** errinfo );
static enum dynamic_err sys_dynamic_ordinal( dynamic_handle* handle, long ordinal, void** fun, const char** errinfo );

/*----------------------------------------------------------------------
  We maintain a list of dynamic libraries that are loaded. These
  libraries are reference counted.
----------------------------------------------------------------------*/
struct dynamic_lib {
  struct dynamic_lib* next;
  long                refcount;
  char*               name;
  char*               path;
  dynamic_handle      handle;
};

static struct dynamic_lib* dynamic_libs = NULL;

static struct dynamic_lib* dynamic_open( const char* libname );

static void   dynamic_release( struct dynamic_lib* lib );
static void   dynamic_close  ( struct dynamic_lib* lib );
static value  dynamic_symbol ( struct dynamic_lib* lib, const char* name, bool is_ordinal );
static void   dynamic_decorate( char* name_buf, const char* name, long size
                              , enum call_conv cconv, const char* type );


/*----------------------------------------------------------------------
  [load_dynamic_symbol]
  the toplevel routine to load symbols. All resource management and
  decoration is done automatically. Returns a custom Symbol block.
----------------------------------------------------------------------*/
value load_dynamic_symbol( const char*    libname,
                           const char*    name,
                           enum call_conv cconv,
                           const char*    type,
                           enum name_flag flag )
{
    CAMLparam0();
    CAMLlocal1(v);
    struct dynamic_lib* lib;

    lib = dynamic_open( libname );

    switch (flag) {
    case Name_ordinal:
        v = dynamic_symbol( lib, name, true );
        break;
    case Name_decorate: {
        char decorated[MAXPATH];
        dynamic_decorate( decorated, name, MAXPATH, cconv, type );
        v = dynamic_symbol( lib, decorated, false );
        break;
        }
    case Name_plain:
    default:
        v = dynamic_symbol( lib, name, false );
        break;
    }

    CAMLreturn(v);
}


/*----------------------------------------------------------------------
  Symbols in a dll are Custom blocks that hold both a
  pointer to the symbol and a pointer to the [dynamic_lib] struct.
----------------------------------------------------------------------*/
static void symbol_finalize( value v )
{
  gc_message( 8,"finalise dynamic symbol\n", 0 );
  dynamic_release(Symbol_lib(v));
  return;
}

static int symbol_compare(value v1, value v2)
{
  raise_internal( "comparing dynamic symbol" );
  return 0;
}

static long symbol_hash(value v)
{
  return (long)(Symbol_fun(v));
}

static void symbol_serialize(value v, unsigned long * wsize_32,
                            unsigned long * wsize_64)
{
  raise_internal( "serializing dynamic symbol" );
}

static unsigned long symbol_deserialize(void * dst)
{
  raise_internal( "deserializing dynamic symbol" );
  return 0;
}


static struct custom_operations symbol_ops = {
  "_dynamic_symbol",
  symbol_finalize,
  symbol_compare,
  symbol_hash,
  symbol_serialize,
  symbol_deserialize
};

static void dynamic_raise_symbol( enum dynamic_err err, const char* info
                                , const char* libname, const char* name )
{
  switch (err) {
    case Err_success:
      return;
    case Err_noordinal:
      if (info == NULL) info = "this platform doesn't support linking by ordinal";
      break;
    case Err_nodynamic:
      if (info == NULL) info = "this platform doesn't support dynamic linking";
      break;
    case Err_nosymbol:
    default:
      break;
  }

  if (info) raise_internal( "can not resolve symbol \"%s\" in \"%s\": %s", name, libname, info);
       else raise_internal( "can not resolve symbol \"%s\" in \"%s\"", name, libname );
}

static value dynamic_symbol( struct dynamic_lib* lib, const char* name, bool is_ordinal )
{
  CAMLparam0();
  CAMLlocal1(v);
  enum dynamic_err err;
  const char*      errinfo;
  void*            fun;

  Assert(lib);
  if (is_ordinal) err = sys_dynamic_ordinal( &(lib->handle), (long)name, &fun, &errinfo );
             else err = sys_dynamic_symbol( &(lib->handle), name, &fun, &errinfo );
  if (err != Err_success) dynamic_raise_symbol( err, errinfo, lib->name, name );

  v = alloc_custom( &symbol_ops, 2*sizeof(void*), 0, 1 );
  Symbol_lib(v) = lib;
  Symbol_fun(v) = fun;
  CAMLreturn(v);
};


/*----------------------------------------------------------------------
  [dynamic_lib] init/done
----------------------------------------------------------------------*/
void init_dynamic(void)
{
  dynamic_libs = NULL;
}

void done_dynamic(void)
{
  Assert(dynamic_libs == NULL);
  while (dynamic_libs != NULL) {
    dynamic_close(dynamic_libs);
  }
}

/*----------------------------------------------------------------------
  [dynamic_close], [dynamic_release] and [dynamic_open]
----------------------------------------------------------------------*/
static void dynamic_release( struct dynamic_lib* lib )
{
  if (lib == NULL) return;

  /* decrease the reference count */
  lib->refcount--;
  if (lib->refcount <= 0) dynamic_close(lib);
};

static void dynamic_close( struct dynamic_lib* lib )
{
  struct dynamic_lib *prev, *current;
  Assert( lib != NULL && lib->refcount == 0);
  if (lib == NULL) return;

  /* find the previous block and relink */
  for( prev = NULL, current = dynamic_libs;
       current != NULL && current != lib;
       prev = current, current = current->next) {}

  if (current == lib) { /* if found */
    if (prev == NULL) dynamic_libs = lib->next;
                 else prev->next   = lib->next;
  }

  gc_message( 8,"finalise dynamic library \"%s\"\n", (long)(lib->path) );

  /* free the block */
  sys_dynamic_close(lib->handle);
  stat_free(lib->name);
  stat_free(lib->path);
  stat_free(lib);
};

static void dynamic_open_error( enum dynamic_err err, const char* info, const char* libpath )
{
  switch (err) {
    case Err_success:
      return;
    case Err_nodynamic:
      if (info == NULL) info = "this system doesn't support dynamic linking";
      /* fall through */
    default:
      if (info) raise_internal( "can not open library \"%s\": %s", libpath, info);
           else raise_internal( "can not open library \"%s\"", libpath );
      break;
  }
}

static struct dynamic_lib* dynamic_open( const char* libname )
{
  const char*         libpath;
  struct dynamic_lib* lib;
  dynamic_handle      handle;
  enum dynamic_err    err;
  const char*         errinfo;

  /* first try to find the library by name */
  for( lib = dynamic_libs; lib != NULL; lib = lib->next ) {
    if (strcmp(libname,lib->name) == 0) {
      /* found! */
      lib->refcount++;
      return lib;
    }
  }

  /* try to find the library and search again by path */
  libpath = searchpath_dll( libname );
  if (libpath == NULL) {
    libpath = libname;
  } else {
    for( lib = dynamic_libs; lib != NULL; lib = lib->next ) {
      if (strcmp(libpath,lib->path) == 0) {
        /* found! */
        lib->refcount++;
        return lib;
      }
    }
  }

  /* we load the library for the first time */
  gc_message( 8,"load dynamic library \"%s\"\n", (long)(libpath) );
  err = sys_dynamic_open( libpath, &handle, &errinfo );
  if (err != Err_success) dynamic_open_error( err, errinfo, libpath );

  lib           = stat_alloc( sizeof(struct dynamic_lib) );
  lib->refcount = 1;
  lib->name     = stat_alloc_string(libname);
  lib->path     = stat_alloc_string(libpath);
  lib->handle   = handle;
  lib->next     = dynamic_libs;
  dynamic_libs  = lib;

  return lib;
}

/*-----------------------------------------------------------------------
  decorate a symbol according to its calling convention:
  - ccall   : prefix with "_"
  - stdcall : prefix with "_", postfix with "@i" where i is the byte size
              of its arguments in decimal.
-----------------------------------------------------------------------*/
#define Symbol_extra      16

static int type_size( const char* type );
static int arg_size ( char rep );

static void dynamic_decorate( char* name_buf, const char* name, long size
                            , enum call_conv cconv, const char* type )
{
   Assert( name_buf );
   Assert( name );
   if (str_len(name) + Symbol_extra >= size)
      raise_internal( "symbol name too long: \"%s\"", name );

   switch (cconv)
   {
   case Call_std: {
       sprintf( name_buf, "_%s@%i", name, type_size(type) );
       break;
     }
   case Call_c: {
       sprintf( name_buf, "_%s", name );
       break;
     }
   default: {
     /* unknown call type, don't do anything */
       strcpy( name_buf, name );
       break;
     }
   }
}

/* return the size of a list of arguments */
static int type_size( const char* type )
{
   int count = type ? strlen(type)-1 : 0;
   int size;
   int i;

   Assert(type);
   Assert(count >= 0);

   for (size = 0, i = 1; i <= count; i++)
   {
      size += arg_size( type[i] );
   }

   return size;
}

/* return the representation size of type. */
static int  arg_size( char rep )
{
  switch (rep)
  {
    case 'b':
    case 'c':
    case 'i':
    case 'u': return (sizeof(int));

    case 'v':
    case 'I':
    case 'U': return (sizeof(long));

    case 'p':
    case 'z':
    case 'Z': return (sizeof(void*));

    case 'f': return (sizeof(float));
    case 'F': return (sizeof(double));

    case '0': return 0;
    case '1': return 4;
    case '2': return 4;
    case '4': return 4;
    case '8': return 8;

    default: {
      raise_internal( "unknown C type represenation: \"%c\"", rep );
      return 0;
    }
  }
}



/*-----------------------------------------------------------------------
 Platform specific routines.
-----------------------------------------------------------------------*/

#if defined(OS_WINDOWS)

#include <windows.h>

static void sys_dynamic_close( dynamic_handle handle )
{
#ifdef DEBUG
  if (FreeLibrary(handle) == 0) Assert(0);
#else
  FreeLibrary(handle);
#endif
}

static enum dynamic_err sys_dynamic_open ( const char* libpath, dynamic_handle* handle, const char** error_info )
{
  if (error_info) *error_info = NULL;

  *handle = LoadLibrary(libpath);
  if (*handle == NULL) return Err_nolibrary; /* GetLastError doesn't tell anything more in practice. */
  return Err_success;
}

static enum dynamic_err sys_dynamic_symbol ( dynamic_handle* handle, const char* name
                                           , void** fun, const char** errinfo )
{
  if (errinfo) *errinfo = NULL;
  *fun = GetProcAddress(*handle,name);
  if (*fun == NULL) return Err_nosymbol;
  return Err_success;
}

static enum dynamic_err sys_dynamic_ordinal( dynamic_handle* handle, long ordinal
                                           , void** fun, const char** errinfo )
{
  if (errinfo) *errinfo = NULL;
  *fun = GetProcAddress(*handle,(LPCSTR)(ordinal));
  if (*fun == NULL) return Err_nosymbol;
  return Err_success;
}




#elif defined(HAS_DLFCN_H)
/* eg CYGWIN, LINUX, SOLARIS, ULTRIX */

#include <stdio.h>
#include <dlfcn.h>

#ifndef RTLD_NOW
# define RTLD_NOW 1
#endif

#ifndef RTLD_GLOBAL
# define RTLD_GLOBAL 0
#endif

static void sys_dynamic_close( dynamic_handle handle )
{
  dlclose(handle);
}

static enum dynamic_err sys_dynamic_open ( const char* libpath, dynamic_handle* handle, const char** error_info )
{
  if (error_info) *error_info = NULL;

  *handle = dlopen( libpath, RTLD_NOW | RTLD_GLOBAL );
  if (*handle == NULL) {
    if (error_info) *error_info = dlerror();
    return Err_nolibrary;
  }
  return Err_success;
}

static enum dynamic_err sys_dynamic_symbol ( dynamic_handle* handle, const char* name
                                           , void** fun, const char** errinfo )
{
  if (errinfo) *errinfo = NULL;
  *fun = dlsym(*handle,name);
  if (*fun == NULL) {
    if (errinfo) *errinfo = dlerror();
    return Err_nosymbol;
  }
  return Err_success;
}

static enum dynamic_err sys_dynamic_ordinal( dynamic_handle* handle, long ordinal
                                           , void** fun, const char** errinfo )
{
  if (errinfo) *errinfo = NULL;
  return Err_noordinal;
}


#elif HAS_DL_H /* eg HPUX */

#include <dl.h>

static void sys_dynamic_close( dynamic_handle handle )
{
  shl_close(handle);
}

static enum dynamic_err sys_dynamic_open ( const char* libpath, dynamic_handle* handle, const char** error_info )
{
  if (error_info) *error_info = NULL;

  *handle = shl_open(libpath,BIND_IMMEDIATE,0L);
  if (*handle == NULL) return Err_nolibrary;

  return Err_success;
}


static enum dynamic_err sys_dynamic_symbol ( dynamic_handle* handle, const char* name
                                           , void** fun, const char** errinfo )
{
  if (errinfo) *errinfo = NULL;
  if (0 == shl_findsym(handle,name,TYPE_PROCEDURE,fun))
   return Err_success;
  else
   return Err_nosymbol;
}

static enum dynamic_err sys_dynamic_ordinal( dynamic_handle* handle, long ordinal
                                           , void** fun, const char** errinfo )
{
  if (errinfo) *errinfo = NULL;
  return Err_noordinal;
}


#else /* Dynamic loading not available */

static void sys_dynamic_close( dynamic_handle handle )
{
}

static enum dynamic_err sys_dynamic_open ( const char* libpath, dynamic_handle* handle, const char** error_info )
{
  return Err_nodynamic;
}


static enum dynamic_err sys_dynamic_symbol ( dynamic_handle* handle, const char* name
                                           , void** fun, const char** errinfo )
{
  return Err_nodynamic;
}

static enum dynamic_err sys_dynamic_ordinal( dynamic_handle* handle, long ordinal
                                           , void** fun, const char** errinfo )
{
  return Err_nodynamic;
}


#endif