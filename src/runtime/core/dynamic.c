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
#include "config.h"       /* OS_WINDOWS, HAS_DLFCN_H, HAS_DL_H */
#include "fail.h"
#include "module.h"
#include "dynamic.h"

/* platform specific routines only return a status code. */
enum load_status
{
  Load_success,
  Load_nodynamic,         /* no support for dynamic loading */
  Load_no_ordinal,        /* no support for dynamic loading by ordinal */
  Load_nolibrary,         /* can't load library */
  Load_nolibrary_info,    /* can't load library and I returned extra error information */
  Load_nosymbol,          /* couldn't find symbol */
  Load_nocallconv         /* this platform doesn't support this calling convention */
};


/* prototype for the platform specific library loading */
static enum load_status sys_load_lib_symbol( char* lib_name_buf, char* name_buf
                                    , void** symbol, const char** error_info
                                    , enum call_conv cconv );

static enum load_status sys_load_lib_ordinal( char* lib_name_buf, int ordinal
                                      , void** symbol, const char** error_info
                                      , enum call_conv cconv );


/*-----------------------------------------------------------------------
  decorate a symbol according to its calling convention:
  - ccall   : prefix with "_"
  - stdcall : prefix with "_", postfix with "@i" where i is the byte size
              of its arguments in decimal.
-----------------------------------------------------------------------*/
#define Symbol_extra      16

int type_size( const char* type );
int arg_size ( char rep );

static void decorate_symbol( char* name_buf, const char* name, wsize_t size
                           , enum call_conv cconv, const char* type )
{
   Assert( name_buf );
   Assert( name );
   if (name != NULL && strlen(name) + Symbol_extra >= size)
      raise_internal( "loader: symbol name too long: \"%s\"", name );

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
int type_size( const char* type )
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
int  arg_size( char rep )
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
      raise_internal( "loader: unknown type represenation: \"%c\"", rep );
      return 0;
    }
  }
}



/*-----------------------------------------------------------------------
 load_lib_symbol
-----------------------------------------------------------------------*/
#define Library_extra     128

void* load_dynamic_symbol( const char* lib_name,
                           const char* name,
                           enum call_conv cconv,
                           const char* type,
                           enum  name_flag flag )
{
    void* symbol         = NULL;
    const char* error_info     = NULL;
    enum load_status status = Load_success;
    char  lib_name_buf[Max_lib_name + Library_extra];
    char  name_buffer[Max_name + Symbol_extra];
    char* name_buf = name_buffer;

    Assert( lib_name );

    /* Copy the library name to a local buffer so that the system specific
      functions can attach/prepend NAME_EXTRA stuff to it, like ".dll" */
    if (lib_name && strlen(lib_name) >= Max_lib_name)
      raise_internal( "loader: library name too long: \"%s\"", lib_name );

    lib_name_buf[0] = '\0';
    if (lib_name) strcpy(lib_name_buf,lib_name);

    /* Copy the name so we can decorate it */
    name_buf[0] = '\0';
    if (flag == Name_decorate) {
      decorate_symbol( name_buf, name, Max_name + Symbol_extra, cconv, type );
    } else if (flag == Name_ordinal) {
      /* ok */
    } else {
      if (name != NULL && strlen(name) >= Max_name)
        raise_internal( "loader: symbol name too long: \"%s\"", name );
      strncpy( name_buf, name, Max_name);
    }

    /* Call a system specific routine for loading the symbol. */
    if (flag == Name_ordinal)
      status = sys_load_lib_ordinal( lib_name_buf, (long)name, &symbol, &error_info, cconv );
    else
      status = sys_load_lib_symbol( lib_name_buf, name_buf, &symbol, &error_info, cconv );

    switch (status)
    {
    case Load_success:
      return symbol;

    case Load_nolibrary:
      raise_internal( "loader: can not open library \"%s\"", lib_name_buf );
      break;
    case Load_nolibrary_info:
      raise_internal( "loader: can not open library \"%s\": %s", lib_name_buf, error_info);
      break;
    case Load_nosymbol:
      raise_internal( "loader: library \"%s\" doesn't export symbol \"%s\"", lib_name_buf, name_buf );
      break;
    case Load_nocallconv: {
      char* call_conv_name;
      switch (cconv)
      {
      case Call_c   : call_conv_name = "ccall";   break;
      case Call_std : call_conv_name = "stdcall"; break;
      default       : call_conv_name = "<unknown>";
      }

      raise_internal( "loader: error while loading library \"%s\", symbol \"%s\": "
                      "this platform doesn't support: \"%s\""
                      , lib_name_buf, name_buf, call_conv_name );
      break;
      }
    case Load_nodynamic:
      raise_internal( "loader: error while loading library \"%s\", symbol \"%s\", "
                  "this system doesn't support dynamic linking."
                  , lib_name_buf, name_buf );
      break;
    case Load_no_ordinal:
      raise_internal( "loader: error while loading library \"%s\", ordinal %i, "
                      "this system doesn't support linking by ordinal."
                      , lib_name_buf, name );
      break;

    default:
      raise_internal( "loader: unknown error while loading library \"%s\", symbol \"%s\"", lib_name_buf, name_buf );
      break;
    }

    return NULL;
}

/*-----------------------------------------------------------------------
 Platform specific routines.
-----------------------------------------------------------------------*/

#if defined(OS_WINDOWS)

#include <windows.h>
enum load_status sys_load_lib_ordinal( char* lib_name_buf, int ordinal
                                      , void** symbol, const char** error_info
                                      , enum call_conv cconv )
{
  return sys_load_lib_symbol( lib_name_buf, (char*)ordinal, symbol, error_info, cconv );
}

enum load_status sys_load_lib_symbol( char* lib_name_buf, char* name_buf
                                    , void** symbol, const char** error_info
                                    , enum call_conv cconv )
{
    HINSTANCE  lib;
    strcat(lib_name_buf,".dll");

    if (cconv > Call_std) return Load_nocallconv;

    lib = LoadLibrary(lib_name_buf);
    if (lib == NULL) return Load_nolibrary; /* GetLastError doesn't tell anything more in practice. */

    *symbol = GetProcAddress(lib,name_buf);
    if (*symbol == NULL) return Load_nosymbol;

    return Load_success;
}

#elif defined(HAS_DLFCN_H)
/* eg CYGWIN, LINUX, SOLARIS, ULTRIX */

#include <stdio.h>
#include <dlfcn.h>

enum load_status sys_load_lib_ordinal( char* lib_name_buf, int ordinal
                                      , void** symbol, const char** error_info
                                      , enum call_conv cconv )
{
  return Load_no_ordinal;
}

enum load_status sys_load_lib_symbol( char* lib_name_buf, char* name_buf
                                    , void** symbol, const char** error_info
                                    , enum call_conv cconv )
{
    void* lib = NULL;

#ifdef OS_CYGWIN
    if (cconv > Call_std) return Load_nocallconv;
    strcat( lib_name_buf, ".dll" );
#else
    if (cconv > Call_c) return Load_nocallconv;
    strcat( lib_name_buf, ".so" );
#endif


#ifdef RTLD_NOW
    lib = dlopen(lib_name_buf,RTLD_NOW);
#elif defined RTLD_LAZY /* eg SunOS4 doesn't have RTLD_NOW */
    lib = dlopen(lib_name_buf,RTLD_LAZY);
#else /* eg FreeBSD doesn't have RTLD_LAZY */
    lib = dlopen(lib_name_buf,1);
#endif

    if (lib == NULL) {
      if (error_info) {
           *error_info = dlerror();
           return Load_nolibrary_info;
      }
      else return Load_nolibrary;
    }

    *symbol = dlsym(lib,name_buf);
    if (*symbol == NULL) return Load_nosymbol;

    return Load_success;
}


#elif HAS_DL_H /* eg HPUX */

#include <dl.h>

enum load_status sys_load_lib_ordinal( char* lib_name_buf, int ordinal
                                      , void** symbol, const char** error_info
                                      , enum call_conv cconv )
{
  return Load_no_ordinal;
}

enum load_status sys_load_lib_symbol( char* lib_name_buf, char* name_buf
                                    , void** symbol, const char** error_info
                                    , enum call_conv cconv )
{
    shl_t lib;

    if (cconv > Call_c) return Load_nocallconv;

    lib = shl_load(lib_name_buf,BIND_IMMEDIATE,0L);
    if (lib == NULL) return Load_nolibrary;

    if (0 == shl_findsym(&lib,name_buf,TYPE_PROCEDURE,symbol))
      return Load_success;
    else
      return Load_nosymbol;
}


#else /* Dynamic loading not available */

enum load_status sys_load_lib_ordinal( char* lib_name_buf, int ordinal
                                      , void** symbol, const char** error_info
                                      , enum call_conv cconv )
{
  return Load_no_ordinal;
}

enum load_status sys_load_lib_symbol( char* lib_name_buf, char* name_buf
                                    , void** symbol, const char** error_info
                                    , enum call_conv cconv )
{
    return Load_nodynamic;
}


#endif
