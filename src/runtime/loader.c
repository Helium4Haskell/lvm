/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "mlvalues.h"
#include "config.h"
#include "sys.h"
#include "fail.h"
#include "reverse.h"
#include "memory.h"
#include "alloc.h"
#include "bytes.h"
#include "instr.h"
#include "module.h"
#include "dynamic.h"
#include "static.h"
#include "print.h"

#define VERSION_MAJOR 5
#define VERSION_MINOR 0

/*----------------------------------------------------------------------
  alignment & endianess
----------------------------------------------------------------------*/
#define Is_aligned(i) ((i) % sizeof(word_t) == 0)
#define Word_bytes(i) (((i) + sizeof(word_t) - 1) / sizeof(word_t))
#define Word_sizeof(x)  (Word_bytes(sizeof(x)))

#define Reverse_word(w1,w2)  Reverse_32(w1,w2)


static void reverse_endian( word_t* data, nat count )
{
  word_t* p;
  for(p = data; p < data + count; p++)
  {
    Reverse_word(p,p);
  }
}

/*----------------------------------------------------------------------
algorithm:
I)
) read the header
) read the constant section
) read the code section

II)
) resolve the constants:
  ) signatures, names etc. (forward references possible)
  ) read other modules, perform step I & II

III)
) resolve the code
   ) test for circularity on import declarations

----------------------------------------------------------------------*/

static value* load_symbol( value module, const char* modname, long major_version
                         , const char* name, enum const_kind );


/*----------------------------------------------------------------------
   read the header
----------------------------------------------------------------------*/
static int open_module(const char **fname)
{
extern const char* lvmpath;
  const char * truename;
  int fd;

  truename = searchpath(lvmpath,*fname,".lvm");
  if (truename == 0) truename = *fname; else *fname = truename;
  fd = file_open_binary(truename, O_RDONLY );
  if (fd < 0)  raise_module( truename, "could not open file" );

  return fd;
}

static void read_header( const char* name, int handle,
                          struct module_header_t* header, int* is_rev_endian )
{
  static const word_t magic = 0x4C564D58;
  int res;

  *is_rev_endian = 0;

  /* TODO: skip possible extra fields in the header */
  res = file_read( handle, (void*)header, sizeof(struct module_header_t) );
  if (res != sizeof(struct module_header_t))
  {
    file_close(handle);
    raise_module( name, "truncated header" );
  }

  if (magic != header->magic)
  {
    word_t rev_magic;
    Reverse_word(  &rev_magic, &magic );
    if (rev_magic != header->magic) {
      file_close(handle);
      raise_module( name, "invalid magic number" );
    }

    *is_rev_endian = 1;
    reverse_endian( (word_t*)header, Word_sizeof(*header) );
  }

  if (header->major_version != VERSION_MAJOR ||
      header->minor_version > VERSION_MINOR) {
    raise_module( name, "module requires runtime version %i.%02i but this is runtime version %i.%02i.\n"
                        "  please recompile with a compatible compiler"
      , header->major_version, header->minor_version
      , VERSION_MAJOR, VERSION_MINOR );
  }

  return;
}

/*----------------------------------------------------------------------
 read constants
 - read all constants into the constants array. don't resolve ref's yet
----------------------------------------------------------------------*/
#define Word_read(x)          {(x) = *p++; if (is_rev_endian) { Reverse_word(&x,&x); }; }
#define String_read(v,n)      {(v) = alloc_string(n); bcopy(p,String_val(v),n); p += Word_bytes(n); }
#define Store_read(fld)       { word_t x; Word_read(x); Store_field(decl,fld,Val_long(x)); }
#define Store_zero(fld)       { Store_field(decl,fld,Val_long(0)); }
#define Alloc_constant(size)  { decl = alloc(size,kind); Constant(constants,i) = decl; }



#define Const_raise(msg) {stat_free(buffer); file_close(handle); raise_module( fname, msg ); }


static void read_constants( const char* fname, int handle, int is_rev_endian,
                            unsigned constant_len, value constants )
{
  CAMLparam1(constants);
  CAMLlocal1(decl);

  nat   read_count;
  void* buffer;

  nat     i;    /* index in constants */
  word_t* p;    /* pointer in the read buffer */

  /* read this section first into a statically allocated buffer */
  buffer      = stat_alloc( constant_len );
  read_count  = file_read( handle, buffer, constant_len );
  if (read_count != constant_len)  Const_raise( "truncated constant section" );


  /* process all constants in the buffer */
  for(p = buffer, i = 1; i <= Wosize_constants(constants); i++)
  {
    word_t kind;
    word_t len;
    word_t* next;

    Word_read(kind);
    Word_read(len);
    next = p + Word_bytes(len);

    switch(kind)
    {
      case Const_name:
      case Const_type:
      case Const_string:
      case Const_kind:
      { value  str;
        word_t slen;
        Alloc_constant(Const_string_size);
        Word_read(slen);
        String_read(str,slen);
        Store_field( decl, Field_string_string, str );
        break;
      }

      case Const_module: {
        Alloc_constant(Const_module_size);
        Store_read( Field_module_name );
        Store_read( Field_module_major );
        Store_read( Field_module_minor );
        break;
      }

      case Const_value: {
        Alloc_constant(Const_value_size);
        Store_zero( Field_fixup );
        Store_read( Field_name );
        Store_read( Field_type );
        Store_read( Field_flags );
        Store_read( Field_custom );
        Store_read( Field_arity );
        Store_read( Field_enclosing );
        Store_zero( Field_code );
        break;
      }

      case Const_con: {
        Alloc_constant(Const_con_size);
        Store_zero( Field_fixup );
        Store_read( Field_name );
        Store_read( Field_type );
        Store_read( Field_flags );
        Store_read( Field_custom );
        Store_read( Field_arity );
        Store_read( Field_con_tag );
        break;
      }

      case Const_import: {
        Alloc_constant(Const_import_size);
        Store_zero( Field_fixup );
        Store_read( Field_name );
        Store_read( Field_type );
        Store_read( Field_flags );
        Store_read( Field_custom );
        Store_read( Field_import_module );
        Store_read( Field_import_name );
        Store_read( Field_import_const_kind );
        break;
      }

      case Const_extern: {
        Alloc_constant(Const_extern_size);
        Store_zero( Field_fixup );
        Store_read( Field_name );
        Store_read( Field_type );
        Store_read( Field_flags );
        Store_read( Field_custom );
        Store_read( Field_arity );
        Store_read( Field_extern_module );
        Store_read( Field_extern_name );
        Store_read( Field_extern_nameflag );
        Store_read( Field_extern_link );
        Store_read( Field_extern_call );
        break;
      }

      case Const_data: {
        Alloc_constant(Const_data_size);
        Store_zero( Field_fixup );
        Store_read( Field_name );
        Store_read( Field_type );   /* kind */
        Store_read( Field_flags );
        Store_read( Field_custom );
        Store_read( Field_arity );
        break;
      }

      case Const_typedecl: {
        Alloc_constant(Const_typedecl_size);
        Store_zero( Field_fixup );
        Store_read( Field_name );
        Store_read( Field_type );
        Store_read( Field_flags );
        Store_read( Field_custom );
        Store_read( Field_arity );
        break;
      }
    default: {
        Const_raise(( "unknown constant kind" ));
        break;
      }
    }

    if (p != next) {
      Const_raise( "invalid module: invalid constant length" );
    }
    p = next;
  }
  stat_free(buffer);
  decl = 0;

  CAMLreturn0;
}

/*----------------------------------------------------------------------
 resolve constants
 - resolve all internal and external constant references
----------------------------------------------------------------------*/
#define Check_constant(v,idx)  { if (idx <= 0 || idx > (long)Wosize_constants(constants))  \
                                   raise_module( fname, "invalid constant index" ); \
                                 else v = Constant(constants,idx); }

#define Check_tag(v,tag)  { if (!Is_block(v) && Tag_val(v) != tag) raise_module( fname, "invalid reference" ); }

#define Resolve_index(decl,fld,tag,idx) { value x; \
                                          Check_constant(x,idx); \
                                          Check_tag(x,tag); \
                                          Store_field(decl,fld,x); }
#define Resolve_field(decl,fld,tag)     { long idx = Long_val(Field(decl,fld)); \
                                          Resolve_index(decl,fld,tag,idx); }


static void resolve_module_name( value module, int index_name )
{
  CAMLparam1(module);
  CAMLlocal2(decl,constants);
  const char* fname;

  constants = Constants_module(module);
  fname     = String_val(Field(module,Module_fname));

  if (index_name == 0) {
    Store_field( module, Module_name, Field(module,Module_fname));
  }
  else {
    Check_constant(decl,index_name);
    Check_tag(decl,Const_name);
    Store_field( module, Module_name, Field(decl,Field_name_string));
  }

  CAMLreturn0;
}

static void resolve_constants( value module )
{
  CAMLparam1(module);
  CAMLlocal2(decl,constants);
  const char* fname;
  nat i;

  constants = Constants_module(module);
  fname     = String_val(Field(module,Module_fname));

  /* walk all constants to resolve references */
  for( i = 1; i <= Wosize_constants(constants); i++)
  {
    decl = Constant( constants, i );
    switch (Tag_val(decl)) {
      case Const_name:
      case Const_type:
      case Const_string:
      case Const_kind: {
       /* nothing to do */
       break;
      }

      case Const_module: {
        Resolve_field(decl,Field_module_name,Const_name);
        break;
      }

      case Const_value: {
        long  idx = Long_val(Field(decl,Field_enclosing));
        if (idx != 0) {
          value enclosing;
          Check_constant(enclosing,idx);
          Store_field(decl,Field_enclosing, enclosing);
        }
        /* fall through for name & type */
      }

      case Const_typedecl:
      case Const_con:   {
        long idx;
        Resolve_field(decl,Field_name,Const_name);

        idx = Long_val(Field(decl,Field_type));
        if (idx != 0) {
          Resolve_index(decl,Field_type,Const_type,idx);
        }

        break;
      }

      case Const_import: {
        long idx;
        Resolve_field(decl,Field_name,Const_name);

        idx = Long_val(Field(decl,Field_type));
        if (idx != 0) Resolve_index(decl,Field_type,Const_type,idx);

        Resolve_field(decl,Field_import_module,Const_module);
        Resolve_field(decl,Field_import_name,Const_name);
        break;
      }

      case Const_extern: {
        long idx;

        enum link_mode link = Long_val(Field(decl,Field_extern_link));
        enum name_flag flag = Long_val(Field(decl,Field_extern_nameflag));

        Resolve_field(decl,Field_name,Const_name);

        idx = Long_val(Field(decl,Field_type));
        if (idx != 0) Resolve_index(decl,Field_type,Const_type,idx);

        idx = Long_val(Field(decl,Field_extern_module));
        if (idx == 0 && link != Link_static) {
          raise_module(fname,"extern declaration without module name" );
        } else if (idx != 0) {
          Resolve_index(decl,Field_extern_module,Const_name,idx);
        }

        if (flag != Name_ordinal) {
          Resolve_field(decl,Field_extern_name,Const_name);
        }

        break;
      }

      case Const_data:   {
        long idx;
        Resolve_field(decl,Field_name,Const_name);

        idx = Long_val(Field(decl,Field_type));
        if (idx != 0) {
          Resolve_index(decl,Field_type,Const_kind,idx);
        }

        break;
      }

    default: {
        raise_module( fname, "unknown constant kind" );
      }
    }
  }

  /* and walk once more to fixup extern references */
  for( i = 1; i <= Wosize_constants(constants); i++)
  {
    decl = Constant( constants, i );
    switch (Tag_val(decl)) {
      case Const_import: {
        value* fixup  = load_symbol( module
                                   , Name_module_field(decl,Field_import_module)
                                   , Long_val(Field(Field(decl,Field_import_module),Field_module_major))
                                   , Name_field(decl,Field_import_name)
                                   , Long_val(Field( decl, Field_import_const_kind ))
                                   );
        Store_field(decl,Field_fixup,Val_ptr(fixup));
        break;
      }
      case Const_extern: {
        enum link_mode link  = Long_val(Field(decl,Field_extern_link));
        enum name_flag flag  = Long_val(Field(decl,Field_extern_nameflag));
        enum call_conv call  = Long_val(Field(decl,Field_extern_call));
        const char*    cname;
        void*          symbol;

        if (flag == Name_ordinal)
          cname = (const char*)(Long_val(Field(decl,Field_extern_name)));
        else
          cname = Name_field(decl,Field_extern_name);

        if (link == Link_dynamic) {
          symbol = load_dynamic_symbol( Name_field(decl,Field_extern_module),
                                        cname,
                                        call,
                                        Name_field(decl,Field_type),
                                        flag );
        } else if (link == Link_static) {
          symbol = load_static_symbol( Name_field(decl,Field_extern_module),
                                       cname,
                                       call,
                                       Name_field(decl,Field_type),
                                       flag );
        } else {
          symbol = NULL; /* nothing to fixup, supplied at runtime */
        }
        Store_field(decl,Field_fixup,Val_ptr(symbol));
        break;
      }
      default: {
        break;
      }
    }
  }


  CAMLreturn0;
}


/*----------------------------------------------------------------------
  read code
----------------------------------------------------------------------*/
static void read_code( const char* fname, int handle, int is_rev_endian,
                       unsigned code_len, unsigned code_count,
                       value module )
{
  CAMLparam1(module);
  CAMLlocal4(decl,code,cafs,constants);

  nat       read_count;
  nat       i;                /* index in code */
  opcode_t* p;                /* pointer in the code */
  nat       const_count;

  constants   = Constants_module(module);
  const_count = Wosize_constants(constants);

  /* read this section into a statically allocated buffer */
  code = alloc_bytes( code_len );
  Store_field(module,Module_code,code);

  read_count = file_read( handle, Bytes_val(code), code_len );
  file_close(handle);
  if (read_count != code_len)
    raise_module( fname, "truncated declaration section" );

  if (is_rev_endian) reverse_endian( (word_t*)Bytes_val(code), Word_bytes(code_len) );

  /* record all decls in the signatures */
  for(p = (opcode_t*)Bytes_val(code), i = 0; i < code_count; i++)
  {
    opcode_t* next;
    opcode_t  idx, len;
    idx     = *p++;
    len     = *p++;
    next    = p + Word_bytes(len);

    if (!Is_aligned(len))  raise_module( fname,  "unaligned declaration length" );
    if (idx <= 0 || idx > const_count) raise_module( fname,  "invalid declaration signature" );

    decl = Constant( constants, idx );
    switch (Tag_val(decl))
    {
    case Const_value: {
        if (Long_val(Field(decl,Field_arity)) == 0) {
          /* create a caf node */
          value caf;
          p++; /* skip dummy */

          /* invent a header: size is in bytes!!  */
          Hd_val(p) = Make_header( len-sizeof(opcode_t), Code_tag, Caml_white );

          /* create a caf node that points to the code */
          caf = alloc_small(1, Caf_tag);
          Store_field(caf,0,Val_code(p));

          /* store the caf node in the fixup field */
          Store_field( decl, Field_fixup, caf );
          Store_field( decl, Field_code, Val_code(p));
          break;
        }

        p++; /* skip dummy */

        /* invent a header */
        Hd_val(p) = Make_header( len-sizeof(opcode_t), Code_tag, Caml_white );

        /* store a pointer in the fixup field of the declaration */
        Store_field( decl, Field_fixup, Val_code(p) );
        Store_field( decl, Field_code, Val_code(p));
        break;
      }

    default:
       raise_module( fname, "invalid declaration kind for code" );
    }

    /* jump to next decl */
    p = next;
  }

  CAMLreturn0;
}


/*----------------------------------------------------------------------
  resolve code
----------------------------------------------------------------------*/
#if defined(THREADED_CODE)
 extern char** instr_table;
 #if defined(THREADED_OFFSET)
   extern char* instr_first;
   #define Set_instr(p,i)   {p = (opcode_t)(instr_table[i] - instr_first); }
 #else
   #define Set_instr(p,i)   {p = (opcode_t)(instr_table[i]);}
 #endif
#else
 #define Set_instr(p,i)     {p = i;}
#endif


static void apply_fixup( const char* name, opcode_t* opcode, void* fixup )
{
#if defined(FIXUP_OFFSET)
  long offset = Fixup_ptr(fixup);
  if (offset < -2147483647L || offset > 2147483647L) {
    raise_module( name, "can not fixup code references beyond a 4gb memory span" );
  }
  opcode[0] = (opcode_t)offset;
#else
  opcode[0] = (opcode_t)fixup;
#endif
}

static void fixup_con( const char* name, const char* instr_name,
                       opcode_t* opcode, value constants )
{
  CAMLparam1(constants);
  opcode_t const_count = Wosize_constants(constants);

  word_t  idx = opcode[0];
  value*  pdecl;
  value*  pdecl_org;
  void*   fixup;
  char    msg[MAXSTR];

  if (idx <= 0 || idx > const_count) {
    snprintf( msg, MAXSTR, "invalid constant index in instruction %s", instr_name );
    raise_module( name, msg );
  }

  /* check circular reference */
  pdecl_org = pdecl  = &Constant( constants, idx );
  while (Tag_val(*pdecl) == Const_import) {
    pdecl = Ptr_val(Field(*pdecl,Field_fixup));
    if (pdecl == pdecl_org) {
      snprintf( msg, MAXSTR, "circular reference on symbol \"%s\"", Name_field(*pdecl,Field_name) );
      raise_module( name, msg );
    }
  }

  /* check tag */
  if (Tag_val(*pdecl) != Const_con) {
    snprintf( msg, MAXSTR, "invalid constant record in %s instruction", instr_name );
    raise_module( name, msg );
  }


  fixup = (void*)Long_val(Field(*pdecl,Field_con_tag));
  apply_fixup( name, opcode, fixup );
  CAMLreturn0;
}

static void resolve_instrs( const char* name, opcode_t code_len, opcode_t* code
                          , value constants )
{
  CAMLparam1(constants);
  opcode_t  const_count = Wosize_constants(constants);
  opcode_t* opcode;

  /* fixup all instructions: resolve adresses and implement threaded code */
  for(opcode = code; opcode < code + Word_bytes(code_len); )
  {
    enum instruction instr = *opcode;

#ifdef THREADED_CODE
    Set_instr(opcode[0], instr);
#endif

    opcode++;

    switch (instr) {
    case SWITCHCON:
      opcode += opcode[0];
      break;

    case MATCHINT:
      /* TODO: check bounds */
      opcode += 2*opcode[0];
      break;

    case MATCHCON: {
      nat n = opcode[0];
      nat i;
      for (i = 1; i <= n; i++) {
        fixup_con( name, "MATCHCON", opcode + (2*i), constants );
      }
      opcode += 2*n;
      break;
    }

/*
    case EVALVAR:
      opcode[0] -= 3;
      break;
*/
    case CALL:

    case ENTERCODE:
    case PUSHCODE:

    case PUSHSTRING:

    case RETURNCON:
    case RETURNCON0:
    case ALLOCCON:
    case NEWCON:
    case NEWCON0:
    case NEWCON1:
    case NEWCON2:
    case NEWCON3:
    case TESTCON:  {
      word_t  idx = opcode[0];
      value*  pdecl;
      value*  pdecl_org;
      void*   fixup;
      char msg[MAXSTR];

      if (idx <= 0 || idx > const_count) {
        raise_module( name, "invalid constant index in instruction" );
      }

      pdecl_org = pdecl  = &Constant( constants, idx );
      while (Tag_val(*pdecl) == Const_import) {
        pdecl = Ptr_val(Field(*pdecl,Field_fixup));
        if (pdecl == pdecl_org) {
          snprintf( msg, MAXSTR, "circular reference on symbol \"%s\"", Name_field(*pdecl,Field_name) );
          raise_module( name, msg );
        }
      }

      switch (instr) {
      case RETURNCON:
      case ALLOCCON:
      case NEWCON: {
          /* check tag */
          if (Tag_val(*pdecl) != Const_con) {
            raise_module( name, "invalid constant record in CON instruction" );
          }

          /* check arity */
          if (Long_val(Field(*pdecl,Field_arity)) != (long)opcode[1]) {
            raise_module( name, "size doesn't match arity in CON instruction" );
          }

          fixup = (void*)Long_val(Field(*pdecl,Field_con_tag));
          break;
        }

      case RETURNCON0:
      case NEWCON0:
      case NEWCON1:
      case NEWCON2:
      case NEWCON3:
      case TESTCON:   {
          /* check tag */
          if (Tag_val(*pdecl) != Const_con) {
            raise_module( name, "invalid constant record in RETURNCON0/NEWCON/TESTCON instruction" );
          }

          fixup = (void*)Long_val(Field(*pdecl,Field_con_tag));
          break;
        }
      case PUSHSTRING: {
          /* check tag */
          if (Tag_val(*pdecl) != Const_string) {
            raise_module( name, "invalid constant record in PUSHSTRING instruction" );
          }

          fixup = pdecl;
          break;
        }
      case CALL: {
          /* check tag */
          if (Tag_val(*pdecl) != Const_extern) {
            raise_module( name, "invalid constant record in CALL instruction" );
          }

          /* check number of arguments */
          if (opcode[1]+1 != strlen( Type_field(*pdecl,Field_type)) ) {
            snprintf( msg, MAXSTR, "type doesn't match number of arguments on call to symbol \"%s\"", Name_field(*pdecl,Field_name) );
            raise_module( name, msg );
          }

          fixup = pdecl;
          break;
        }

      case PUSHCODE: {
          /* check for caf */
          if (Is_caf_val(*pdecl)) {
            Set_instr(opcode[-1],PUSHCAF);
            fixup = pdecl;                      /* caf pointer can move */
            break;
          }
          /* fall through */
        }
      case ENTERCODE:
      default: {
          /* check tag */
          if (Tag_val(*pdecl) != Const_value)
            raise_module( name, "invalid constant reference in instruction PUSHCODE/ENTERCODE" );

          fixup = Code_val(Field(*pdecl,Field_fixup));
          break;
        }
      }

#if defined(FIXUP_OFFSET)
      { long offset = Fixup_ptr(fixup);
        if (offset < -2147483647L || offset > 2147483647L) {
          raise_module( name, "can not fixup code references beyond a 4Gb memory span" );
        }
        opcode[0] = (opcode_t)offset;
      }
#else
      opcode[0] = (opcode_t)fixup;
#endif

      /* Assert(opcode[0] != 0);  can happen with con tags */
      break;
    }

    case RETURNINT:
    case PUSHINT:
    case INCINT:
    case TESTINT: {
      #ifdef CHECK_BOUNDS
      long i = opcode[0];
      if (i < Min_long || i > Max_long) {
        value decl = Constant( constants,idx);
        const char* name = Name_field(decl, Field_name);
        if (i< Min_long)
          raise_exn_str( Exn_underflow, name );
        else
          raise_exn_str( Exn_overflow, name );
      }
      #endif
      /* shaky on 64bit archs: opcode[0] = (opcode_t)Val_long(opcode[0]); */
      break;
    }
    default:
      break;
    }

    opcode += instr_arg_count(instr);
  }

  CAMLreturn0;
}


static void resolve_code( value module )
{
  CAMLparam1(module);
  CAMLlocal2(code,constants);
  const char* fname;
  nat         code_count;
  nat       i;                /* index in decls */
  opcode_t* p;                /* pointer in the decls */

  constants   = Constants_module(module);
  code        = Field(module,Module_code);
  code_count  = Long_val(Field(module,Module_code_count));
  fname       = String_val(Field(module,Module_name));

  /* fixup all code and constant references */
  for( p = Void_bytes_val(code), i = 0; i < code_count; i++ )
  {
    opcode_t* next;
    opcode_t  idx, len;
    idx       = *p++;
    p++; /* len invalid, overwritten on 64bit machines! */
    p++; /* dummy invalid, overwritten with header! */
    Assert(Is_block((value)p) && Tag_val((value)p) == Code_tag);

    len       = Wosize_val((value)p); /* note: size is in bytes! */
    next      = p + Word_bytes(len);

    switch (Tag_val(Constant(constants,idx))) {
    case Const_value: {
        resolve_instrs( fname, len, p, constants );
        break;
      }
    default: {
        raise_module( fname,  "invalid declaration kind for code" );
      }
    }

    /* jump to next decl */
    p = next;
  }

  CAMLreturn0;
}


/*----------------------------------------------------------------------
 load_symbol
----------------------------------------------------------------------*/
static value read_module( value parent, const char* modname )
{
  CAMLparam1(parent);
  CAMLlocal2(module,constants);
  int      handle        = 0;
  int      is_rev_endian = 0;
  struct module_header_t header;
  const char* fname = modname;

  /* read the header */
  handle = open_module( &fname );
  read_header( fname, handle, &header, &is_rev_endian );

  /* read the module */
  module = alloc( Module_size, Module_tag );

  if (parent) {
    Store_field(module,Module_next,Field(parent,Module_next));
    Store_field(parent,Module_next,module);
  } else {
    Store_field( module, Module_next, module );
  }

  /* Store_field( module, Module_name, copy_string(modname) ); */
  Store_field( module, Module_fname, copy_string(fname) );
  Store_field( module, Module_major, Val_long(header.module_major_version) );
  Store_field( module, Module_minor, Val_long(header.module_minor_version) );
  Store_field( module, Module_code_count, Val_long(header.decl_count) );
  Store_field( module, Module_code_len,   Val_long(header.decl_length) );

  constants = alloc_fixed( header.constant_count );
  Store_field( module, Module_constants, constants );

  read_constants( fname, handle, is_rev_endian,
                  header.constant_length, constants );

  /* resolve the module name */
  resolve_module_name( module, header.module_name );

  read_code( fname, handle, is_rev_endian,
              header.decl_length, header.decl_count, module );
  file_close(handle);

  /* resolve constants */
  resolve_constants( module );

  /* print( "module %s\n", String_val(Field(module,Module_name)));   */

  CAMLreturn(module);
}

static value find_module( value module, const char* modname, long major_version )
{
  CAMLparam1(module);
  CAMLlocal1(mod);

  /* check if it is already loaded */
  mod = module;
  do{
    if (stricmp(String_val(Field(mod,Module_name)),modname) == 0) {
      if (Long_val(Field(mod,Module_major)) != major_version) {
        raise_module( modname, "invalid version number -- please recompile the main program" );
      }
      CAMLreturn(mod);
    }

    mod = Field(mod,Module_next);
  } while (mod != module);

  /* ok, we need to load it */
  mod = read_module( module, modname );
  if (Long_val(Field(mod,Module_major)) != major_version) {
    raise_module( modname, "invalid version number -- please recompile the main program" );
  }
  CAMLreturn(mod);
}

static value* find_symbol( value module, const char* name, enum const_kind kind )
{
  CAMLparam1(module);
  CAMLlocal2(constants,decl);
  char msg[MAXSTR];
  nat i;

  constants = Field(module,Module_constants);
  for( i = 1; i <= Wosize_constants(constants); i++)
  {
    decl = Constant(constants,i);
    if (   Tag_val(decl) == kind
        && (Long_val(Field(decl,Field_flags)) & Flag_public) != 0
        && strcmp(name,Name_field(decl,Field_name)) == 0)
    {
        CAMLreturn(&Constant(constants,i));
    }
    /*
      switch (Tag_val(decl)) {
      case Const_value:
      case Const_con:
      case Const_import:
      case Const_extern:
      case Const_data:
      case Const_typedecl: {
        if (  (Long_val(Field(decl,Field_flags)) & Flag_public) != 0
           && strcmp(name,Name_field(decl,Field_name)) == 0) {
          CAMLreturn(&Constant(constants,i));
        }
        break;
      }

      default: {
        break;
      }
    }
     */
  }

  snprintf( msg, MAXSTR, "does not export symbol \"%s\"", name );
  raise_module( String_val(Field(module,Module_name)), msg );
  CAMLreturn(0);
}

static value* load_symbol( value module, const char* modname, long major_version
                         , const char* name, enum const_kind kind )
{
  CAMLparam1(module);
  CAMLreturn(find_symbol(find_module(module,modname,major_version),name, kind));
}


/*----------------------------------------------------------------------
 load_module
----------------------------------------------------------------------*/
value load_module( const char* name )
{
  CAMLparam0();
  CAMLlocal2(module,mod);

  module = read_module( 0, name );

  /* resolve all references */
  mod = module;
  do{
    resolve_code(mod);
    mod = Field(mod,Module_next);
  } while (mod != module);

  CAMLreturn(module);
}
