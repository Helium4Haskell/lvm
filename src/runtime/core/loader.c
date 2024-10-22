/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>     /* strerror */
#include <fcntl.h>
#include <errno.h>     /* errno */
#include "mlvalues.h"
#include "memory.h"
#include "alloc.h"
#include "heap/heap.h"

#include "sys.h"
#include "fail.h"
#include "reverse.h"
#include "bytes.h"
#include "instr.h"
#include "module.h"
#include "dynamic.h"
#include "print.h"
#include "prim/prims.h"
#include "options.h"    /* lvmpath */


#ifdef TRACE_TRACE
# define Trace(msg)             { print(msg); print("\n"); }
# define Trace_i(msg,i)         { print(msg); print(" %i\n", i); }
# define Trace_i_str(msg,i,str) { print(msg); print(" %i \"%s\"\n", i, str); }
#else
# define Trace(msg)
# define Trace_i(msg,i)
# define Trace_i_str(msg,i,str)
#endif

#define VERSION_MAJOR 15
#define VERSION_MINOR 1

/*----------------------------------------------------------------------
  alignment & endianess
----------------------------------------------------------------------*/
#define Is_aligned(i) ((i) % sizeof(word_t) == 0)
#define Word_bytes(i) (((i) + sizeof(word_t) - 1) / sizeof(word_t))
#define Word_sizeof(x)  (Word_bytes(sizeof(x)))

#define Reverse_word(w1,w2)  Reverse_32(w1,w2)


static void reverse_endian( word_t* data, wsize_t count )
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
                         , const char* name, value kind );


static void resolve_instrs( const char* name, wsize_t code_len, opcode_t* code
                          , value records );

/*----------------------------------------------------------------------
   read the header
----------------------------------------------------------------------*/
static int open_module(const char **fname)
{
  const char * fullname;
  int fd;

  fullname = searchpath_lvm(*fname);
  if (fullname != NULL) *fname = fullname;

  fd = file_open_binary(*fname, O_RDONLY );
  if (fd < 0) {
    if (fullname == NULL)
      raise_module( *fname, "file not found in the search path:\n  \"%s\"", lvmpath );
    else
      raise_module( *fname, "file can not be opened, code %li: %s", errno, strerror(errno) );
  }

  return fd;
}

#define Decode(x)     (Long_val(x))

static void read_header( const char* name, int handle,
                          struct module_header_t* header, int* is_rev_endian )
{
  int res;

  *is_rev_endian = 0;

  /* TODO: skip possible extra fields in the header */
  res = file_read( handle, (void*)header, sizeof(struct module_header_t) );
  if (res != sizeof(struct module_header_t))
  {
    file_close(handle);
    raise_module( name, "truncated header" );
  }

  if (Magic_header != header->magic)
  {
    word_t rev_magic = header->magic;
    Reverse_word(  &rev_magic, &rev_magic );
    if (Magic_header != rev_magic) {
      file_close(handle);
      raise_module( name, "invalid magic number.\n\nHINT: Maybe it's not a valid LVM file" );
    }

    *is_rev_endian = 1;
    reverse_endian( (word_t*)header, Word_sizeof(*header) );
  }

  /* convert ints & indices */
#define headerInt(x)  header->x = Decode(header->x)
#define headerIdx(x)  header->x = Decode(header->x)
    headerInt(magic);
    headerInt(header_length);
    headerInt(total_length);
    headerInt(major_version);
    headerInt(minor_version);
    headerInt(records_count);
    headerInt(records_length);
    headerIdx(module_idx);

  if (header->major_version != VERSION_MAJOR ||
      header->minor_version > VERSION_MINOR) {
    raise_module( name, "module requires runtime version %i.%02i but this is runtime version %i.%02i.\n"
                        "  please recompile with a compatible compiler or update the lvm runtime"
      , header->major_version, header->minor_version
      , VERSION_MAJOR, VERSION_MINOR );
  }

  /* skip trailing fields */
  file_skip(handle, header->header_length + 8 - sizeof(struct module_header_t));
  return;
}

/*----------------------------------------------------------------------
 read records
 - read all records into the records array. don't resolve ref's yet
----------------------------------------------------------------------*/
#define Raw_read(x)           {(x) = *p++; if (is_rev_endian) { Reverse_word(&x,&x); }; }
#define Word_read(x)          { Raw_read(x); (x) = Decode(x); }
#define String_read(v,n)      {(v) = alloc_string_major(n); bcopy(p,String_val(v),n); p += Word_bytes(n); }
#define Store_read(fld)       { word_t x; Word_read(x); Init_field(rec,fld,Val_long(x)); }
#define Store_zero(fld)       { Init_field(rec,fld,Val_long(0)); }
#define Alloc_record(size)    { rec = alloc_major(size,kind); Record(records,i) = rec; }



#define Rec_raise(msg) {stat_free(buffer); file_close(handle); raise_module( fname, msg ); }


static void read_records( const char* fname, int handle, int is_rev_endian,
                          unsigned record_len, value records )
{
  CAMLparam1(records);
  CAMLlocal4(rec,instrs,str,code);

  wsize_t read_count;
  void*   buffer;
  struct  module_footer_t footer;

  wsize_t i;    /* index in records */
  word_t* p;    /* pointer in the read buffer */

  /* read this section first into a statically allocated buffer */
  buffer      = stat_alloc( record_len );
  read_count  = file_read( handle, buffer, record_len );
  if (read_count != record_len)  Rec_raise( "truncated record section" );

  /* read the footer */
  read_count  = file_read( handle, &footer, sizeof(footer) );
  if (read_count != sizeof(footer)) Rec_raise( "truncated footer section" );
  if (is_rev_endian) { Reverse_word( &footer.magic, &footer.magic ); }
  if (footer.magic != Magic_footer) Rec_raise( "invalid magic number in footer section" );

  /* process all records in the buffer */
  for(p = buffer, i = 1; i <= Wosize_records(records); i++)
  {
    word_t kind;
    word_t len;
    word_t* next;

    Raw_read(kind);
    Word_read(len);

    if (!Is_aligned(len)) Rec_raise( "unaligned declaration" );
    next = p + Word_bytes(len);

    if (!Is_long(kind)) {
      word_t kindidx = Decode(kind);
      kind = Rec_custom;              /* used by "Alloc_record" */
      Trace_i( "Rec_custom", i );
      Alloc_record(Rec_custom_size);
      Store_read(Field_name);
      if (Field(rec,Field_name)!=0) {
        Store_read(Field_flags);
      } else {
        Store_zero(Field_flags);
      }
      Store_field(rec,Field_custom_kind,Val_long(kindidx));
    }
    else {
      kind = Decode(kind);
      switch(kind)
      {
        case Rec_name:
        case Rec_kind:
        case Rec_bytes:
        case Rec_extern_type:
        { word_t slen;
          Alloc_record(Rec_name_size);
          Word_read(slen);
          String_read(str,slen);
          Trace_i_str ("Rec_string", i, str);
          Init_field( rec, Field_name_string, str );
          break;
        }

        case Rec_module: {
          Trace_i ("Rec_module", i);
          Alloc_record(Rec_module_size);
          Store_read( Field_module_name );
          Store_read( Field_module_major );
          Store_read( Field_module_minor );
          break;
        }

        case Rec_value: {
          Trace_i ("Rec_value", i);
          Alloc_record(Rec_value_size);
          Store_read( Field_name );
          Store_read( Field_flags );
          Store_read( Field_arity );
          Store_read( Field_value_enc );
          Store_read( Field_value_code );
          Store_zero( Field_value_fun );
          Trace_i ("-name", Long_val(Field(rec,Field_value_name)));
          Trace_i ("-code", Long_val(Field(rec,Field_value_code)));
          break;
        }

        case Rec_con: {
          Trace_i ("Rec_con", i);
          Alloc_record(Rec_con_size);
          Store_read( Field_name );
          Store_read( Field_flags );
          Store_read( Field_arity );
          Store_read( Field_con_tag );
          break;
        }

        case Rec_import: {
          word_t impkind;
          Trace_i ("Rec_import", i);
          Alloc_record(Rec_import_size);
          Store_read( Field_name );
          Store_read( Field_flags );
          Store_read( Field_import_module );
          Store_read( Field_import_name );
          Raw_read(impkind);
          if (Is_long(impkind)) {
            Store_field( rec, Field_import_iscustom, Val_long(0) );
            Store_field( rec, Field_import_kind, impkind );
          }
          else {
            Store_field( rec, Field_import_iscustom, Val_long(1) );
            Store_field( rec, Field_import_kind, Val_long(Decode(impkind)) );
          }
          Store_zero( Field_import_fixup );
          Trace_i ("-name", Long_val(Field(rec,Field_import_name)));
          Trace_i ("-module", Long_val(Field(rec,Field_import_module)));
          break;
        }

        case Rec_extern: {
          Trace_i ("Rec_extern", i);
          Alloc_record(Rec_extern_size);
          Store_read( Field_name );
          Store_read( Field_flags );
          Store_read( Field_arity );
          Store_read( Field_extern_type );
          Store_read( Field_extern_module );
          Store_read( Field_extern_name );
          Store_read( Field_extern_nameflag );
          Store_read( Field_extern_link );
          Store_read( Field_extern_call );
          Store_zero( Field_extern_fun );
          Store_zero( Field_extern_symbol );
          break;
        }

        case Rec_code: {
          char* instrs;
          wsize_t instrlen = len;
          Trace_i ("Rec_code", i);

          if (!Is_aligned(instrlen)) Rec_raise( "unaligned instructions" );

          Alloc_record(Rec_code_size);

          /* allocate non-gc'd memory for the instructions */
          code   = alloc_bytes(instrlen+sizeof(header_t));
          instrs = Bytes_val(code);
          Store_field(rec,Field_code_code,code);

          /* pretend that the bytes are a heap block & copy  (CAF's are allocated during resolve) */
          instrs += sizeof(header_t);
          Hd_val(instrs) = Make_header(instrlen /* in bytes! */, Code_tag, Caml_white );
          memcpy(instrs, p, instrlen);
          if (is_rev_endian) reverse_endian( (word_t*)instrs, Word_bytes(instrlen) );

          /* and increment the read pointer */
          p += Word_bytes(instrlen);
          break;
        }

        default: {
          Rec_raise(( "unknown record kind" ));
          break;
        }
      } /* switch (kind) */
    } /*  if custom kind */

/*
#ifdef DEBUG
    if (p != next) {
      Rec_raise( "invalid constant length" );
    }
#endif
*/
    p = next;
  }
  stat_free(buffer);
  rec = 0;

  CAMLreturn0;
}

/*----------------------------------------------------------------------
 resolve internal records
 ---------------------------------------------------------------------*/
#define Check_record(v,idx)  { if (idx <= 0 || idx > (long)Wosize_records(records))  \
                                   raise_module( fname, "invalid record index" ); \
                                 else v = Record(records,idx); }

#define Check_tag(v,tag)  { if (!Is_block(v) && Tag_val(v) != tag) raise_module( fname, "invalid reference tag" ); }

#define Resolve_index(decl,fld,tag,idx) { value x; \
                                          Check_record(x,idx); \
                                          Check_tag(x,tag); \
                                          Store_field(decl,fld,x); }
#define Resolve_field(decl,fld,tag)     { long idx = Long_val(Field(decl,fld)); \
                                          Resolve_index(decl,fld,tag,idx); }


static void resolve_module_info( value module, long index_info )
{
  CAMLparam1(module);
  CAMLlocal1(records);
  const char* fname;

  fname   = String_val(Field(module,Module_fname));
  records = Records_module(module);
  Resolve_index(module,Module_info,Rec_module,index_info);

  CAMLreturn0;
}

static void resolve_internal_records( value module )
{
  CAMLparam1(module);
  CAMLlocal3(rec,val,records);
  const char* fname;
  wsize_t i;

  records = Records_module(module);
  fname   = String_val(Field(module,Module_fname));

  /* walk all constants to resolve references */
  for( i = 1; i <= Count_records(records); i++)
  {
    rec = Record( records, i );
    switch (Tag_val(rec)) {
      case Rec_name:
      case Rec_kind:
      case Rec_extern_type:
      case Rec_bytes: {
       /* nothing to do */
       break;
      }

      case Rec_module: {
        Resolve_field(rec,Field_module_name,Rec_name);
        break;
      }

      case Rec_value: {
        long idx = Long_val(Field(rec,Field_value_enc));
        if (idx != 0) Resolve_index(rec,Field_value_enc,Rec_value,idx);
        Resolve_field(rec,Field_name,Rec_name);

        /* resolve the code */
        Resolve_field(rec,Field_value_code,Rec_code);
        Field(rec,Field_value_fun) = Code_value(rec);

        /* allocate a CAF if necessary */
        if (Long_val(Field(rec,Field_value_arity)) == 0) {
          value caf = alloc_major(1,Caf_tag);
          Store_field(caf,0,Field(rec,Field_value_fun));
          Store_field(rec,Field_value_fun,caf);
        }

        break;
      }

      case Rec_code: {
        break;
      }

      case Rec_con:   {
        Resolve_field(rec,Field_name,Rec_name);
        break;
      }

      case Rec_import: {
        Resolve_field(rec,Field_name,Rec_name);
        Resolve_field(rec,Field_import_module,Rec_module);
        Resolve_field(rec,Field_import_name,Rec_name);
        if (Long_val(Field(rec,Field_import_iscustom))==1) {
          Resolve_field(rec,Field_import_kind,Rec_kind);
        }
        break;
      }

      case Rec_custom: {
        long idx = Long_val(Field(rec,Field_name));
        if (idx!=0) Resolve_index(rec,Field_name,Rec_name,idx);
        Resolve_field(rec,Field_custom_kind,Rec_kind);
        break;
      }

      case Rec_extern: {
        long idx;

        enum link_mode link = Long_val(Field(rec,Field_extern_link));
        enum name_flag flag = Long_val(Field(rec,Field_extern_nameflag));

        Resolve_field(rec,Field_name,Rec_name);

        idx = Long_val(Field(rec,Field_extern_type));
        if (idx != 0) Resolve_index(rec,Field_extern_type,Rec_extern_type,idx);

        idx = Long_val(Field(rec,Field_extern_module));
        if (idx == 0 && link != Link_static) {
          raise_module(fname,"extern declaration without module name" );
        } else if (idx != 0) {
          Resolve_index(rec,Field_extern_module,Rec_name,idx);
        }

        if (flag != Name_ordinal) {
          Resolve_field(rec,Field_extern_name,Rec_name);
        }

        break;
      }

      default: {
/*
#ifdef DEBUG
        raise_module( fname, "unknown record kind during resolve" );
#endif
*/
        break;
      }
    }
  }

  CAMLreturn0;
}

/*----------------------------------------------------------------------
 resolve external records
---------------------------------------------------------------------*/
static void resolve_external_records( value module )
{
  CAMLparam1(module);
  CAMLlocal3(rec,records,symbol);
  const char* fname;
  wsize_t i;

  records = Records_module(module);
  fname   = String_val(Field(module,Module_fname));

  /* walk the records to fixup extern references */
  for( i = 1; i <= Count_records(records); i++)
  {
    rec = Record( records, i );
    switch (Tag_val(rec)) {
      case Rec_import: {
        value* fixup  = load_symbol( module
                                   , Name_module_field(rec,Field_import_module)
                                   , Long_val(Field(Field(rec,Field_import_module),Field_module_major))
                                   , Name_field(rec,Field_import_name)
                                   , Field( rec, Field_import_kind )
                                   );
        Store_field(rec,Field_import_fixup,Val_ptr(fixup));
        break;
      }
      case Rec_extern: {
        enum link_mode link  = Long_val(Field(rec,Field_extern_link));
        enum name_flag flag  = Long_val(Field(rec,Field_extern_nameflag));
        enum call_conv call  = Long_val(Field(rec,Field_extern_call));
        const char*    cname;

        if (flag == Name_ordinal)
          cname = (const char*)(Long_val(Field(rec,Field_extern_name)));
        else
          cname = Name_field(rec,Field_extern_name);

        if (link == Link_dynamic) {
          symbol = load_dynamic_symbol( Name_field(rec,Field_extern_module),
                                        cname,
                                        call,
                                        Name_field(rec,Field_extern_type),
                                        flag );
          Store_field(rec,Field_extern_symbol,symbol);
          Store_field(rec,Field_extern_fun,Val_ptr(Symbol_fun(symbol)));
        }
        else if (link == Link_static) {
          void* fun = load_static_symbol( Name_field(rec,Field_extern_module),
                                          cname,
                                          call,
                                          Name_field(rec,Field_extern_type),
                                          flag );
          Store_field(rec,Field_extern_fun,Val_ptr(fun));
        }
        else {
          Store_field(rec,Field_extern_fun,Val_ptr(NULL)); /* nothing to fixup, supplied at runtime */
        }
        break;
      }
      default: {
        break;
      }
    }
  }

  rec = 0;
  CAMLreturn0;
}


/*----------------------------------------------------------------------
   resolve code
---------------------------------------------------------------------*/
static void resolve_code( value module )
{
  CAMLparam1(module);
  CAMLlocal3(rec,val,records);
  const char* fname;
  wsize_t i;

  records = Records_module(module);
  fname   = String_val(Field(module,Module_fname));

  /* walk the records to fixup instructions */
  for( i = 1; i <= Count_records(records); i++)
  {
    rec = Record( records, i );
    switch (Tag_val(rec)) {
      case Rec_code: {
        wsize_t len;

        /* load the instruction pointer & length */
        val = Code_code(rec);
        Assert(Is_block(val) && Tag_val(val) == Code_tag);
        len = Wosize_val(val); /* note: in bytes! */

        /* and resolve the instructions */
        resolve_instrs( fname, len, (opcode_t*)val, records );
        break;
      }
      default: {
        break;
      }
    }
  }

  rec = 0;
  CAMLreturn0;
}

/*----------------------------------------------------------------------
 read_module
----------------------------------------------------------------------*/
static value read_module( value parent, const char* modname )
{
  CAMLparam1(parent);
  CAMLlocal2(module,records);
  int      handle        = 0;
  int      is_rev_endian = 0;
  struct module_header_t header;
  const char* fname = modname;

  /* read the header */
  handle = open_module( &fname );
  read_header( fname, handle, &header, &is_rev_endian );

  /* allocate the module */
  module = alloc_major( Module_size, Module_tag );

  if (parent) {
    Init_field(module,Module_next,Field(parent,Module_next));
    Store_field(parent,Module_next,module);
  } else {
    Init_field( module, Module_next, module );
  }

  Init_field( module, Module_fname, copy_string(fname) );
  records = alloc_fixed( header.records_count );
  Init_field( module, Module_records, records );

  gc_full_major(); /* fixes GC bug for unknown reasons :-( */

debug_gc();
  read_records( fname, handle, is_rev_endian,
                header.records_length, Field(module,Module_records));
  file_close(handle);


  /* resolve */
  resolve_module_info( module, header.module_idx );
  resolve_internal_records( module );

  /* [resolve_external_records] recursively reads other modules */
  resolve_external_records( module );

  /* print( "module %s\n", String_val(Field(module,Module_name)));   */
  CAMLreturn(module);
}

/*----------------------------------------------------------------------
 find_module
----------------------------------------------------------------------*/
static value find_module( value module, const char* modname, long major_version )
{
  CAMLparam1(module);
  CAMLlocal1(mod);
  long major;

  /* check if it is already loaded */
  mod = module;
  do{
    if (stricmp(Name_field(Field(mod,Module_info),Field_module_name),modname) == 0) {
      major = Long_val(Field(Field(mod,Module_info),Field_module_major));
      if (major != major_version) {
        raise_module( modname, "version %i required but module has version %i -- please recompile the main program", major_version, major );
      }
      CAMLreturn(mod);
    }

    mod = Field(mod,Module_next);
  } while (mod != module);

  /* ok, we need to load it */
  mod = read_module( module, modname );

  major = Long_val(Field(Field(mod,Module_info),Field_module_major));
  if (major != major_version) {
    raise_module( modname, "version %i required but module has version %i -- please recompile the main program", major_version, major );
  }
  CAMLreturn(mod);
}


/*----------------------------------------------------------------------
 find_symbol
----------------------------------------------------------------------*/
static int match_kind( value kind1, value kind2 )
{
  CAMLparam2(kind1,kind2);
  if (Is_long(kind1) && Is_long(kind2)) {
    CAMLreturn( kind1==kind2 );
  }
  else if (Is_block(kind1) && Tag_val(kind1)==Rec_kind &&
           Is_block(kind2) && Tag_val(kind2)==Rec_kind) {
    CAMLreturn( strcmp(String_val(Field(kind1,Field_name))
                      ,String_val(Field(kind2,Field_name)))==0 );
  }
  else {
   CAMLreturn(false);
  }
}


static int match_kind_rec( value kind, value rec )
{
  CAMLparam2(kind,rec);
  if (Tag_val(rec)==Rec_import) {
    CAMLreturn(match_kind(kind,Field(rec,Field_import_kind)));
  }
  else if (Tag_val(rec)==Rec_custom) {
    CAMLreturn(match_kind(kind,Field(rec,Field_custom_kind)));
  }
  else if (Is_long(kind)) {
    CAMLreturn(match_kind(kind,Val_long(Tag_val(rec))));
  }
  else {
    CAMLreturn(false);
  }
}

static value* find_symbol( value module, const char* name, value kind )
{
  CAMLparam2(module,kind);
  CAMLlocal2(records,rec);
  wsize_t i;

  records = Field(module,Module_records);
  for( i = 1; i <= Count_records(records); i++)
  {
    rec = Record(records,i);
    if (   (Long_val(Field(rec,Field_flags)) & Flag_public) == Flag_public
        && match_kind_rec( kind, rec )
        && strcmp(name,Name_field(rec,Field_name)) == 0)
    {
        CAMLreturn(&Record(records,i));
    }
  }


  raise_module( Name_field(Field(module,Module_info),Field_module_name), "module doesn't export symbol \"%s\"", name );
  CAMLreturn(0);
}

/*----------------------------------------------------------------------
 load_symbol
----------------------------------------------------------------------*/
static value* load_symbol( value module, const char* modname, long major_version
                         , const char* name, value kind )
{
  CAMLparam2(module,kind);
  value* result = find_symbol(find_module(module,modname,major_version),name, kind);
  CAMLreturn(result);
}


/*----------------------------------------------------------------------
  load_module
----------------------------------------------------------------------*/
value load_module( const char* name )
{
  CAMLparam0();
  CAMLlocal2(module,mod);

  module = read_module( 0, name );

  /* resolve all references in the code */
  mod = module;
  do{
    resolve_code(mod);
    mod = Field(mod,Module_next);
  } while (mod != module);

  CAMLreturn(module);
}



/*----------------------------------------------------------------------
  apply instruction fixups
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



/*----------------------------------------------------------------------
  fixup helpers
----------------------------------------------------------------------*/
static void fixup_ptr( const char* name, opcode_t* opcode, void* fixup )
{
#if defined(FIXUP_OFFSET)
  long offset = Fixup_ptr(fixup);
  if (offset < Min_word_t || offset > Max_word_t) {
    raise_module( name, "can not fixup code references beyond a 4gb memory span" );
  }
  opcode[0] = (opcode_t)offset;
#else
  opcode[0] = (opcode_t)fixup;
#endif
}

static void fixup_code( const char* name, opcode_t* opcode, value val )
{
  CAMLparam1(val);
  fixup_ptr( name, opcode, Code_val(Code_value(val)) );
  CAMLreturn0;
}

static void fixup_con( const char* name, opcode_t* opcode, con_tag_t tag )
{
  if (tag < 0)           { raise_module( name, "negative tag (%li)", tag ); }
  if (  tag > Max_word_t
     || tag > Max_long)  { raise_module( name, "tag is too large (%li)", tag ); }
  opcode[0] = (opcode_t)tag;
}

/*----------------------------------------------------------------------
  resolve helpers
----------------------------------------------------------------------*/
static value* resolve_index( const char* name, const char* instr_name,
                              opcode_t* opcode, value records, enum rec_kind kind )
{
  CAMLparam1(records);
  opcode_t record_count = Count_records(records);

  word_t  idx   = opcode[0];
  value*  prec;
  value*  prec_org;

  /* check index */
  if (idx <= 0 || idx > record_count) {
    raise_module( name, "invalid constant index in instruction %s", instr_name );
  }

  /* check circular reference */
  prec_org = prec  = &Record( records, idx );
  while (Tag_val(*prec) == Rec_import) {
    prec = Ptr_val(Field(*prec,Field_import_fixup));
    if (prec == prec_org) {
      raise_module( name, "circular reference on symbol \"%s\"", Name_field(*prec,Field_name) );
    }
  }

  /* check kind */
  if (Tag_val(*prec) != kind) {
    raise_module( name, "invalid constant record in %s instruction", instr_name );
  }
  Trace_i_str ("resolve_index", idx, instr_name);
  CAMLreturn(prec);
}

/* for constructors */
static value* resolve_con( const char* name, const char* instr_name,
                              opcode_t* opcode, value records )
{
  CAMLparam1(records);
  int32  idx  = (int32)(opcode[0]);
  value* prec = NULL;

  if (idx <= 0) {
    /* interpret as direct tag value */
    fixup_con( name, opcode, -idx );
  }
  else {
    /* lookup the tag from the declaration */
    prec = resolve_index( name, instr_name, opcode, records, Rec_con );
    fixup_con( name, opcode, Long_val(Field(*prec,Field_con_tag)) );
  }
  CAMLreturn(prec);
}

/* for CALL & PUSHBYTES */
static value* resolve_rec( const char* name, const char* instr_name,
                           opcode_t* opcode, value records, enum rec_kind kind )
{
  CAMLparam1(records);
  value* prec = resolve_index( name, instr_name, opcode, records, kind );
  fixup_ptr( name, opcode, prec );
  CAMLreturn(prec);
}


/*----------------------------------------------------------------------
  resolve instructions
----------------------------------------------------------------------*/
static void resolve_instrs( const char* name, wsize_t code_len, opcode_t* code
                          , value records )
{
  CAMLparam1(records);
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

    case MATCH:
      opcode += 3*opcode[0];
      break;

    case MATCHCON: {
      wsize_t n = opcode[0];
      wsize_t i;
      for (i = 1; i <= n; i++) {
        resolve_con( name, "MATCHCON", opcode + (2*i), records );
      }
      opcode += 2*n;
      break;
    }

    case RETURNCON0:
    case NEWCON:
    case NEWCON0:
    case NEWCON1:
    case NEWCON2:
    case NEWCON3: {
      resolve_con( name, "CON", opcode, records );
      break;
    }

    case ALLOCCON:
    case RETURNCON:
    case TESTCON:  {
      value* prec = resolve_con( name, "CON", opcode, records );
      if (prec && Long_val(Field(*prec,Field_arity)) != (long)opcode[1]) {
        raise_module( name, "size doesn't match arity in CON instruction" );
      }
      break;
    }

    case CALL: {
      value* prec = resolve_rec( name, "CALL", opcode, records, Rec_extern );

      if (opcode[1]+1 != (opcode_t)strlen(Type_extern(*prec))) {
        raise_module( name, "type doesn't match number of arguments in external call \"%s\"",
                      Name_field(*prec,Field_name));
      }

      break;
    }

    case PUSHBYTES: {
      resolve_rec( name, "PUSHBYTES", opcode, records, Rec_bytes );
      break;
    }

    case PUSHCODE: {
      value* prec = resolve_index( name, "PUSHCODE", opcode, records, Rec_value );
      if (Long_val(Field(*prec,Field_arity)) == 0) {
        Set_instr(opcode[-1],PUSHCAF);
        fixup_ptr(name,opcode,(void*)prec);
      } else {
        fixup_code(name,opcode,*prec);
      }

      break;
    }

    case ENTERCODE: {
      value* prec = resolve_index( name, "ENTERCODE", opcode, records, Rec_value );
      fixup_code( name, opcode, *prec );
      break;
    }

    case RETURNINT:
    case PUSHINT:
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
      break;
    }

    default: {
      break;
    }
    } /* switch */

    opcode += instr_arg_count(instr);
  }

  CAMLreturn0;
}
