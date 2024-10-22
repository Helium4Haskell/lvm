/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _module_
#define _module_

#include "fixed.h"

#define Module_tag              0
struct module_header_t
{
  word_t magic,
         header_length,
         total_length,
         major_version,
         minor_version,
         records_count,
         records_length,
         module_idx;
};

struct module_footer_t
{
  word_t magic,
         footer_length,
         total_length;
};

#define Magic_header 0x1F4C564D
#define Magic_footer 0x1E4C564D

/*----------------------------------------------------------------------
  MODULES
  A module contains:
  records    - a fixed n-tuple that contains all constants. It is fixed
               because a PUSHCAF and CALL points into this block.
  info       - points to a module record
----------------------------------------------------------------------*/
enum module_fields {
  Module_next,
  Module_fname,
  Module_info,
  Module_records,

  Module_size
};


#define Records_module(m)        Field(m,Module_records)
#define Wosize_records(c)        Wosize_fixed(c)
#define Count_records(c)         Wosize_records(c)
#define Record(c,idx)            Field_fixed(c,idx-1)
#define Record_module(m,idx)     Record(Records_module(m),idx)

/* deprecated: for evolutionary code change */
#define Constants_module(m)      Records_module(m)
#define Wosize_constants(c)      Wosize_records(c)
#define Constant(c,idx)          Record(c,idx)
#define Constant_module(m,idx)   Record_module(m,idx)


/*----------------------------------------------------------------------
  RECORDS
  The kind of the record is also its tag value
----------------------------------------------------------------------*/
enum rec_kind {
  Rec_name,
  Rec_kind,
  Rec_bytes,
  Rec_code,
  Rec_value,
  Rec_con,
  Rec_import,
  Rec_module,
  Rec_extern,
  Rec_extern_type,
  Rec_custom,

  Rec_last = Rec_extern_type
};

#define Is_caf_val(v)  (Tag_val(v) == Rec_value && Long_val(Field(v,Field_arity)) == 0)

/*----------------------------------------------------------------------
  The offsets of fields inside the record declarations
----------------------------------------------------------------------*/
enum rec_fields {
  Field_name = 0,
  Field_flags,
  Field_arity,

  Field_value_name  = 0,
  Field_value_flags,
  Field_value_arity,
  Field_value_enc,
  Field_value_code,     /* points to the Rec_code */
  Field_value_fun,      /* points to either the Code or Caf value */
  Rec_value_size,

  Field_con_tag     = Field_arity+1,
  Rec_con_size,

  Field_extern_type   = Field_arity+1,
  Field_extern_module,
  Field_extern_name,
  Field_extern_nameflag,
  Field_extern_link,
  Field_extern_call,
  Field_extern_fun,       /* the real address: [void*] */
  Field_extern_symbol,    /* a possible [Symbol] custom block, containing a handle and the real address */
  Rec_extern_size,

  Field_import_module = Field_flags+1,
  Field_import_name,
  Field_import_iscustom,
  Field_import_kind,      /* custom kind ptr or kind int */
  Field_import_fixup,
  Rec_import_size,

  Field_module_name = 0,
  Field_module_major,
  Field_module_minor,
  Rec_module_size,

  Field_name_string = 0,
  Rec_name_size,

  Field_extern_type_string = 0,
  Rec_extern_type_size,

  Field_bytes_string = 0,
  Rec_bytes_size,

  Field_code_code = 0,   /* points to a bytes block containing the instructions (as Code_tag) */
  Rec_code_size,

  Field_kind_string = 0,
  Rec_kind_size,

  Field_custom_kind = Field_flags + 1,
  Rec_custom_size
};

enum link_mode {
  Link_static,
  Link_dynamic,
  Link_runtime
};

enum call_conv {
  Call_c,
  Call_std
};

enum name_flag {
  Name_plain,
  Name_decorate,
  Name_ordinal
};

enum decl_flags {
  Flag_public = 0x01
};


/*----------------------------------------------------------------------
  Find code and names
----------------------------------------------------------------------*/
value       find_code( value module, const char* name );
value       find_qualified_code( value module, const char* modname, const char* name );
long        find_arity_of_code( value module, value pc );
const char* find_name_of_code( value module, value code );
bool        is_code_val( value module, value pc );


#define Type_extern(rec)            (String_val(Field(Field(rec,Field_extern_type),Field_extern_type_string)))
#define Name_field(decl,fld)        (String_val(Field(Field(decl,fld),Field_name_string)))
#define Name_module_field(decl,fld) Name_field(Field(decl,fld),Field_module_name)
#define Name_module(mod)            Name_module_field(mod,Module_info)

/* from a Rec_code record to the value that points to the Code_tag block */
#define Code_code(rec)  (Val_hp(Bytes_val(Field(rec,Field_code_code))))
#define Code_value(rec) (Code_code(Field(rec,Field_value_code)))
#endif
