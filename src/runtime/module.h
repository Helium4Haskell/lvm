/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _module_
#define _module_

#include "mlvalues.h"
#include "fixed.h"

typedef opcode_t  word_t;

#define Module_tag              0
struct module_header_t
{
  word_t magic,
         total_length,
         header_length,
         major_version,
         minor_version,
         module_major_version,
         module_minor_version,
         module_name,
         constant_count,
         constant_length,
         decl_count,
         decl_length;
};

/*----------------------------------------------------------------------
  MODULES
  A module contains:
  constants  - a fixed n-tuple that contains all constants
  code       - a statically allocated block that contains all the code
               the operand of a PUSHCODE instruction points into this block
  cafs       - a statically allocated block that contains pointers to all
               heap alloced CAF nodes. the operand of a PUSHCAF instruction
               points into this block

  both [cafs] and [code] are actually custom blocks that contain a pointer
  to the statically allocated block. This allows those blocks to be released
  automatically when gc'ed.
----------------------------------------------------------------------*/
enum module_fields {
  Module_next,
  Module_name,
  Module_fname,
  Module_major,
  Module_minor,
  Module_constants,
  Module_code_count,
  Module_code_len,
  Module_code,
  Module_size
};


#define Constants_module(m)      Field(m,Module_constants)
#define Wosize_constants(c)      Wosize_fixed(c)
#define Constant(c,idx)          Field_fixed(c,idx-1)
#define Constant_module(m,idx)   Constant(Constants_module(m),idx)


/*----------------------------------------------------------------------
  CONSTANTS
  The kind of the constants is also its tag value
----------------------------------------------------------------------*/
enum const_kind {
  Const_name,
  Const_type,
  Const_string,
  Const_module,
  Const_value,
  Const_con,
  Const_extern,
  Const_import,
  Const_kind,
  Const_data,
  Const_typedecl,

  Const_last = Const_import
};

#define Is_caf_val(v)  (Tag_val(v) == Const_value && Long_val(Field(v,Field_arity)) == 0)
/*----------------------------------------------------------------------
  The offsets of fields inside the constant declarations
----------------------------------------------------------------------*/
enum const_fields {
  Field_fixup = 0,
  Field_name,
  Field_type,
  Field_flags,
  Field_custom,
  Field_arity,
  Field_enclosing,
  Field_code,

  Const_value_size  = Field_code+1,

  Field_con_tag     = Field_arity+1,
  Const_con_size,

  Field_name_string = 0,
  Const_name_size,

  Field_type_string = 0,
  Const_type_size,

  Field_string_string = 0,
  Const_string_size,

  Field_module_name = 0,
  Field_module_major,
  Field_module_minor,
  Const_module_size,

  Field_import_module = Field_custom+1,
  Field_import_name,
  Field_import_const_kind,
  Const_import_size,

  Field_extern_module = Field_arity+1,
  Field_extern_name,
  Field_extern_nameflag,
  Field_extern_link,
  Field_extern_call,
  Const_extern_size,

  Field_data_kind = Field_type,
  Const_data_size = Field_arity+1,

  Const_typedecl_size = Field_arity+1
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
const char* find_type_of_code( value module, value valpc );
bool        is_code_val( value module, value pc );


#define Name_field(decl,fld) (String_val(Field(Field(decl,fld),Field_name_string)))
#define Type_field(decl,fld) (String_val(Field(Field(decl,fld),Field_type_string)))
#define Name_module_field(decl,fld) Name_field(Field(decl,fld),Field_module_name)
#endif
