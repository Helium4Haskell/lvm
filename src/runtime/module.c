/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>  /* snprintf */
#include <string.h>
#include "mlvalues.h"
#include "memory.h"
#include "module.h"
#include "bytes.h"


value find_code( value module, const char* codename )
{
  value consts = Constants_module(module);
  unsigned i;

  for( i = 1; i <= Wosize_constants(consts); i++)
  {
    value v = Constant(consts,i);
    if (Is_block(v) && (Tag_val(v) == Const_value))
    {
      /* TODO: check for public flag ? */
      const char* name = Name_field(v,Field_name);
      if (strcmp(codename,name) == 0)
      {
        return Field(v,Field_fixup);
      }
    }
  }

  return 0;
}


value find_module( value module, const char* modname )
{
  CAMLparam1(module);
  CAMLlocal1(mod);

  mod = module;
  do{
    if (strcmp( String_val(Field(mod,Module_name)), modname ) == 0) {
      CAMLreturn(mod);
    }
    mod = Field(mod,Module_next);
  } while (mod != module);

  CAMLreturn(0);
}

value find_qualified_code( value module, const char* modname, const char* name )
{
  value mod = find_module(module,modname);
  if (mod != 0) return find_code( mod, name );
           else return 0;
}

bool is_code_val( value module, value valpc )
{
  CAMLparam2(module,valpc);
  CAMLlocal1(mod);
  opcode_t* pc;

  /* caf & inv tags */
  if (Is_heap_val(valpc)) {
    if (Tag_val(valpc) == Caf_tag) {
      CAMLreturn(true);
    } else if (Tag_val(valpc) == Inv_tag) { /* TODO: shaky */
      CAMLreturn(is_code_val(module,Field(valpc,0)));
    } else {
      CAMLreturn(false);
    }
  }

  pc = Code_val(valpc);

  /* find the associated module */
  mod = module;
  do{
    char* pcode = Bytes_val(Field(mod,Module_code));
    if ((char*)pc >= pcode && (char*)pc <= pcode + Long_val(Field(mod,Module_code_len)))
      break;
    mod = Field(mod,Module_next);
    if (mod == module) mod = 0;
  } while (mod != 0);

  CAMLreturn(mod != 0);
}

static void find_decl_of_code( value module, value valpc, value* _module, value* _decl, nat* _ofs )
{
  CAMLparam2(module,valpc);
  CAMLlocal3(mod,decl,consts);
  opcode_t* pc;
  nat       idx;
  nat       ofs = 0;

  Assert(is_code_val(module,valpc));

  /* caf & inv tags */
  if (Is_heap_val(valpc)) {
    if ((Tag_val(valpc) == Caf_tag) || (Tag_val(valpc) == Inv_tag /* TODO: shaky */)) {
      find_decl_of_code( module, Field(valpc,0), _module, _decl, _ofs );
      CAMLreturn0;
    }
  }

  pc = Code_val(valpc);
  /* find the associated module */
  mod = module;
  do{
    char* pcode = Bytes_val(Field(mod,Module_code));
    if ((char*)pc >= pcode && (char*)pc <= pcode + Long_val(Field(mod,Module_code_len)))
      break;
    mod = Field(mod,Module_next);
    if (mod == module) mod = 0;
  } while (mod != 0);
  Assert(mod != 0);

  /* find the beginning of the code block */
  consts = Constants_module(mod);
  for( idx = 1; idx <= Wosize_constants(consts); idx++) {
    decl = Constant(consts,idx);
    if (Tag_val(decl) == Const_value) {
      opcode_t* pcode;
      long      len;
      pcode = Code_val(Field(decl,Field_code));
      len = Wosize_val((value)pcode); /* in bytes! */
      if ((char*)pc >= (char*)pcode && (char*)pc <= (char*)pcode+len) {
        ofs = (char*)pc - (char*)pcode;
        break;
      }
    }
  }
  Assert(idx <= Wosize_constants(consts));
  Assert(decl != 0 && Tag_val(decl) == Const_value);

  if (_module) *_module = mod;
  if (_decl)   *_decl   = decl;
  if (_ofs)    *_ofs    = ofs;

  CAMLreturn0;
}

static void format_name( char* buf, nat max, value module, value decl, nat ofs )
{
  CAMLparam2(module,decl);
  CAMLlocal1(declEnc);

  const char* module_name;
  const char* enc_name;
  const char* name;

  /* get the name */
  declEnc = decl;
  while (   (Tag_val(declEnc) == Const_value)
         && Is_block(Field(declEnc,Field_enclosing)))
    declEnc = Field(declEnc,Field_enclosing);


  module_name = String_val(Field(module,Module_name));
  enc_name    = Name_field(declEnc,Field_name);
  name        = Name_field(decl,Field_name);

  if (declEnc != decl) {
    /* if (ofs != 0) snprintf( buf, max, "%s.%s(%s):%li", module_name, enc_name, name, ofs );
             else */
    snprintf( buf, max, "%s.%s(%s)", module_name, enc_name, name );
  } else {
    /* if (ofs != 0) snprintf( buf, max, "%s.%s:%li", module_name, name, ofs );
             else */
    snprintf( buf, max, "%s.%s", module_name, name);
  }


  CAMLreturn0;
}



const char* find_name_of_code( value module, value valpc )
{
  CAMLparam2(module,valpc);
  CAMLlocal3(mod,decl,declEnc);
  static char name[MAXSTR];
  nat ofs;

  find_decl_of_code( module, valpc, &mod, &decl, &ofs );
  format_name( name, MAXSTR, mod, decl, ofs );

  CAMLreturn(name);
}

const char* find_type_of_code( value module, value valpc )
{
  CAMLparam2(module,valpc);
  CAMLlocal3(mod,decl,declEnc);
  static char name[MAXSTR];
  nat ofs;

  find_decl_of_code( module, valpc, &mod, &decl, &ofs );
  if (Field(decl,Field_type) != Val_long(0)) {
    strncpy(name,Type_field(decl,Field_type),MAXSTR);
  } else {
    name[0] = '\0';
  }

  CAMLreturn(name);
}

long find_arity_of_code( value module, value pc )
{
  CAMLparam2(module,pc);
  CAMLlocal1(decl);

  find_decl_of_code( module, pc, NULL, &decl, NULL );
  if (Tag_val(decl) == Const_value)
    CAMLreturn(Long_val(Field(decl,Field_arity)));
  else
    CAMLreturn(0);
}
