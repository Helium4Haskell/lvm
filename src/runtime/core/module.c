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


/*---------------------------------------------------------
  find_code & find_module by name
---------------------------------------------------------*/
value find_code( value module, const char* codename )
{
  value records = Records_module(module);
  unsigned i;

  for( i = 1; i <= Count_records(records); i++)
  {
    value v = Record(records,i);
    if (Is_block(v) && (Tag_val(v) == Rec_value))
    {
      /* TODO: check for public flag ? */
      const char* name = Name_field(v,Field_name);
      if (strcmp(codename,name) == 0)
      {
        return Field(v,Field_value_fun);
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
    if (strcmp( Name_field(Field(mod,Module_info),Field_module_name), modname ) == 0) {
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

/*---------------------------------------------------------
  find value declaration from pc
---------------------------------------------------------*/
static void find_decl_of_code_in_module( value module, opcode_t* pc, value* _rec, long* _ofs )
{
  CAMLparam1(module);
  CAMLlocal3(records,rec,code);
  wsize_t i;

  if (_rec) *_rec = 0;
  if (_ofs) *_ofs = 0;

  /* walk through all records */
  records = Records_module(module);
  for( i = 1; i <= Count_records(records); i++ )
  {
    rec = Record(records,i);
    if (Tag_val(rec) == Rec_value) {
      wsize_t len;
      code = Code_value(rec);
      len  = Wosize_val(code); /* in bytes! */
      if ((char*)pc >= (char*)code && (char*)pc <= (char*)code + len) {
        /* found */
        if (_rec) *_rec = rec;
        if (_ofs) *_ofs = (char*)pc - (char*)code;
        CAMLreturn0;
      }
    }
  }

  CAMLreturn0;
}

static void find_decl_of_code( value module, value valpc, value* _module, value* _rec, long* _ofs )
{
  CAMLparam2(module,valpc);
  CAMLlocal1(mod);
  opcode_t* pc;

  Assert(_rec != NULL);

  if (_module) *_module  = 0;
  if (_ofs)    *_ofs     = 0;
  *_rec = 0;

  /* caf & inv tags */
  if (is_heap_val(valpc)) {
    if ((Tag_val(valpc) == Caf_tag) || (Tag_val(valpc) == Inv_tag /* TODO: shaky */)) {
      find_decl_of_code( module, Field(valpc,0), _module, _rec, _ofs );
      CAMLreturn0;
    }
  }

  pc = Code_val(valpc);


  /* walk through all known modules & records */
  mod = module;
  do{
    find_decl_of_code_in_module( mod, pc, _rec, _ofs  );
    if (*_rec != 0) {
      /* found */
      if (_module) *_module = mod;
      CAMLreturn0;
    }
    else {
      mod = Field(mod,Module_next);
    }
  } while (mod != module);

  CAMLreturn0;
}

bool is_code_val( value module, value valpc )
{
  CAMLparam2(module,valpc);
  CAMLlocal1(rec);
  find_decl_of_code( module, valpc, NULL, &rec, NULL );
  CAMLreturn(rec != 0);
}


/*---------------------------------------------------------
    find the name of the code belonging to a pc
---------------------------------------------------------*/
static void format_name( char* buf, wsize_t max, value module, value rec, long ofs )
{
  CAMLparam2(module,rec);
  CAMLlocal1(recEnc);

  const char* module_name;
  const char* enc_name;
  const char* name;

  /* valid data? */
  if (module == 0 || rec == 0) {
    snprintf( buf,max, "<unknown>" );
    CAMLreturn0;
  }

  /* get the name */
  recEnc = rec;
  while (   (Tag_val(recEnc) == Rec_value)
         && Field(recEnc,Field_value_enc) != Val_long(0))
    recEnc = Field(recEnc,Field_value_enc);


  module_name = Name_field(Field(module,Module_info),Field_module_name);
  enc_name    = Name_field(recEnc,Field_name);
  name        = Name_field(rec,Field_name);

  if (recEnc != rec) {
    /* if (ofs != 0) snprintf( buf, max, "%s.%s(%s):%li", module_name, enc_name, name, ofs );
             else */
    snprintf( buf, max, "%s.%s (%s)", module_name, enc_name, name );
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
  long ofs;

  find_decl_of_code( module, valpc, &mod, &decl, &ofs );
  format_name( name, MAXSTR, mod, decl, ofs );

  CAMLreturn(name);
}


long find_arity_of_code( value module, value pc )
{
  CAMLparam2(module,pc);
  CAMLlocal1(rec);

  find_decl_of_code( module, pc, NULL, &rec, NULL);
  if (rec != 0 && Tag_val(rec) == Rec_value)
    CAMLreturn(Long_val(Field(rec,Field_arity)));
  else
    CAMLreturn(0);
}