/*-----------------------------------------------------------------------
  The Lazy Virtual Machine.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file is
  distributed under the terms of the GNU Library General Public License.
-----------------------------------------------------------------------*/

/* $Id$ */

#ifndef _instr_
#define _instr_

#define INSTRLIST       \
  Ins( ARGCHK, 1 ),     \
  Ins( PUSHCODE, 1 ),   \
  Ins( PUSHCONT, 1 ),   \
  Ins( PUSHVAR, 1 ),    \
  Ins( PUSHINT, 1 ),    \
  Ins( PUSHDOUBLE, 2 ), \
  Ins( PUSHSTRING, 1 ), \
  Ins( SLIDE, 2 ),      \
  Ins( STUB, 1 ),       \
  \
  Ins( ALLOCAP, 1 ),    \
  Ins( PACKAP, 2 ),     \
  Ins( PACKNAP, 2 ),    \
  Ins( NEWAP, 1),       \
  Ins( NEWNAP, 1 ),     \
  \
  Ins( ENTER, 0 ),      \
  Ins( RETURN, 0 ),     \
  Ins( CATCH, 0 ),      \
  Ins( RAISE, 0 ),      \
  Ins( CALL, 2 ),       \
  \
  Ins( ALLOCCON, 2 ),   \
  Ins( PACKCON, 2 ),    \
  Ins( NEWCON, 2),      \
  Ins( UNPACKCON, 1),   \
  Ins( TESTCON, 2 ),    \
  \
  Ins( TESTINT, 2),     \
  Ins( ADDINT, 0),      \
  Ins( SUBINT, 0 ),     \
  Ins( MULINT, 0 ),     \
  Ins( DIVINT, 0 ),     \
  Ins( MODINT, 0 ),     \
  Ins( QUOTINT, 0 ),    \
  Ins( REMINT, 0 ),     \
  \
  Ins( ANDINT, 0 ),     \
  Ins( XORINT, 0 ),     \
  Ins( ORINT, 0 ),      \
  Ins( SHRINT, 0 ),     \
  Ins( SHLINT, 0 ),     \
  Ins( SHRNAT, 0 ),     \
  \
  Ins( NEGINT, 0 ),     \
  \
  Ins( EQINT, 0 ),      \
  Ins( NEINT, 0 ),      \
  Ins( LTINT, 0 ),      \
  Ins( GTINT, 0 ),      \
  Ins( LEINT, 0 ),      \
  Ins( GEINT, 0 ),      \
  \
  Ins( ALLOC, 0 ),      \
  Ins( NEW, 1 ),        \
  Ins( GETFIELD, 0 ),   \
  Ins( SETFIELD, 0 ),   \
  Ins( GETTAG, 0 ),     \
  Ins( GETSIZE, 0 ),    \
  Ins( PACK, 1 ),       \
  Ins( UNPACK, 1 ),     \
  \
  Ins( PUSHVAR0, 0 ),   \
  Ins( PUSHVAR1, 0 ),   \
  Ins( PUSHVAR2, 0 ),   \
  Ins( PUSHVAR3, 0 ),   \
  Ins( PUSHVAR4, 0 ),   \
  \
  Ins( PUSHVARS2, 2 ),  \
  Ins( PUSHVARS3, 3 ),  \
  Ins( PUSHVARS4, 4 ),  \
  \
  Ins( NEWAP1, 0 ),     \
  Ins( NEWAP2, 0 ),     \
  Ins( NEWAP3, 0 ),     \
  Ins( NEWAP4, 0 ),     \
  \
  Ins( NEWNAP1, 0 ),    \
  Ins( NEWNAP2, 0 ),    \
  Ins( NEWNAP3, 0 ),    \
  Ins( NEWNAP4, 0 ),    \
  \
  Ins( NEWCON0, 1 ),    \
  Ins( NEWCON1, 1 ),    \
  Ins( NEWCON2, 1 ),    \
  Ins( NEWCON3, 1 ),    \
  \
  Ins( ENTERCODE, 1 ),  \
  Ins( EVALVAR, 1 ),    \
  Ins( RETURNCON, 2 ),  \
  Ins( RETURNINT, 1 ),  \
  Ins( RETURNCON0, 1 ), \
  \
  Ins( MATCHCON, 2 ),   \
  Ins( SWITCHCON, 2 ),  \
  Ins( MATCHINT, 2 ),   \
  Ins( PUSHEAGER, 1 ),  \
  Ins( INCINT, 1 ),     \
  \
  /* internal instructions */ \
  Ins( PUSHCAF, 1 ),    \
  Ins( STOP, 0 )

#define Ins(i,n)  i
enum instruction { INSTRLIST };
#undef Ins

extern int instr_arg_count( enum instruction instr );
extern char* instr_name( enum instruction instr );

#endif /* _instr_ */


