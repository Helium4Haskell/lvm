/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/***---------------------------------------------------------------------
  Modified and adapted for the Lazy Virtual Machine by Daan Leijen.
  Modifications copyright 2001, Daan Leijen. This (modified) file is
  distributed under the terms of the GNU Library General Public License.
---------------------------------------------------------------------***/

/* $Id$ */

#include <stdio.h>
#include <string.h>

/* Check for the availability of "__int64" type as in Microsoft Visual C++ */

int main(int argc, char **argv)
{
  __int64 l;
  unsigned __int64 u;
  char buffer[64];

  if (sizeof(__int64) == 8) {
    l = 123456789123456789I64;
    buffer[0] = 0;
    sprintf(buffer, "%I64d", l);
    if (strcmp(buffer, "123456789123456789") == 0) return 1;
    return 0;
  }
  else
    return 100;
}
