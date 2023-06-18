
/******************************************************************************
 * MODULE     : object_l3.cpp
 * DESCRIPTION: Implementation of scheme objects (L3)
 * COPYRIGHT  : (C) 1999-2011 Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "object_l3.hpp"
#include "object.hpp"

bool
tmscm_is_patch (tmscm p) {
  return (tmscm_is_blackbox (p) &&
          (type_box (tmscm_to_blackbox (p)) == type_helper<patch>::id)) ||
         (tmscm_is_string (p));
}

tmscm
patch_to_tmscm (patch p) {
  return blackbox_to_tmscm (close_box<patch> (p));
}

patch
tmscm_to_patch (tmscm obj) {
  return open_box<patch> (tmscm_to_blackbox (obj));
}

bool
tmscm_is_array_patch (tmscm p) {
  if (tmscm_is_null (p)) return true;
  else
    return tmscm_is_pair (p) && tmscm_is_patch (tmscm_car (p)) &&
           tmscm_is_array_patch (tmscm_cdr (p));
}

tmscm
array_patch_to_tmscm (array<patch> a) {
  int   i, n= N (a);
  tmscm p= tmscm_null ();
  for (i= n - 1; i >= 0; i--)
    p= tmscm_cons (patch_to_tmscm (a[i]), p);
  return p;
}

array<patch>
tmscm_to_array_patch (tmscm p) {
  array<patch> a;
  while (!tmscm_is_null (p)) {
    a << tmscm_to_patch (tmscm_car (p));
    p= tmscm_cdr (p);
  }
  return a;
}
