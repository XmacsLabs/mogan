
/******************************************************************************
 * MODULE     : object_l2.cpp
 * DESCRIPTION: Implementation of scheme objects (L2)
 * COPYRIGHT  : (C) 1999-2011 Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "object_l2.hpp"
#include "object.hpp"

/******************************************************************************
 * Urls
 ******************************************************************************/

bool
tmscm_is_url (tmscm u) {
  return (tmscm_is_blackbox (u) &&
          (type_box (tmscm_to_blackbox (u)) == type_helper<url>::id)) ||
         (tmscm_is_string (u));
}

tmscm
url_to_tmscm (url u) {
  return blackbox_to_tmscm (close_box<url> (u));
}

url
tmscm_to_url (tmscm obj) {
  if (tmscm_is_string (obj))
#ifdef OS_MINGW
    return url_system (tmscm_to_string (obj));
#else
    return tmscm_to_string (obj);
#endif
  return open_box<url> (tmscm_to_blackbox (obj));
}

url
as_url (object obj) {
  tmscm t= object_to_tmscm (obj);
  if (!tmscm_is_url (t)) return url ("");
  return tmscm_to_url (t);
}

bool
is_url (object obj) {
  return tmscm_is_url (object_to_tmscm (obj));
}

bool
tmscm_is_array_url (tmscm p) {
  if (tmscm_is_null (p)) return true;
  else
    return tmscm_is_pair (p) && tmscm_is_url (tmscm_car (p)) &&
           tmscm_is_array_url (tmscm_cdr (p));
}

tmscm
array_url_to_tmscm (array<url> a) {
  int   i, n= N (a);
  tmscm p= tmscm_null ();
  for (i= n - 1; i >= 0; i--)
    p= tmscm_cons (url_to_tmscm (a[i]), p);
  return p;
}

array<url>
tmscm_to_array_url (tmscm p) {
  array<url> a;
  while (!tmscm_is_null (p)) {
    a << tmscm_to_url (tmscm_car (p));
    p= tmscm_cdr (p);
  }
  return a;
}
