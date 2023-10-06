
/******************************************************************************
 * MODULE     : tmfs_url.cpp
 * DESCRIPTION: url rooted with tmfs://
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *                  2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tmfs_url.hpp"

tmfs_url::tmfs_url (string name)
    : url (url_root ("tmfs") * url_get_name (name)) {}

tmfs_url::tmfs_url (const char* name)
    : url (url_root ("tmfs") * url_get_name (string (name))) {}

bool
is_tmfs_protocol (url u, string protocol) {
  return u->t == protocol ||
         (is_concat (u) && is_tmfs_protocol (u[1], protocol));
}

bool
is_rooted_tmfs (url u, string protocol) {
  return (is_concat (u) && is_root_tmfs (u[1]) &&
          is_tmfs_protocol (u[2], protocol)) ||
         (is_or (u) && is_rooted_tmfs (u[1], protocol) &&
          is_rooted_tmfs (u[2], protocol));
}

bool
is_root_tmfs (url u) {
  return is_root (u, "tmfs");
}

bool
is_rooted_tmfs (url u) {
  return is_root_tmfs (u) || (is_concat (u) && is_rooted_tmfs (u[1])) ||
         (is_or (u) && is_rooted_tmfs (u[1]) && is_rooted_tmfs (u[2]));
}
