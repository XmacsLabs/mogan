
/******************************************************************************
 * MODULE     : tmfs_url.hpp
 * DESCRIPTION: tmfs url
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef LOLLY_TMFS_URL
#define LOLLY_TMFS_URL

#include "string.hpp"
#include "url.hpp"

class tmfs_url : public url {
public:
  tmfs_url (const char* name);
  tmfs_url (string name);
};

bool is_root_tmfs (url u);
bool is_rooted_tmfs (url u);
bool is_rooted_tmfs (url u, string sub_protocol);

#endif
