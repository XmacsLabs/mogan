
/******************************************************************************
 * MODULE     : url.hpp
 * DESCRIPTION: unified resource location handling
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TM_URL_H
#define TM_URL_H

#include "tm_debug.hpp"
#include "tree.hpp"
#include "url.hpp"

bool is_secure (url u); // is u secure?

/******************************************************************************
 * url resolution
 ******************************************************************************/

bool   url_test (url u, string filter);
url    complete (url u, string filter= "fr"); // wildcard completion
url    resolve (url u, string filter= "fr");  // find first match only
url    resolve_in_path (url u);               // find file in path
bool   exists (url u);                        // file exists
bool   exists_in_path (url u);                // file exists in path
bool   has_permission (url u, string filter); // check file permissions
url    descendance (url u);                   // utility for style&package menus
url    concretize_url (url u);                // variant of concretize below
string concretize (url u);                    // system name for resolved url
string materialize (url u, string f= "fr");   // resolve + concretize

#endif // defined URL_H
