
/******************************************************************************
 * MODULE     : git.hpp
 * DESCRIPTION: Interface with Git
 * COPYRIGHT  : (C) 2024    Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef PLUGIN_GIT_HPP
#define PLUGIN_GIT_HPP

#include "string.hpp"
#include "url.hpp"

string libgit2_version ();
string git_load_blob (string rev, url u, url repo_u= url_none ());
string git_status_file (url file_u, url repo_u);

#endif
