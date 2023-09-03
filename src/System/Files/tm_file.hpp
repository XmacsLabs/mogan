
/******************************************************************************
* MODULE     : tm_file.hpp
* DESCRIPTION: file handling for TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_FILE_H
#define TM_FILE_H

#include "string.hpp"
#include "url.hpp"

void make_dir (url which);
url  url_temp_dir ();
url  url_temp_dir_sub ();
url  url_temp (string suffix= "");

bool is_of_type (url name, string filter);
int  file_size (url u);
int  last_modified (url u, bool cache_flag= true);

url  url_numbered (url dir, string prefix, string postfix, int i=1);
url  url_scratch (string prefix="no_name_", string postfix=".tm", int i=1);
bool is_scratch (url u);
string file_format (url u);

url search_sub_dirs (url root);
array<string> file_completions (url search, url dir);

url grep (string what, url u);
url search_file_in (url u, string name);
url search_file_upwards (url u, string name, array<string> stops);

int search_score (url u, array<string> a);

#define CMD_GET_FROM_WEB    1
#define CMD_GET_FROM_SERVER 2
#define CMD_APPLY_EFFECT    3
url make_file (int cmd, tree data, array<url> args);

#endif
