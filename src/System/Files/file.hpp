
/******************************************************************************
* MODULE     : file.hpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LOLLY_FILE_H
#define LOLLY_FILE_H

#include "url.hpp"
#include "tm_url.hpp"
#include "sys_utils.hpp"
#include "tm_sys_utils.hpp"
#include "analyze.hpp"


bool is_regular (url name);
bool is_directory (url name);
bool is_symbolic_link (url name);
bool is_newer (url which, url than);
url  url_temp_dir ();
url  url_temp_dir_sub ();
url  url_temp (string suffix= "");

array<string> read_directory (url name, bool& error_flag);

void move (url from, url to);
void copy (url from, url to);
void remove (url what);
void mkdir (url dir);
void make_dir (url which);
void rmdir (url what);
void change_mode (url u, int mode);


#endif // defined FILE_H
