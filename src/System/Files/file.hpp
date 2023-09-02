
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

/**
 * Load the url to a string, and return a boolean indicator
 * @param u the url to load
 * @param s the loaded string
 * @return If there are errors, return true, else, return false
 */
bool load_string (url file_name, string& s, bool fatal);
string string_load (url u);

bool save_string (url file_name, string s, bool fatal=false);
void string_save (string s, url u);

bool append_string (url u, string s, bool fatal= false);
void string_append_to_file (string s, url u);

bool is_regular (url name);
bool is_directory (url name);
bool is_symbolic_link (url name);
bool is_newer (url which, url than);
url  url_temp_dir ();
url  url_temp_dir_sub ();
url  url_temp (string suffix= "");

array<string> read_directory (url name, bool& error_flag);

inline string sys_concretize (url u1) {
  return escape_sh (concretize (u1)); }

inline void system (string which, url u1) {
  lolly::system (which * " " * sys_concretize (u1)); }
inline void system (string which, url u1, url u2) {
  lolly::system (which * " " * sys_concretize (u1) * " " * sys_concretize (u2)); }
inline void system (string which, url u1, const char* post) {
  lolly::system (which * " " * sys_concretize (u1) * " " * post); }
inline void system (string which, url u1, const char* sep, url u2) {
  lolly::system (which * " " * sys_concretize (u1) * " " * sep *
	          " " * sys_concretize (u2)); }
inline string eval_system (string which, url u1) {
  return eval_system (which * " " * escape_sh (concretize (u1))); }
inline string eval_system (string which, url u1, url u2) {
  return eval_system (which * " " * escape_sh (concretize (u1)) * " " * escape_sh (concretize (u2))); }

void move (url from, url to);
void copy (url from, url to);
void remove (url what);
void append_to (url what, url to);
void mkdir (url dir);
void make_dir (url which);
void rmdir (url what);
void change_mode (url u, int mode);


#endif // defined FILE_H
