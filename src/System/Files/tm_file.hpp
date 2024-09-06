
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
#include "tm_url.hpp"
#include "analyze.hpp"
#include "file.hpp"


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
void append_to (url what, url to);

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
