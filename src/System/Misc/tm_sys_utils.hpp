
/******************************************************************************
* MODULE     : tm_sys_utils.hpp
* DESCRIPTION: system utilities for TeXmacs
* COPYRIGHT  : (C) 1999-2016  Joris van der Hoeven, Denis Raux
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_SYS_UTILS_H
#define TM_SYS_UTILS_H

#include "string.hpp"
#include "array.hpp"
#include "url.hpp"

extern int script_status; // 0: never accept, 1: prompt, 2: always accept

string get_stacktrace (unsigned int max_frames= 127);
string get_current_cpu_arch ();
string get_pretty_os_name ();

int    system (string s, string &r);
int    system (string s, string &r, string& e);

string eval_system (string s);
string var_eval_system (string s);

url get_texmacs_path ();
url get_texmacs_home_path ();
url get_tm_cache_path ();

string get_printing_default ();
bool has_printing_cmd (void);
string get_printing_cmd (void);
void set_printing_cmd (string cmd);

const char* default_look_and_feel ();

#endif
