
/******************************************************************************
* MODULE     : file.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
                   2023  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "analyze.hpp"
#include "string.hpp"
#include "sys_utils.hpp"

#include "tbox/tbox.h"

static bool
is_single_path (url u) {
  string label= u.label ();
  return (label == "") || (label == "concat");
}

bool
is_directory (url u) {
  if (!is_single_path (u)) return false;

  string         path= as_string (u);
  tb_file_info_t info;
  if (tb_file_info (as_charp (path), &info)) {
    switch (info.type) {
    case TB_FILE_TYPE_DIRECTORY:
      return true;
    default:
      return false;
    }
  }
  else {
    return false;
  }
}

bool
is_regular (url u) {
  if (!is_single_path (u)) return false;

  string         path= as_string (u);
  tb_file_info_t info;
  if (tb_file_info (as_charp (path), &info)) {
    switch (info.type) {
    case TB_FILE_TYPE_FILE:
      return true;
    default:
      return false;
    }
  }
  else {
    return false;
  }
}

bool
is_symbolic_link (url u) {
  if (!is_single_path (u)) return false;

  string         path= as_string (u);
  tb_file_info_t info;
  if (tb_file_info (as_charp (path), &info)) {
    return (info.flags & TB_FILE_FLAG_LINK) != 0;
  }
  else {
    return false;
  }
}

bool
is_newer (url which, url than) {
  if (!is_single_path (which)) return false;
  if (!is_single_path (than)) return false;

  tb_file_info_t info1, info2;
  if (tb_file_info (as_charp (as_string (which)), &info1) &
      tb_file_info (as_charp (as_string (than)), &info2)) {
    return info1.mtime > info2.mtime;
  }
  else {
    return false;
  }
}

bool
is_of_type (url name, string filter) {
  if (is_ramdisc (name)) return true;

  if (!is_single_path (name)) return false;

  if (filter == "") return true;
  int i, n= N (filter);

  // Normal files
#if defined(OS_MINGW) || defined(OS_WIN)
  string suf;
  if (filter == "x") {
    suf= suffix (name);
    if ((suf != "exe") && (suf != "bat") && (suf != "com")) {
      name= glue (name, ".exe");
      suf = "exe";
    }
  }
#endif
  string         path= as_string (name);
  tb_file_info_t info;
  if (!tb_file_info (as_charp (path), &info)) {
    return false;
  }
  for (i= 0; i < n; i++)
    switch (filter[i]) {
      // FIXME: should check user id and group id for r, w and x
    case 'f':
      if (info.type != TB_FILE_TYPE_FILE) return false;
      break;
    case 'd':
      if (info.type != TB_FILE_TYPE_DIRECTORY) return false;
      break;
    case 'l':
      if (info.flags != TB_FILE_FLAG_LINK) return false;
      break;
    case 'r':
      if (!tb_file_access (as_charp (path), TB_FILE_MODE_RO)) return false;
      break;
    case 'w':
      if (!tb_file_access (as_charp (path), TB_FILE_MODE_WO)) return false;
      break;
    case 'x':
      if (!tb_file_access (as_charp (path), TB_FILE_MODE_EXEC)) return false;
      break;
    }
  return true;
}

int
file_size (url u) {
  if (!is_single_path (u)) return -1;

  string         path= as_string (u);
  tb_file_info_t info;
  if (tb_file_info (as_charp (path), &info)) {
    return info.size;
  }
  else {
    return -1;
  }
}

int
last_modified (url u) {
  if (!is_single_path (u)) return -1;

  string         path= as_string (u);
  tb_file_info_t info;
  if (tb_file_info (as_charp (path), &info)) {
    return info.mtime;
  }
  else {
    return -1;
  }
}

static tb_long_t
tb_directory_walk_func (tb_char_t const* path, tb_file_info_t const* info,
                        tb_cpointer_t priv) {
  // check
  tb_assert_and_check_return_val (path && info, TB_DIRECTORY_WALK_CODE_END);

  array<string>* p_arr_result= (array<string>*) priv;
  (*p_arr_result) << as_string (tail (url_system (string (path))));
  return TB_DIRECTORY_WALK_CODE_CONTINUE;
}

array<string>
read_directory (url u, bool& error_flag) {
  if (!is_single_path (u)) {
    error_flag= false;
    return array<string> ();
  }

  string        path      = as_string (u);
  array<string> arr_result= array<string> ();
  error_flag              = !is_directory (u);
  if (error_flag) {
    return arr_result;
  }
  tb_directory_walk (as_charp (path), 0, tb_false, tb_directory_walk_func,
                     &arr_result);
  return arr_result;
}

void
mkdir (url u) {
  string label= u.label ();
  if (label == "none" || label == "root" || label == "wildcard") return;
  else if (is_single_path (u)) { // label == "" or label == "concat"
    string path= as_string (u);
    tb_directory_create (as_charp (path));
  }
  else { // label == "or"
    mkdir (u[1]);
    mkdir (u[2]);
  }
}

void
make_dir (url which) {
  if (is_none (which)) return;
  if (!is_directory (which)) {
    make_dir (head (which));
    mkdir (which);
  }
}

void
rmdir (url u) {
  string label= u.label ();
  if (label == "none" || label == "root" || label == "wildcard") return;
  else if (is_single_path (u)) {
    string path= as_string (u);
    tb_directory_remove (as_charp (path));
  }
  else { // label == "or"
    rmdir (u[1]);
    rmdir (u[2]);
  }
}

url
url_temp (string suffix) {
  tb_char_t        uuid[37];
  const tb_char_t* ret= tb_uuid4_make_cstr (uuid, tb_null);
  if (ret == NULL) {
    TM_FAILED ("Failed to generate UUID");
  }
  string file_name=
      replace (ret, string ("-"), string ("")) * string ("_") * suffix;
  url u= url_temp_dir () * url (file_name);
  if (file_size (u) == -1) {
    return u;
  }
  else {
    return url_temp (suffix);
  }
}

url
url_temp_dir_sub () {
#if defined(OS_WIN) || defined(OS_MINGW)
  url main_tmp_dir= url_system ("$TMP") * url (".lolly");
#else
  url main_tmp_dir= url_system ("/tmp") * url (".lolly");
#endif
  static url tmp_dir= main_tmp_dir * url (as_string (get_process_id ()));
  return (tmp_dir);
}

url
url_temp_dir () {
  static url u;
  if (u == url_none ()) {
    u= url_temp_dir_sub ();
    make_dir (u);
  }
  return u;
}

void
move (url u1, url u2) {
  string p1= as_string (u1);
  string p2= as_string (u2);

  tb_file_rename (as_charp (p1), as_charp (p2));
}

void
copy (url u1, url u2) {
  string p1= as_string (u1);
  string p2= as_string (u2);

  tb_file_copy (as_charp (p1), as_charp (p2), TB_FILE_COPY_LINK);
}

void
remove (url u) {
  string label= u.label ();
  if (label == "none" || label == "root" || label == "wildcard") return;
  else if (is_single_path (u)) {
    string path= as_string (u);
    tb_file_remove (as_charp (path));
  }
  else { // label == "or"
    remove (u[1]);
    remove (u[2]);
  }
}
