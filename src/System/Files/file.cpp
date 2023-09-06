
/******************************************************************************
* MODULE     : file.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "tm_file.hpp"
#include "tmfs_url.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "merge_sort.hpp"
#include "data_cache.hpp"
#include "web_files.hpp"
#include "tree_label.hpp"
#include "tree_helper.hpp"
#include "tm_timer.hpp"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/stat.h>

#ifndef OS_WIN
#include <sys/file.h>
#include <unistd.h>
#endif

#include <sys/types.h>
#include <string.h>  // strerror
#if defined (OS_MINGW) || defined (OS_WIN)
#include "Windows/win_utf8_compat.hpp"
#include <time.h>
#include <dirent.h>
#else
#include <dirent.h>
#define struct_stat struct stat
#endif

string main_tmp_dir= "$TEXMACS_HOME_PATH/system/tmp";

bool is_regular (url name) { return is_of_type (name, "f"); }
bool is_directory (url name) { return is_of_type (name, "d"); }
bool is_symbolic_link (url name) { return is_of_type (name, "l"); }

url
url_temp (string suffix) {
#ifdef OS_MINGW
  unsigned int rnd= raw_time ();
#else
  static bool initialized= false;
  if (!initialized) {
    srandom ((int) raw_time ());
    initialized= true;
  }
  unsigned int rnd= random ();
#endif
  string name= "tmp_" * as_string (rnd) * suffix;
  url u= url_temp_dir () * url (name);
  if (exists (u)) return url_temp (suffix);
  return u;
}

url
url_temp_dir_sub () {
#ifdef OS_MINGW
  static url tmp_dir=
    url_system (main_tmp_dir) * url_system (as_string (time (NULL)));
#else
  static url tmp_dir=
    url_system (main_tmp_dir) * url_system (as_string ((int) getpid ()));
#endif
  return (tmp_dir);
}

void
make_dir (url which) {
  if (is_none(which))
    return ;
  if (!is_directory (which)) {
    make_dir (head (which));
    mkdir (which);
  }
}

url
url_temp_dir () {
  static url u;
  if (u == url_none()) {
    u= url_temp_dir_sub ();
    make_dir (u);
  }
  return u;
}

/******************************************************************************
* Reading directories
******************************************************************************/

static array<string>
cache_dir_get (string dir) {
  tree t= cache_get ("dir_cache.scm", dir);
  array<string> a (N(t));
  for (int i=0; i<N(t); i++) a[i]= t[i]->label;
  return a;
}

static void
cache_dir_set (string dir, array<string> a) {
  tree t (TUPLE, N(a));
  for (int i=0; i<N(a); i++) t[i]= a[i];
  cache_set ("dir_cache.scm", dir, t);
}

array<string>
read_directory (url u, bool& error_flag) {
  // cout << "Directory " << u << LF;
  u= resolve (u, "dr");
  if (is_none (u)) return array<string> ();
  string name= concretize (u);

  // Directory contents in cache?
  if (is_cached ("dir_cache.scm", name) && is_up_to_date (u))
    return cache_dir_get (name);
  bench_start ("read directory");
  // End caching

  DIR* dp;
  c_string temp (name);
  dp= opendir (temp);
  error_flag= (dp==NULL);
  if (error_flag) return array<string> ();

  array<string> dir;
  struct dirent* ep;
  while (true) {
    ep= readdir (dp);
    if (ep==NULL) break;
    dir << string (ep->d_name);
  }
  (void) closedir (dp);
  merge_sort (dir);

  // Caching of directory contents
  bench_cumul ("read directory");
  if (do_cache_dir (name))
    cache_dir_set (name, dir);
  // End caching

  return dir;
}

/******************************************************************************
* Commands on files
******************************************************************************/

void
move (url u1, url u2) {
  c_string _u1 (concretize (u1));
  c_string _u2 (concretize (u2));
  (void) rename (_u1, _u2);
}

void
copy (url u1, url u2) {
  string s;
  if (!load_string (u1, s, false))
    (void) save_string (u2, s, false);
}

void
remove_sub (url u) {
  if (is_none (u));
  else if (is_or (u)) {
    remove_sub (u[1]);
    remove_sub (u[2]);
  }
  else {
    c_string _u (concretize (u));
#ifdef OS_MINGW
    if (nowide::remove (_u) && DEBUG_AUTO) {
#else
    if (::remove (_u) && DEBUG_AUTO) {
#endif
      std_warning << "Remove failed: " << strerror (errno) << LF;
      std_warning << "File was: " << u << LF;
    }
  }
}

void
remove (url u) {
  remove_sub (expand (complete (u)));
}

void
rmdir (url u) {
  remove_sub (expand (complete (u, "dr")));
}

void
mkdir (url u) {
#if defined (HAVE_SYS_TYPES_H) && defined (HAVE_SYS_STAT_H)
  if (!exists (u)) {
    if (!is_atomic (u) && !is_root (u)) mkdir (head (u));
    c_string _u (concretize (u));
    (void) ::mkdir (_u, S_IRWXU + S_IRGRP + S_IROTH);
  }
#else
#ifdef OS_MINGW
  system ("mkdir", u);
#else
  system ("mkdir -p", u);
#endif
#endif
}

void
change_mode (url u, int mode) {
#if defined (HAVE_SYS_TYPES_H) && defined (HAVE_SYS_STAT_H)
  c_string _u (concretize (u));
  (void) ::chmod (_u, mode);
#else
  string m0= as_string ((mode >> 9) & 7);
  string m1= as_string ((mode >> 6) & 7);
  string m2= as_string ((mode >> 3) & 7);
  string m3= as_string (mode & 7);
  system ("chmod -f " * m0 * m1 * m2 * m3, u);
#endif
}
