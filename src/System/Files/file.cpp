
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

/******************************************************************************
* New style loading and saving
******************************************************************************/

bool
load_string (url u, string& s, bool fatal) {
  // cout << "Load " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r);
  // cout << "Resolved " << r << LF;
  bool err= !is_rooted_name (r);
  if (!err) {
    string name= concretize (r);
    // cout << "Concrete :" << name << LF;
    // File contents in cache?
    bool file_flag= do_cache_file (name);
    bool doc_flag= do_cache_doc (name);
    string cache_type= doc_flag? string ("doc_cache"): string ("file_cache");
    if (doc_flag) cache_load ("doc_cache");
    bool currently_cached= is_cached (cache_type, name);
    if (currently_cached && is_up_to_date (url_parent (r))) {
      s= cache_get (cache_type, name) -> label;
      return false;
    }
    // End caching

    bench_start ("load file");
    c_string _name (name);
    // cout << "OPEN :" << _name << LF;
#ifdef OS_MINGW
    FILE* fin= fopen (_name, "rb");
#else
    FILE* fin= fopen (_name, "r");
    int fd= -1;
    if (fin != NULL) {
      fd= fileno (fin);
      if (flock (fd, LOCK_SH) == -1) {
        fclose (fin);
        fin= NULL;
      }
    }
#endif
    if (fin == NULL) {
      err= true;
      if (!occurs ("system", name))
        std_warning << "Load error for " << name << ", "
                    << strerror(errno) << "\n";
    }
    int size= 0;
    if (!err) {
      if (fseek (fin, 0L, SEEK_END) < 0) err= true;
      else {
        size= ftell (fin);
        if (size<0) err= true;
      }
      if (err) {
        std_warning << "Seek failed for " << as_string (u) << "\n";
#ifdef OS_MINGW
#else
        flock (fd, LOCK_UN);
#endif
        fclose (fin);
      }
    }
    if (!err) {
      rewind (fin);
      s->resize (size);
      int read= fread (&(s[0]), 1, size, fin);
      if (read < size) s->resize (read);
#ifdef OS_MINGW
#else
      flock (fd, LOCK_UN);
#endif
      fclose (fin);
    }
    bench_cumul ("load file");

    // Cache file contents
    if (!err && (N(s) <= 10000 || currently_cached))
      if (file_flag || doc_flag)
        cache_set (cache_type, name, s);
    // End caching
  }
  if (err) {
    string err_msg = string("Failed to load file: ") * as_string (u);
    if (fatal) {
      failed_error << err_msg << LF;
      TM_FAILED ("file not readable");
    }
    //else debug_io << err_msg << LF;
  }
  return err;
}

string string_load (url u) {
  string s;
  (void) load_string (u, s, false);
  return s;
}

bool
save_string (url u, string s, bool fatal) {
  if (is_rooted_tmfs (u)) {
    bool err= save_to_server (u, s);
    if (err && fatal) {
      failed_error << "File name= " << as_string (u) << "\n";
      TM_FAILED ("file not writeable");
    }
    return err;
  }

  // cout << "Save " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r, "");
  bool err= !is_rooted_name (r);
  if (!err) {
    string name= concretize (r);
    {
      c_string _name (name);
#ifdef OS_MINGW
      FILE* fout= fopen (_name, "wb");
#else
      FILE* fout= fopen (_name, "r+");
      bool rw= (fout != NULL);
      if (!rw) fout= fopen (_name, "w");
      int fd= -1;
      if (fout != NULL) {
        fd= fileno (fout);
        if (flock (fd, LOCK_EX) == -1) {
          fclose (fout);
          fout= NULL;
        }
        else if (rw) ftruncate (fd, 0);
      }
#endif
      if (fout == NULL) {
        err= true;
        std_warning << "Save error for " << name << ", "
                    << strerror(errno) << "\n";
      }
      if (!err) {
        int i, n= N(s);
        for (i=0; i<n; i++)
          fputc (s[i], fout);
#ifdef OS_MINGW
#else
        flock (fd, LOCK_UN);
#endif
        fclose (fout);
      }
    }
    // Cache file contents
    bool file_flag= do_cache_file (name);
    bool doc_flag= do_cache_doc (name);
    string cache_type= doc_flag? string ("doc_cache"): string ("file_cache");
    if (!err && N(s) <= 10000)
      if (file_flag || doc_flag)
        cache_set (cache_type, name, s);
    declare_out_of_date (url_parent (r));
    // End caching
  }

  if (err && fatal) {
    failed_error << "File name= " << as_string (u) << "\n";
    TM_FAILED ("file not writeable");
  }
  return err;
}

void string_save (string s, url u) {
  (void) save_string (u, s);
}

bool
append_string (url u, string s, bool fatal) {
  if (is_rooted_tmfs (u)) TM_FAILED ("file not appendable");

  // cout << "Save " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r, "");
  bool err= !is_rooted_name (r);
  if (!err) {
    string name= concretize (r);
    {
      c_string _name (name);
#ifdef OS_MINGW
      FILE* fout= fopen (_name, "ab");
#else
      FILE* fout= fopen (_name, "a");
      int fd= -1;
      if (fout != NULL) {
        fd= fileno (fout);
        if (flock (fd, LOCK_EX) == -1) {
          fclose (fout);
          fout= NULL;
        }
      }
#endif
      if (fout == NULL) {
        err= true;
        std_warning << "Append error for " << name << ", "
                    << strerror(errno) << "\n";
      }
      if (!err) {
        int i, n= N(s);
        for (i=0; i<n; i++)
          fputc (s[i], fout);
#ifdef OS_MINGW
#else
        flock (fd, LOCK_UN);
#endif
        fclose (fout);
      }
    }
    // Cache file contents
    declare_out_of_date (url_parent (r));
    // End caching
  }

  if (err && fatal) {
    failed_error << "File name= " << as_string (u) << "\n";
    TM_FAILED ("file not appendable");
  }
  return err;
}

void string_append_to_file (string s, url u) {
  (void) append_string (u, s);
}

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
append_to (url what, url to) {
  string what_s;
  if (load_string (what, what_s, false) ||
      append_string (to, what_s, false))
    std_warning << "Append failed for " << to << LF;
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

/******************************************************************************
* Tab-completion for file names
******************************************************************************/

#ifdef OS_WIN
#define URL_CONCATER  '\\'
#else
#define URL_CONCATER  '/'
#endif

static void
file_completions_file (array<string>& a, url search, url u) {
  if (is_or (u)) {
    file_completions_file (a, search, u[1]);
    file_completions_file (a, search, u[2]);
  }
  else {
    url v= delta (search * url ("dummy"), u);
    if (is_none (v)) return;
    string s= as_string (v);
    if (is_directory (u)) s= s * string (URL_CONCATER);
    a << s;
  }
}

static void
file_completions_dir (array<string>& a, url search, url dir) {
  if (is_or (search)) {
    file_completions_dir (a, search[1], dir);
    file_completions_dir (a, search[2], dir);
  }
  else if (is_or (dir)) {
    file_completions_dir (a, search, dir[1]);
    file_completions_dir (a, search, dir[2]);
  }
  else {
    url u= search * dir * url_wildcard ("*");
    u= complete (u, "r");
    u= expand (u);
    file_completions_file (a, search, u);
  }
}

array<string>
file_completions (url search, url dir) {
  array<string> a;
  file_completions_dir (a, search, dir);
  return a;
}
