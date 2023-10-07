
/******************************************************************************
* MODULE     : url.cpp
* DESCRIPTION: unified resource location handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* The url class uses a tree representation for urls.
* This allows us to generalize the concept of an url and allow paths and
* patterns to be regarded as urls too. An url is either a string or a tuple
* of one of the following types:
*   "." -- here
*   ".." -- parent
*   none -- invalid url
*   concat -- a/b/c is represented as (concat "a" (concat "b" "c"));
*   or -- the path a:b/c is represented as (or "a" (concat "b" "c"));
*   root -- the url http://gnu.org yields (concat (root "http") "gnu.org");
*   wildcard -- (wildcard) corresponds to any url, (wildcard "*.tm")
*     to all strings which end with .tm and (wildcard "*.tm" "file")
*     to all TeXmacs files (i.e. discarding directories ending with .tm).
*******************************************************************************
* There are three main types of urls:
*   - rootless urls, like a/b/c. These urls are mainly used in computations.
*     For example, they can be appended to another url.
*   - Standard rooted urls, like file:///usr or https://www.texmacs.org.
*     These are the same as those used on the web.
*   - System urls, characterized by a "default" root.
*     These urls are similar to standard rooted urls, but they behave
*     in a slightly different way with respect to concatenation.
*     For instance https://www.texmacs.org/Web * file:///tmp would yield
*     file:///tmp, where as https://www.texmacs.org/Web * /tmp yields
*     https://www.texmacs.org/tmp
*******************************************************************************
* There are several formats for parsing (and printing) urls:
*   - System format: the usual format on your operating system.
*     On unix systems "/usr/bin:/usr/local/bin" would be a valid url
*     representing a path and on windows systems "c:\windows;c:\TeXmacs"
*     would be OK.
*   - Unix format: this format forces unix-like notation even for
*     other systems like Windows. This is convenient for url's in
*     the source code. Unix environment variables like ~ and $TEXMACS_PATH
*     can also be part of the url.
*   - Standard format: the format which is used on the web.
*     Notice that ftp://www.texmacs.org/pub and ftp://www.texmacs.org/pub/
*     represent different urls. The second one is represented by concating
*     on the right with an empty name.
*******************************************************************************
* When an explicit operation on urls need to be performed,
* like reading a file, the url is first "resolved" into a simple url
* with a unique name (modulo symbolic links) for the resource.
* Next, the url is "concretized" as a file name which is understood
* by the operating system. This may require searching the file from the web.
* Concretized urls should be used quickly and not memorized,
* since such names may be the names of temporary files,
* which may be destroyed soon afterwards.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"

#include "url.hpp"
#include "tmfs_url.hpp"
#include "tm_url.hpp"
#include "sys_utils.hpp"
#include "web_files.hpp"
#include "file.hpp"
#include "tm_file.hpp"
#include "analyze.hpp"
#include "scheme.hpp"

url tm_url (string name) {
  if (starts (name, "tmfs://")) return tmfs_url (name (7, N (name)));
  else return url_general (name, URL_UNIX);
}

bool url_test (url name, string filter) {
    if (filter == "") return true;
  int i, n= N(filter);

  // Files from the web
  if (is_rooted_web (name)) {
    // cout << "  try " << name << "\n";
    url from_web= get_from_web (name);
    // cout << "  --> " << from_web << "\n";
    if (is_none (from_web)) return false;
    for (i=0; i<n; i++)
      switch (filter[i]) {
      case 'd': return false;
      case 'l': return false;
      case 'w': return false;
      case 'x': return false;
      }
    return true;
  }

  // Files from a remote server
  if (is_rooted_tmfs (name)) {
    for (i=0; i<n; i++)
      switch (filter[i]) {
      case 'd': return false;
      case 'l': return false;
      case 'r':
        if (!as_bool (call ("tmfs-permission?", name, "read")))
          return false;
        break;
      case 'w':
        if (!as_bool (call ("tmfs-permission?", name, "write")))
          return false;
        break;
      case 'x': return false;
      }
    return true;
  }

  // Files from the ramdisk
  if (is_ramdisc (name))
    return true;

  return is_of_type (name, filter);
}


/******************************************************************************
* Conversion routines for urls
******************************************************************************/

bool
is_secure (url u) {
  return descends (u, expand (url_path ("$TEXMACS_SECURE_PATH")));
}

/******************************************************************************
* Url resolution and wildcard expansion
******************************************************************************/

url complete (url base, url u, string filter, bool flag);

static url
complete (url base, url sub, url u, string filter, bool flag) {
  if (is_or (sub)) {
    url res1= complete (base, sub[1], u, filter, flag);
    if ((!is_none (res1)) && flag) return res1;
    return res1 | complete (base, sub[2], u, filter, flag);
  }
  if (is_concat (sub) && is_rooted (sub[1])) {
    url res= complete (sub[1], sub[2], u, filter, flag);
    return sub[1] * res;
  }
  return sub * complete (base * sub, u, filter, flag);
}

url
complete (url base, url u, string filter, bool flag) {
  // cout << "complete " << base << " |||| " << u << LF;
  if (!is_rooted(u)) {
     if (is_none (base)) return base;
     if (is_none (u)) return u;
     if ((!is_root (base)) && (!is_rooted_name (base))) {
       failed_error << "base  = " << base << LF;
       failed_error << "u     = " << u << LF;
       failed_error << "filter= " << filter << LF;
       TM_FAILED ("invalid base url");
     }
  }
  if (is_name (u) || (is_concat (u) && is_root (u[1]) && is_name (u[2]))) {
    url comp= base * u;
    if (is_rooted (comp, "default") || is_rooted (comp, "file")) {
      if (url_test (comp, filter)) return reroot (u, "default");
      return url_none ();
    }
    if (is_rooted_web (comp) || is_rooted_tmfs (comp) || is_ramdisc (comp)) {
      if (url_test (comp, filter)) return u;
      return url_none ();
    }
    failed_error << "base  = " << base << LF;
    failed_error << "u     = " << u << LF;
    failed_error << "filter= " << filter << LF;
    ASSERT (is_rooted (comp), "unrooted url");
    TM_FAILED ("bad protocol in url");
  }
  if (is_root (u)) {
    // FIXME: test filter flags here
    return u;
  }
  if (is_concat (u) && is_wildcard (u[1], 0) && is_wildcard (u[2], 1)) {
    // FIXME: ret= ret | ... is unefficient (quadratic) in main loop
    if (!(is_rooted (base, "default") || is_rooted (base, "file"))) {
      failed_error << "base  = " << base << LF;
      failed_error << "u     = " << u << LF;
      failed_error << "filter= " << filter << LF;
      TM_FAILED ("wildcards only implemented for files");
    }
    url ret= url_none ();
    bool error_flag;
    array<string> dir= read_directory (base, error_flag);
    int i, n= N(dir);
    for (i=0; i<n; i++) {
      if ((!is_none (ret)) && flag) return ret;
      if ((dir[i] == ".") || (dir[i] == "..")) continue;
      if (starts (dir[i], "http://") ||
          starts (dir[i], "https://") ||
          starts (dir[i], "ftp://"))
        if (is_directory (base * dir[i])) continue;
      ret= ret | (dir[i] * complete (base * dir[i], u, filter, flag));
      if (match_wildcard (dir[i], u[2][1]->t->label))
        ret= ret | complete (base, dir[i], filter, flag);
    }
    return ret;
  }
  if (is_concat (u)) {
    url sub= complete (base, u[1], "", false);
    // "" should often be faster than the more correct "d" here
    return complete (base, sub, u[2], filter, flag);
  }
  if (is_or (u)) {
    url res1= complete (base, u[1], filter, flag);
    if ((!is_none (res1)) && flag) return res1;
    return res1 | complete (base, u[2], filter, flag);
  }
  if (is_wildcard (u)) {
    // FIXME: ret= ret | ... is unefficient (quadratic) in main loop
    if (!(is_rooted (base, "default") || is_rooted (base, "file"))) {
      failed_error << "base  = " << base << LF;
      failed_error << "u     = " << u << LF;
      failed_error << "filter= " << filter << LF;
      TM_FAILED ("wildcards only implemented for files");
    }
    url ret= url_none ();
    if (is_wildcard (u, 0) && url_test (base, filter)) ret= url_here ();
    bool error_flag;
    array<string> dir= read_directory (base, error_flag);
    int i, n= N(dir);
    for (i=0; i<n; i++) {
      if ((!is_none (ret)) && flag) return ret;
      if ((dir[i] == ".") || (dir[i] == "..")) continue;
      if (starts (dir[i], "http://") ||
          starts (dir[i], "https://") ||
          starts (dir[i], "ftp://"))
        if (is_directory (base * dir[i])) continue;
      if (is_wildcard (u, 0))
        ret= ret | (dir[i] * complete (base * dir[i], u, filter, flag));
      else if (match_wildcard (dir[i], u[1]->t->label))
        ret= ret | complete (base, dir[i], filter, flag);
    }
    return ret;
  }
  failed_error << "url= " << u << LF;
  TM_FAILED ("bad url");
  return u;
}

url
complete (url u, string filter, bool flag) {
  url home= url_pwd ();
  return home * complete (home, u, filter, flag);
}

url
complete (url u, string filter) {
  // This routine can be used in order to find all possible matches
  // for the wildcards in an url and replace the wildcards by these matches.
  // Moreover, matches are normalized (file root -> default root).
  url r =  complete (u, filter, false);
  // cout << "complete:" << u << " filter:" << filter << " result:" << r << LF;
  return r;
}

url
resolve (url u, string filter) {
  // This routine does the same thing as complete, but it stops at
  // the first match. It is particularly useful for finding files in paths.
  return complete (u, filter, true);
  /*
  url res= complete (u, filter, true);
  if (is_none (res))
    cout << "Failed resolution of " << u << ", " << filter << LF;
  return res;
  */
}

url
resolve_in_path (url u) {
#ifdef OS_MINGW
  return resolve ((url_path ("$TEXMACS_PATH/bin") | url_path ("$PATH")) * u, "x");
#else
  return resolve (url_path ("$PATH") * u, "x");
#endif
}

bool
exists (url u) {
  return !is_none (resolve (u, "r"));
}

bool
exists_in_path (url u) {
#ifdef OS_MINGW
  return !is_none (resolve_in_path (url (as_string (u) * ".bat"))) ||\
         !is_none (resolve_in_path (url (as_string (u) * ".exe"))) ||\
         !is_none (resolve_in_path (url (as_string (u) * ".com")));
#else
  return !is_none (resolve_in_path (u));
#endif
}

bool
has_permission (url u, string filter) {
  return !is_none (resolve (u, filter));
}

static url
descendance_sub (url u) {
  if (is_or (u))
    return descendance_sub (u[1]) | descendance_sub (u[2]);
  return complete (u, url_wildcard (), "r", false);
}

url
descendance (url u) {
  // Utility for style and package menus in tm_server.cpp
  // Compute and merge subdirectories of directories in path
  return factor (descendance_sub (u));
}

url
subdirectories (url u) {
  if (is_or (u))
    return subdirectories (u[1]) | subdirectories (u[2]);
  else if (is_directory (u)) {
    url ret= u;
    bool error_flag;
    array<string> dir= read_directory (u, error_flag);
    for (int i=0; i<N(dir); i++)
      if (!starts (dir[i], ".") && is_directory (u * dir[i]))
        ret= ret | subdirectories (u * dir[i]);
    return ret;
  }
  else return url_none ();
}

/******************************************************************************
* Concretization of resolved urls
******************************************************************************/

url
concretize_url (url u) {
  // This routine transforms a resolved url into a system url.
  // In the case of distant files from the web, a local copy is created.
  if (is_rooted (u, "default") ||
      is_rooted (u, "file") ||
      is_rooted (u, "blank"))
        return reroot (u, "default");
  if (is_rooted_web (u)) return concretize_url (get_from_web (u));
  if (is_rooted_tmfs (u)) return concretize_url (get_from_server (u));
  if (is_ramdisc (u)) return concretize_url (get_from_ramdisc (u));
  if (is_here (u)) return url_pwd ();
  if (is_parent (u)) return url_pwd () * url_parent ();
  return url_none ();
}

string
concretize (url u) {
  // This routine transforms a resolved url into a system file name.
  // In the case of distant files from the web, a local copy is created.
  url c= concretize_url (u);
  if (!is_none (c)) return as_string (c);
  if (is_wildcard (u, 1)) return u->t[1]->label;
  std_warning << "Couldn't concretize " << u->t << LF;
  // failed_error << "u= " << u << LF;
  // TM_FAILED ("url has no root");
  return "xxx";
}

string
materialize (url u, string filter) {
  // Combines resolve and concretize
  url r= resolve (u, filter);
  if (!(is_rooted (r) || is_here (r) || is_parent (r))) {
    failed_error << "u= " << u << LF;
    TM_FAILED ("url could not be resolved");
  }
  return concretize (r);
}
