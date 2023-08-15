
#include "url_helper.hpp"
#include "web_files.hpp"
#include "sys_utils.hpp"
#include "file.hpp"
#include "config.h"


#ifdef OS_MINGW
#define WINPATHS
#endif

#ifdef WINPATHS
#define URL_CONCATER  '\\'
#define URL_SEPARATOR ';'
#else
#define URL_CONCATER  '/'
#define URL_SEPARATOR ':'
#endif


/******************************************************************************
* Unrooted url constructors
******************************************************************************/

static url
url_get_atom (string s, int type) {
  if (type < URL_STANDARD) {
    if (s == "~") return url_system (get_env ("HOME"));
    if (starts (s, "$")) {
      string val= get_env (s (1, N(s)));
      if (val == "") return url_none ();
      return unblank (url_system (val));
    }
  }
  if (occurs ("*", s)) return url_wildcard (s);
#ifdef WINPATHS
  if (N(s)==2 && ends (s, ":"))
    s->resize(1); // remove the ':' after unit letter
#endif
  return as_url (tree (s));
}

static void
skip_ipv6 (string s, int& i) {
  i++;
  while (i<N(s) && (s[i] == ':' ||
                    (s[i] >= '0' && s[i] <= '9') ||
                    (s[i] >= 'a' && s[i] <= 'f') ||
                    (s[i] >= 'A' && s[i] <= 'F'))) i++;
  if (i<N(s) && s[i] == ']') i++;
}

static url
url_get_name (string s, int type= URL_STANDARD, int i=0) {
  char sep= (type == URL_SYSTEM)? URL_CONCATER: '/';
  int start= i, n= N(s);
  while ((i<n) && (s[i] != sep) && (s[i] != '/')) {
    if (s[i] == '[') skip_ipv6 (s, i); else i++; }
  url u= url_get_atom (s (start, i), type);
  // url u= tree (s (start, i));
  if (i == n) return u;
  if (start == i) return url_get_name (s, type, i+1);
  return u * url_get_name (s, type, i+1);
}

static url
url_get_path (string s, int type= URL_STANDARD, int i=0) {
  char sep= (type == URL_SYSTEM)? URL_SEPARATOR: ':';
  int start= i, n= N(s);
  if (i == n) return url_none ();
  while ((i<n) && (s[i] != sep)) {
    if (s[i] == '[') skip_ipv6 (s, i); else i++; }
  url u= url_general (s (start, i), type);
  if (i == n) return u;
  if (start == i) return url_get_path (s, type, i+1);
  return u | url_get_path (s, type, i+1);
}

static url
url_path (string s, int type= URL_SYSTEM) {
  url u= url_get_path (s, type);
  return u;
}



/******************************************************************************
* Url resolution and wildcard expansion
******************************************************************************/

static url complete (url base, url u, string filter, bool flag);

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

static url
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
      if (is_of_type (comp, filter)) return reroot (u, "default");
      return url_none ();
    }
    if (is_rooted_web (comp) || is_rooted_tmfs (comp) || is_ramdisc (comp)) {
      if (is_of_type (comp, filter)) return u;
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
    if (is_wildcard (u, 0) && is_of_type (base, filter)) ret= url_here ();
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

static url
complete (url u, string filter, bool flag) {
  url home= url_pwd ();
  return home * complete (home, u, filter, flag);
}

url
complete (url u, string filter="fr") {
  // This routine can be used in order to find all possible matches
  // for the wildcards in an url and replace the wildcards by these matches.
  // Moreover, matches are normalized (file root -> default root).
  url r =  complete (u, filter, false);
  // cout << "complete:" << u << " filter:" << filter << " result:" << r << LF;
  return r;
}

url
complete (url u) {
    return complete(u, "fr");
}

url
resolve (url u) {
    return resolve(u, "fr");
}

url
resolve (url u, string filter="fr") {
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

string materialize(url u) {
    return materialize(u, "fr");
}

string
materialize (url u, string filter="fr") {
  // Combines resolve and concretize
  url r= resolve (u, filter);
  if (!(is_rooted (r) || is_here (r) || is_parent (r))) {
    failed_error << "u= " << u << LF;
    TM_FAILED ("url could not be resolved");
  }
  return concretize (r);
}