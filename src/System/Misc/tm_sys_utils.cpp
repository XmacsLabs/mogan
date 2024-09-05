
/******************************************************************************
 * MODULE     : sys_utils.cpp
 * DESCRIPTION: sys utils for texmacs
 * COPYRIGHT  : (C) 1999-2016  Joris van der Hoeven, Denis Raux
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_sys_utils.hpp"
#include "file.hpp"
#include "lolly/system/subprocess.hpp"
#include "sys_utils.hpp"
#include "tm_debug.hpp"
#include "tm_file.hpp"
#include "url.hpp"

#if defined(OS_MINGW) || defined(OS_WIN)
#include "Windows/win_sys_utils.hpp"
#endif

#ifdef QTTEXMACS
#include "Qt/qt_sys_utils.hpp"
#endif

int script_status= 1;

string
mogan_app_id () {
#ifdef APP_MOGAN_RESEARCH
  return "research";
#endif

#ifdef APP_MOGAN_BEAMER
  return "beamer";
#endif

#ifdef APP_MOGAN_CODE
  return "code";
#endif

#ifdef APP_MOGAN_DRAW
  return "draw";
#endif
}

static string
app_dir () {
  if (mogan_app_id () == "research") return "XmacsLabs";
  return string ("app.mogan.") * mogan_app_id ();
}

string
get_current_cpu_arch () {
#ifdef QTTEXMACS
  return qt_get_current_cpu_arch ();
#else
  return "unknown";
#endif
}

string
get_pretty_os_name () {
#ifdef QTTEXMACS
  return qt_get_pretty_os_name ();
#else
  return "unknown";
#endif
}

/******************************************************************************
 * System functions
 ******************************************************************************/

string
eval_system (string s) {
#ifdef OS_WASM
  cout << "eval_system: " << s << LF;
  return "";
#else
  string result;
  debug_io << "check_output: " << s << LF;
  (void) lolly::system::check_stdout (s, result);
  return result;
#endif
}

string
var_eval_system (string s) {
  string r= eval_system (s);
  while ((N (r) > 0) && (r[N (r) - 1] == '\n' || r[N (r) - 1] == '\r'))
    r= r (0, N (r) - 1);
  return r;
}

string
check_stdout (string s) {
#ifdef OS_WASM
  cout << "eval_system: " << s << LF;
  return "";
#else
  string r;
  debug_io << "check_stdout: " << s << LF;
  (void) lolly::system::check_stdout (s, r);
  while ((N (r) > 0) && (r[N (r) - 1] == '\n' || r[N (r) - 1] == '\r'))
    r= r (0, N (r) - 1);
  return r;
#endif
}

string
check_stderr (string s) {
#ifdef OS_WASM
  cout << "eval_system: " << s << LF;
  return "";
#else
  string r;
  debug_io << "check_stderr: " << s << LF;
  (void) lolly::system::check_stderr (s, r);
  while ((N (r) > 0) && (r[N (r) - 1] == '\n' || r[N (r) - 1] == '\r'))
    r= r (0, N (r) - 1);
  return r;
#endif
}

url
get_texmacs_path () {
  string tmpath= get_env ("TEXMACS_PATH");
  // FIXME: Why is this?
  while ((N (tmpath) > 0) && (tmpath[N (tmpath) - 1] == '/'))
    tmpath= tmpath (0, N (tmpath) - 1);
  return url_system (tmpath);
}

url
get_texmacs_home_path () {
  string path= get_env ("TEXMACS_HOME_PATH");
  if (is_empty (path)) {
    path= "$HOME/.TeXmacs";
  }
  return url_system (path);
}

void
init_texmacs_home_path () {
  if (!is_empty (get_env ("TEXMACS_HOME_PATH"))) return;

  if (os_mingw () || os_win ()) {
    set_env ("TEXMACS_HOME_PATH", get_env ("APPDATA") * "\\" * app_dir ());
  }
  else if (os_macos ()) {
    set_env ("TEXMACS_HOME_PATH",
             get_env ("HOME") * "/Library/Application Support/" * app_dir ());
  }
  else if (os_wasm ()) {
    set_env ("TEXMACS_HOME_PATH", string ("/.") * app_dir ());
  }
  else {
#if defined(OS_HAIKU)
    set_env ("TEXMACS_HOME_PATH",
             get_env ("HOME") * "/config/settings/TeXmacs");
#else
    string xdg_data_home= get_env ("XDG_DATA_HOME");
    if (is_empty (xdg_data_home))
      xdg_data_home= get_env ("HOME") * "/.local/share";
    set_env ("TEXMACS_HOME_PATH", xdg_data_home * "/" * app_dir ());
#endif
  }
}

string
xmacs_lts_version () {
  array<string> parts= tokenize (string (XMACS_VERSION), ".");
  if (N (parts) >= 4) {
    return parts[0] * "." * parts[1] * "." * parts[2];
  }
  else {
    return XMACS_VERSION;
  }
}

url
get_tm_cache_path () {
#if defined(OS_WIN) || defined(OS_MINGW)
  return url (string ("$LOCALAPPDATA") * "/" * app_dir () * "/system/cache/" *
              xmacs_lts_version ());
#endif
#if defined(OS_MACOS)
  return url (string ("$HOME/Library/Caches/") * app_dir () * "/" *
              xmacs_lts_version ());
#endif
#if defined(OS_LINUX)
  return url (string ("$XDG_CACHE_HOME") * "/" * app_dir () * "/" *
              xmacs_lts_version ());
#endif
  return url (string ("$TEXMACS_HOME_PATH/system/cache/") *
              xmacs_lts_version ());
}

url
get_tm_preference_path () {
  return get_texmacs_home_path () *
         ("system/" * xmacs_lts_version () * "/preferences.scm");
}

string
get_printing_default () {
#if defined(OS_MINGW) || defined(OS_WIN)
  url embedded ("$TEXMACS_PATH/bin/SumatraPDF.exe");
  if (exists (embedded))
    return sys_concretize (embedded) * " -print-dialog -exit-when-done";
  else return "";
#else
  return "lp";
#endif
}

class PrintCap {
private:
  string prt_cmd;
  bool   blank;

public:
  PrintCap () : blank (true){};
  friend string get_printing_cmd ();
  friend void   set_printing_cmd (string);
} print_cap;

string
get_printing_cmd () {
  if (print_cap.blank) {
    print_cap.prt_cmd= get_printing_default ();
    print_cap.blank  = false;
  }
  return print_cap.prt_cmd;
}

void
set_printing_cmd (string cmd) {
  print_cap.prt_cmd= cmd;
  print_cap.blank  = false;
}

bool
has_printing_cmd () {
  return false;
}

void
open_url (url u) {
#ifdef QTTEXMACS
  qt_open_url (u);
#endif
}

/******************************************************************************
 * Error messages
 ******************************************************************************/

static void (*the_wait_handler) (string, string, int)= NULL;

void
set_wait_handler (void (*routine) (string, string, int)) {
  the_wait_handler= routine;
}

void
system_wait (string message, string argument, int level) {
  if (the_wait_handler == NULL) {
    if (DEBUG_AUTO) {
      if (message == "") cout << "TeXmacs] Done" << LF;
      else {
        if (argument == "") cout << "TeXmacs] " << message << LF;
        else cout << "TeXmacs] " << message << " " << argument << LF;
        cout << "TeXmacs] Please wait..." << LF;
      }
    }
  }
  else the_wait_handler (message, argument, level);
}
