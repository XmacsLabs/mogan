
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
#include "sys_utils.hpp"
#include "tm_debug.hpp"
#include "url.hpp"
#include "file.hpp"
#include "tm_file.hpp"

#ifdef OS_MINGW
#include "Windows/win_sys_utils.hpp"
#endif

#ifdef QTTEXMACS
#include "Qt/qt_sys_utils.hpp"
#endif


int script_status = 1;


string get_current_cpu_arch () {
#ifdef QTTEXMACS
  return qt_get_current_cpu_arch ();
#else
  return "unknown";
#endif
}

string get_pretty_os_name () {
#ifdef QTTEXMACS
  return qt_get_pretty_os_name ();
#else
  return "unknown";
#endif
}

/******************************************************************************
* System functions
******************************************************************************/

int
system (string s, string& result, string& error) {
#ifdef OS_WASM
  return -1;
#else
  return qt_system (s, result, error);
#endif
}

int
system (string s, string& result) {
#ifdef OS_WASM
  return -1;
#else
  return qt_system (s, result);
#endif
}

string
eval_system (string s) {
  string result;
  (void) system (s, result);
  return result;
}

string
var_eval_system (string s) {
  string r= eval_system (s);
  while ((N(r)>0) && (r[N(r)-1]=='\n' || r[N(r)-1]=='\r')) r= r (0,N(r)-1);
  return r;
}

url
get_texmacs_path () {
  string tmpath= get_env ("TEXMACS_PATH");
    //FIXME: Why is this?
  while ((N(tmpath)>0) && (tmpath [N(tmpath) - 1] == '/'))
    tmpath= tmpath (0, N(tmpath)-1);
  return url_system (tmpath);
}

url
get_texmacs_home_path () {
  url path= get_env ("TEXMACS_HOME_PATH");
  if (path == "")
    path= url_system ("$HOME/.TeXmacs");
  return path;
}


string 
get_printing_default () {
#if defined (OS_MINGW)
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
  bool blank;
public:	
  PrintCap (): blank (true) {};
  friend string get_printing_cmd ();
  friend void set_printing_cmd (string);
} print_cap;

string
get_printing_cmd () {
  if (print_cap.blank) {
    print_cap.prt_cmd= get_printing_default ();
    print_cap.blank= false;
  }
  return print_cap.prt_cmd;
}

void
set_printing_cmd (string cmd) {
  print_cap.prt_cmd= cmd;
  print_cap.blank= false;
}

bool
has_printing_cmd () {
  static bool has= get_printing_cmd () != "";
  return has;
}

static const char*
default_look_and_feel_impl () {
  if (os_mingw () || os_win ()) return "windows";
  if (os_macos ()) return "macos";
  return "emacs";
}

const char*
default_look_and_feel () {
  static const char* ret= default_look_and_feel_impl ();
  return ret;
}
