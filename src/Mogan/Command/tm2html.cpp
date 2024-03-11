

/******************************************************************************
 * MODULE     : research.cpp
 * DESCRIPTION: main entry for Mogan Research
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_configure.hpp"
#include <fcntl.h>
#ifndef OS_WIN
#include <unistd.h>
#endif
#include <locale.h> // for setlocale
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef STACK_SIZE
#include <sys/resource.h>
#endif
#include <lolly/system/timer.hpp>

#include "boot.hpp"
#include "data_cache.hpp"
#include "file.hpp"
#include "observers.hpp"
#include "preferences.hpp"
#include "server.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tm_ostream.hpp"
#include "tm_timer.hpp"
#include "tm_window.hpp"
#ifdef AQUATEXMACS
void mac_fix_paths ();
#endif

#ifdef QTTEXMACS
#include "Qt/QTMApplication.hpp"
#endif

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_utilities.h"
#endif

#if defined(MACOSX_EXTENSIONS)
#include "MacOS/mac_app.h"
#endif

extern string original_path;

extern tree the_et;
extern bool texmacs_started;
extern bool headless_mode;

string extra_init_cmd;
void   server_start ();

#ifdef QTTEXMACS
// Qt application infrastructure
static QTMApplication*     qtmapp    = NULL;
static QTMCoreApplication* qtmcoreapp= NULL;
#endif

/******************************************************************************
 * Clean exit on segmentation faults
 ******************************************************************************/

void
clean_exit_on_segfault (int sig_num) {
  (void) sig_num;
  TM_FAILED ("segmentation fault");
}

/******************************************************************************
 * Main program
 ******************************************************************************/

#ifdef OS_MACOS
#include <sys/resource.h>
#endif

void
boot_hacks () {
#ifdef OS_MACOS
  // NOTE: under MACOS, there is a limited number of open file descriptors,
  // by default 256.  Any open file descriptor can actually count several times
  // whenever the files is stored in various chunks on disk.  Hence, the limit
  // is easily exceeded, although this situation cannot easily be debugged.
  // Our current hack is to allow for at least 4096 open file descriptors.
  rlimit lims;
  getrlimit (RLIMIT_NOFILE, &lims);
  lims.rlim_cur= max (lims.rlim_cur, 4096);
  setrlimit (RLIMIT_NOFILE, &lims);
  // getrlimit (RLIMIT_NOFILE, &lims);
  // printf ("cur: %i\n", lims.rlim_cur);
  // printf ("max: %i\n", lims.rlim_max);
#ifdef MACOSX_EXTENSIONS
  mac_fix_yosemite_bug ();
#endif

#endif
}

/******************************************************************************
 * Main program
 ******************************************************************************/

void
immediate_options (int argc, char** argv) {
  if (get_env ("TEXMACS_HOME_PATH") == "")
#if defined(OS_MINGW) || defined(OS_WIN)
  {
    if (get_env ("HOME") == "") set_env ("HOME", get_env ("USERPROFILE"));
    set_env ("TEXMACS_HOME_PATH", get_env ("APPDATA") * "\\TeXmacs");
  }
#elif defined(OS_HAIKU)
    set_env ("TEXMACS_HOME_PATH",
             get_env ("HOME") * "/config/settings/TeXmacs");
#elif defined(OS_WASM)
    set_env ("TEXMACS_HOME_PATH", "/.Xmacs");
#else
    set_env ("TEXMACS_HOME_PATH", get_env ("HOME") * "/.TeXmacs");
#endif
  if (get_env ("TEXMACS_HOME_PATH") == "") return;
  for (int i= 1; i < argc; i++) {
    string s= argv[i];
    if ((N (s) >= 2) && (s (0, 2) == "--")) s= s (1, N (s));
    if (s == "-headless") {
      headless_mode= true;
    }
  }
}

#include <cstdio>

int
main (int argc, char** argv) {
  lolly::init_tbox ();

  original_path= get_env ("PATH");
  boot_hacks ();
  windows_delayed_refresh (1000000000);
  immediate_options (argc, argv);
  load_user_preferences ();
  string theme= get_user_preference ("gui theme", "default");
#if defined(OS_MACOS) && !defined(__arm64__)
  if (theme == "default") theme= "";
#else
  if (theme == "default") theme= "light";
#endif
  if (theme == "light")
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/standard-light.css";
  else if (theme == "dark")
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/standard-dark.css";
  else if (theme != "") tm_style_sheet= theme;
#if !defined(OS_MINGW) && !defined(OS_WIN)
  set_env ("LC_NUMERIC", "POSIX");
#if !(defined OS_MACOS || defined OS_WASM)
  set_env ("QT_QPA_PLATFORM", "xcb");
  set_env ("XDG_SESSION_TYPE", "x11");
#endif
#endif
  qtmapp= new QTMApplication (argc, argv);

  // cout << "Bench  ] Started TeXmacs\n";
  the_et      = tuple ();
  the_et->data= ip_observer (path ());
  cache_initialize ();
  bench_start ("initialize texmacs");
  init_texmacs ();
  bench_cumul ("initialize texmacs");
  start_scheme (argc, argv, TeXmacs_main);
#ifdef QTTEXMACS
  delete qtmcoreapp;

#endif
  return 0;
}
