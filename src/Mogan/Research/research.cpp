
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
#include <lolly/system/args.hpp>
#include <lolly/system/timer.hpp>

#include <sys/stat.h>
#include <sys/types.h>
#ifdef STACK_SIZE
#include <sys/resource.h>
#endif

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

extern string original_path;
extern tree   the_et;
extern bool   headless_mode;

#ifdef QTTEXMACS
static QTMApplication*     qtmapp    = NULL;
static QTMCoreApplication* qtmcoreapp= NULL;
#endif

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

#ifdef QTTEXMACS
#if defined(MAC_OS_X_VERSION_10_9) || defined(MAC_OS_X_VERSION_10_10)
#endif
#endif

#endif
}

/******************************************************************************
 * Main program
 ******************************************************************************/

void
immediate_options (int argc, char** argv) {
#if defined(OS_MINGW) || defined(OS_WIN)
  if (get_env ("HOME") == "") set_env ("HOME", get_env ("USERPROFILE"));
#endif
  init_texmacs_home_path ();
  if (is_empty (get_env ("TEXMACS_HOME_PATH"))) return;
  for (int i= 1; i < argc; i++) {
    string s= argv[i];
    if ((N (s) >= 2) && (s (0, 2) == "--")) s= s (1, N (s));
    if ((s == "-S") || (s == "-setup")) {
      remove (url ("$TEXMACS_HOME_PATH/system/settings.scm"));
      remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
      remove (get_tm_cache_path () * url_wildcard ("*"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
    }
    else if (s == "-delete-cache")
      remove (get_tm_cache_path () * url_wildcard ("*"));
    else if (s == "-delete-style-cache")
      remove (get_tm_cache_path () * url_wildcard ("__*"));
    else if (s == "-delete-font-cache") {
      remove (get_tm_cache_path () * url ("font_cache.scm"));
      remove (get_tm_cache_path () * url ("fonts") * url_wildcard ("*"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
    }
    else if (s == "-delete-plugin-cache")
      remove (get_tm_cache_path () * url ("plugin_cache.scm"));
    else if (s == "-delete-server-data")
      system ("rm -rf", url ("$TEXMACS_HOME_PATH/server"));
    else if (s == "-delete-databases") {
      system ("rm -rf", url ("$TEXMACS_HOME_PATH/system/database"));
      system ("rm -rf", url ("$TEXMACS_HOME_PATH/users"));
    }
#ifdef QTTEXMACS
    else if (s == "-headless") headless_mode= true;
#endif
    else if (s == "-log-file" && i + 1 < argc) {
      i++;
      char*      log_file= argv[i];
      tm_ostream logf (log_file);
      if (!logf->is_writable ())
        cerr << "TeXmacs] Error: could not open " << log_file << "\n";
      cout.redirect (logf);
      cerr.redirect (logf);
    }
  }
}

#include <cstdio>

int
main (int argc, char** argv) {
  lolly::init_tbox ();
  lolly::system::args a (argc, argv);

#ifdef STACK_SIZE
  struct rlimit limit;

  if (getrlimit (RLIMIT_STACK, &limit) == 0) {
    if (limit.rlim_max < STACK_SIZE) {
      cerr << "Max stack allowed value : " << limit.rlim_max << "\n";
      limit.rlim_cur= limit.rlim_max;
    }
    else limit.rlim_cur= STACK_SIZE;
    if (setrlimit (RLIMIT_STACK, &limit)) cerr << "Cannot set stack value\n";
  }
  else cerr << "Cannot get stack value\n";
#endif

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
#ifndef OS_MACOS
#ifndef OS_WASM
  set_env ("QT_QPA_PLATFORM", "xcb");
  set_env ("XDG_SESSION_TYPE", "x11");
#endif
#endif
#endif
#ifdef MACOSX_EXTENSIONS
  // Reset TeXmacs if Alt is pressed during startup
  if (mac_alternate_startup ()) {
    cout << "TeXmacs] Performing setup (Alt on startup)" << LF;
    remove (url ("$TEXMACS_HOME_PATH/system/settings.scm"));
    remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
    remove (get_tm_cache_path () * url_wildcard ("*"));
    remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
  }
#endif
#ifdef QTTEXMACS
  // initialize the Qt application infrastructure
  if (headless_mode) qtmcoreapp= new QTMCoreApplication (argc, argv);
  else qtmapp= new QTMApplication (argc, argv);
#endif
  init_texmacs_path (argc, argv);
#ifdef QTTEXMACS
  if (!headless_mode) {
    // it this really necessary? Should be set in the metadata.
    qtmapp->set_window_icon ("/misc/images/new-mogan-512.png");
    init_style_sheet (qtmapp);
  }
#endif

  // cout << "Bench  ] Started TeXmacs\n";
  the_et      = tuple ();
  the_et->data= ip_observer (path ());
  cache_initialize ();
  bench_start ("initialize texmacs");
  init_texmacs ();
  bench_cumul ("initialize texmacs");
  start_scheme (argc, argv, TeXmacs_main);
#ifdef QTTEXMACS
  if (headless_mode) delete qtmcoreapp;
  else delete qtmapp;
#endif
  return 0;
}
