
/******************************************************************************
 * MODULE     : code.cpp
 * DESCRIPTION: main entry for Mogan Code
 * COPYRIGHT  : (C) 2023 Oyyko
 *                  2023 Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_configure.hpp"
#include <fcntl.h>
#include <locale.h> // for setlocale
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifndef OS_WIN
#include <unistd.h>
#endif
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
#include "tm_file.hpp"
#include "tm_ostream.hpp"
#include "tm_timer.hpp"
#include "tm_window.hpp"

#ifdef QTTEXMACS
#include "Qt/QTMApplication.hpp"
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

void
immediate_options (int argc, char** argv) {
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
  windows_delayed_refresh (1000000000);
  immediate_options (argc, argv);
  load_user_preferences ();
  string theme= get_user_preference ("gui theme", "default");
  if (theme == "default") theme= "light";
  if (theme == "light")
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/standard-light.css";
  else if (theme == "dark")
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/standard-dark.css";
  else if (theme != "") tm_style_sheet= theme;

  set_env ("LC_NUMERIC", "POSIX");
  set_env ("XDG_SESSION_TYPE", "x11");

#ifdef QTTEXMACS
  // initialize the Qt application infrastructure
  if (headless_mode) qtmcoreapp= new QTMCoreApplication (argc, argv);
  else qtmapp= new QTMApplication (argc, argv);
#endif
  init_texmacs_path (argc, argv);
#ifdef QTTEXMACS
  if (!headless_mode) qtmapp->set_window_icon ("/misc/images/texmacs-512.png");
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
