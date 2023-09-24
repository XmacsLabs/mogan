

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

#include "../app_type.hpp"
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
#include "Qt/qt_utilities.hpp"
#include <QDir>
#endif

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_utilities.h"
#endif

#if defined(X11TEXMACS) && defined(MACOSX_EXTENSIONS)
#include "MacOS/mac_app.h"
#endif

extern bool char_clip;

extern url    tm_init_file;
extern url    tm_init_buffer_file;
extern string my_init_cmds;
extern string original_path;

extern int geometry_w, geometry_h;
extern int geometry_x, geometry_y;

extern tree the_et;
extern bool texmacs_started;
extern bool headless_mode;

bool   disable_error_recovery= false;
bool   start_server_flag     = false;
string extra_init_cmd;
void   server_start ();

#ifdef QTTEXMACS
// Qt application infrastructure
static QTMApplication*     qtmapp    = NULL;
static QTMCoreApplication* qtmcoreapp= NULL;
#endif

/******************************************************************************
 * For testing
 ******************************************************************************/

// #define ENABLE_TESTS
#ifdef ENABLE_TESTS
void
test_routines () {
  extern void test_math ();
  test_math ();
}
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
 * Texmacs paths
 ******************************************************************************/

void
TeXmacs_init_paths (int& argc, char** argv) {
  (void) argc;
  (void) argv;
#ifdef QTTEXMACS
  url exedir= url_system (qt_application_directory ());
#else
  url exedir= url_system (argv[0]) * "..";
  if (!is_rooted (exedir)) {
    exedir= url_pwd () * exedir;
  }
#endif

  string current_texmacs_path= get_env ("TEXMACS_PATH");

#ifdef Q_OS_LINUX
  if (is_empty (current_texmacs_path) && exists (exedir * "../share/Xmacs")) {
    set_env ("TEXMACS_PATH", as_string (exedir * "../share/Xmacs"));
  }
#endif

#ifdef Q_OS_MAC
  // the following line can inibith external plugin loading
  // QCoreApplication::setLibraryPaths(QStringList());
  // ideally we would like to control the external plugins
  // and add the most useful (gif, jpeg, svg converters)
  // to the bundle package. I still do not have a reliable solution
  // so just allow everything that is reachable.

  // plugins need to be installed in TeXmacs.app/Contents/Plugins
  QCoreApplication::addLibraryPath (QDir::cleanPath (
      QCoreApplication::applicationDirPath ().append ("/../Plugins")));
  // cout << from_qstring ( QCoreApplication::libraryPaths () .join("\n") ) <<
  // LF;
  {
    // ensure that private versions of the Qt frameworks have priority on
    // other instances.
    // in the event that we load qt plugins which could possibly link to
    // other instances of the Qt libraries
    string buf;
    buf= as_string (exedir * "../Frameworks");
    if (get_env ("DYLD_FRAMEWORK_PATH") != "")
      buf= buf * ":" * get_env ("DYLD_FRAMEWORK_PATH");
    set_env ("DYLD_FRAMEWORK_PATH", buf);
    buf= as_string (exedir * "../Resources/lib");
    if (get_env ("DYLD_LIBRARY_PATH") != "")
      buf= buf * ":" * get_env ("DYLD_LIBRARY_PATH");
    set_env ("DYLD_LIBRARY_PATH", buf);
  }
#endif

#if defined(AQUATEXMACS) || defined(Q_OS_MAC) ||                               \
    (defined(X11TEXMACS) && defined(MACOSX_EXTENSIONS))
  // Mac bundle environment initialization
  // We set some environment variables when the executable
  // is in a .app bundle on MacOSX
  if (is_empty (current_texmacs_path))
    set_env ("TEXMACS_PATH", as_string (exedir * "../Resources/share/Xmacs"));
  // cout << get_env("PATH") * ":" * as_string(url("$PWD") * argv[0]
  //  * "../../Resources/share/TeXmacs/bin") << LF;
  if (exists ("/bin/bash")) {
    string shell_env= var_eval_system ("PATH='' /bin/bash -l -c 'echo $PATH'");
    set_env ("PATH", get_env ("PATH") * ":" * shell_env * ":" *
                         as_string (exedir * "../Resources/share/TeXmacs/bin"));
  }
  else {
    set_env ("PATH", get_env ("PATH") * ":" *
                         as_string (exedir * "../Resources/share/TeXmacs/bin"));
  }
  // system("set");
#endif

#if defined(OS_MINGW) || defined(OS_WIN)
  // Win bundle environment initialization
  // TEXMACS_PATH is set by assuming that the executable is in TeXmacs/bin/
  // HOME is set to USERPROFILE
  // PWD is set to HOME
  // if PWD is lacking, then the path resolution machinery may not work

  if (is_empty (current_texmacs_path))
    set_env ("TEXMACS_PATH", as_string (exedir * ".."));
  // if (get_env ("HOME") == "") //now set in immediate_options otherwise
  // --setup option fails
  //   set_env ("HOME", get_env("USERPROFILE"));
  // HACK
  // In WINE the variable PWD is already in the outer Unix environment
  // so we need to override it to have a correct behaviour
  if ((get_env ("PWD") == "") || (get_env ("PWD")[0] == '/')) {
    set_env ("PWD", as_string (exedir));
    // set_env ("PWD", get_env("HOME"));
  }
  // system("set");
#endif

#ifdef OS_HAIKU
  // Initialization inside the Haiku package management environment
  // TEXMACS_PATH is set relative to the executable which is in $prefix/app
  // to $prefix/data/TeXmacs

  if (is_empty (current_texmacs_path))
    set_env ("TEXMACS_PATH", as_string (exedir * "../data/TeXmacs"));

  set_env ("PATH", get_env ("PATH") * ":" *
                       as_string (exedir * "/system/lib/TeXmacs/bin"));
#endif

#ifdef OS_WASM
  set_env ("PWD", "/");
  set_env ("HOME", "/");
  if (is_empty (current_texmacs_path)) set_env ("TEXMACS_PATH", "/TeXmacs");
#endif

  // check on the latest $TEXMACS_PATH
  current_texmacs_path= get_env ("TEXMACS_PATH");
  if (is_empty (current_texmacs_path) ||
      !exists (url_system (current_texmacs_path))) {
    cout << "The required TEXMACS_PATH(" << current_texmacs_path
         << ") does not exists" << LF;
    exit (1);
  }
}

/******************************************************************************
 * Real main program for encaptulation of guile
 ******************************************************************************/

void
TeXmacs_main (int argc, char** argv) {
  int    i;
  bool   flag= true;
  string the_default_font;
  for (i= 1; i < argc; i++)
    if (argv[i][0] == '\0') argc= i;
    else if (((argv[i][0] == '-') || (argv[i][0] == '+')) &&
             (argv[i][1] != '\0')) {
      string s= argv[i];
      if ((N (s) >= 2) && (s (0, 2) == "--")) s= s (1, N (s));
      if ((s == "-s") || (s == "-silent")) flag= false;
      else if ((s == "-q") || (s == "-quit"))
        my_init_cmds= my_init_cmds * " (quit-TeXmacs)";
      else if ((s == "-c") || (s == "-convert")) {
        i+= 2;
        if (i < argc) {
          url in ("$PWD", argv[i - 1]);
          url out ("$PWD", argv[i]);
          my_init_cmds= my_init_cmds * " " * "(load-buffer " *
                        scm_quote (as_string (in)) * " :strict) " *
                        "(export-buffer " *
                        scm_quote (as_string (out) * string (".html")) * ")";
        }
      }
      else if (s == "-dir") {
        cout << "[DEBUG] convert dir to dir" << LF;
        i+= 2;
        if (i < argc) {
          url in ("$PWD", argv[i - 1]);
          url out ("$PWD", argv[i]);
          cout << "[DEBUG] in: " << as_string (in) << LF;
          cout << "[DEBUG] out: " << as_string (out) << LF;
          my_init_cmds= my_init_cmds * " " * "(begin (tmweb-convert-dir " *
                        scm_quote (as_string (in)) * " " *
                        scm_quote (as_string (out)) * "))";
        }
      }
    }
  // if (flag) {
  //   debug (DEBUG_FLAG_AUTO, true);
  //   cout << "[DEBUG] TeXmacs started" << LF;
  // }

  // Further options via environment variables
  //   if (get_env ("TEXMACS_RETINA") == "off") {
  //     cout << "[DEBUG] retina off" << LF;
  //     retina_manual= true;
  //     retina_factor= 1;
  //     retina_icons = 1;
  //     retina_scale = 1.0;
  //   }
  //   if (get_env ("TEXMACS_RETINA") == "on") {
  //     cout << "[DEBUG] retina on" << LF;
  //     retina_manual= true;
  // #ifdef MACOSX_EXTENSIONS
  //     retina_factor= 2;
  //     retina_zoom  = 1;
  //     retina_scale = 1.4;
  // #else
  //     retina_factor= 1;
  //     retina_zoom  = 2;
  //     retina_scale = (tm_style_sheet == "" ? 1.0 : 1.6666);
  // #endif
  //     retina_icons= 2;
  //   }
  // if (get_env ("TEXMACS_RETINA_ICONS") == "off") {
  //   cout << "[DEBUG] retina icons off" << LF;
  //   retina_iman = true;
  //   retina_icons= 1;
  // }
  // if (get_env ("TEXMACS_RETINA_ICONS") == "on") {
  //   cout << "[DEBUG] retina icons on" << LF;
  //   retina_iman = true;
  //   retina_icons= 2;
  // }
  // End options via environment variables

  // Further user preferences
  string native= string ("off");
  string unify = string ("off");
  string mini  = (os_macos () ? string ("off") : string ("on"));
  if (tm_style_sheet != "") mini= "off";
  use_native_menubar = get_preference ("use native menubar", native) == "on";
  use_unified_toolbar= get_preference ("use unified toolbar", unify) == "on";
  use_mini_bars      = get_preference ("use minibars", mini) == "on";
  if (!use_native_menubar) use_unified_toolbar= false;
  // End user preferences

  bench_start ("initialize plugins");
  init_plugins ();
  bench_cumul ("initialize plugins");

  gui_open (argc, argv);
  set_default_font (the_default_font);
  server sv (app_type::TM2HTML);
  string where= "";
  for (i= 1; i < argc; i++) {
    if (argv[i] == NULL) break;
    string s= argv[i];
    if ((N (s) >= 2) && (s (0, 2) == "--")) s= s (1, N (s));
    if ((s[0] != '-') && (s[0] != '+')) {
      url u= url_system (s);
      if (!is_rooted (u)) u= resolve (url_pwd (), "") * u;
      string b  = scm_quote (as_string (u));
      string cmd= "(load-buffer " * b * " " * where * ")";
      where     = " :new-window";
      exec_delayed (scheme_cmd (cmd));
    }
    if ((s == "-c") || (s == "-convert")) i+= 2;
    else if ((s == "-b") || (s == "-initialize-buffer") || (s == "-fn") ||
             (s == "-font") || (s == "-i") || (s == "-initialize") ||
             (s == "-g") || (s == "-geometry") || (s == "-x") ||
             (s == "-execute") || (s == "-log-file") ||
             (s == "-build-manual") || (s == "-reference-suite") ||
             (s == "-test-suite")) {
      i++;
    }
  }
  if (install_status == 1) {
    string cmd= "(load-help-article \"about/welcome/new-welcome\")";
    // FIXME: force to load welcome message into new window
    exec_delayed (scheme_cmd (cmd));
  }
  else if (install_status == 2) {
    url    u  = "tmfs://help/plain/tm/doc/about/changes/changes-recent.en.tm";
    string b  = scm_quote (as_string (u));
    string cmd= "(load-buffer " * b * " " * where * ")";
    where     = " :new-window";
    exec_delayed (scheme_cmd (cmd));
  }
  if (number_buffers () == 0) {
    open_window ();
  }

  bench_print (std_bench);
  bench_reset ("initialize texmacs");
  bench_reset ("initialize plugins");
  bench_reset ("initialize scheme");

#ifdef QTTEXMACS
  if (!headless_mode) init_style_sheet (qtmapp);
#endif

  texmacs_started= true;
  if (!disable_error_recovery) signal (SIGSEGV, clean_exit_on_segfault);
  if (start_server_flag) server_start ();
  release_boot_lock ();
  if (N (extra_init_cmd) > 0) exec_delayed (scheme_cmd (extra_init_cmd));
  gui_start_loop ();
  gui_close ();

#if defined(X11TEXMACS) && defined(MACOSX_EXTENSIONS)
  finalize_mac_application ();
#endif

  if (DEBUG_STD) debug_boot << "Good bye...\n";
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
    if ((s == "-S") || (s == "-setup")) {
      cout << "[DEBUG] setup" << LF;
      remove (url ("$TEXMACS_HOME_PATH/system/settings.scm"));
      remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
      remove (url ("$TEXMACS_HOME_PATH/system/cache") * url_wildcard ("*"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-database.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-features.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-characteristics.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
    }
    if (s == "-headless") {
      headless_mode= true;
      cout << "[DEBUG] headless mode" << LF;
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
#if  !(defined OS_MACOS || defined OS_WASM)
  set_env ("QT_QPA_PLATFORM", "xcb");
  set_env ("XDG_SESSION_TYPE", "x11");
#endif
#endif
#ifdef MACOSX_EXTENSIONS
  // Reset TeXmacs if Alt is pressed during startup
  if (mac_alternate_startup ()) {
    cout << "TeXmacs] Performing setup (Alt on startup)" << LF;
    remove (url ("$TEXMACS_HOME_PATH/system/settings.scm"));
    remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
    remove (url ("$TEXMACS_HOME_PATH/system/cache") * url_wildcard ("*"));
    remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
  }
#endif
#ifdef QTTEXMACS
  // initialize the Qt application infrastructure
  if (headless_mode) qtmcoreapp= new QTMCoreApplication (argc, argv);
  else qtmapp= new QTMApplication (argc, argv);
#endif
  TeXmacs_init_paths (argc, argv);
#ifdef QTTEXMACS
  if (!headless_mode) qtmapp->set_window_icon ("/misc/images/texmacs-512.png");
#endif
  // cout << "Bench  ] Started TeXmacs\n";
  the_et     = tuple ();
  the_et->obs= ip_observer (path ());
  cache_initialize ();
  bench_start ("initialize texmacs");
  init_texmacs ();
  bench_cumul ("initialize texmacs");
#ifdef ENABLE_TESTS
  cout << "[DEBUG] Started tests\n";
  test_routines ();
#endif
  // #ifdef EXPERIMENTAL
  //   test_environments ();
  // #endif
  start_scheme (argc, argv, TeXmacs_main);
#ifdef QTTEXMACS
  if (headless_mode) delete qtmcoreapp;
  else delete qtmapp;
#endif
  return 0;
}
