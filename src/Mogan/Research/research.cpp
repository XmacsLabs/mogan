
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
      else if ((s == "-V") || (s == "-verbose"))
        debug (DEBUG_FLAG_VERBOSE, true);
      else if ((s == "-d") || (s == "-debug")) debug (DEBUG_FLAG_STD, true);
      else if (s == "-debug-events") debug (DEBUG_FLAG_EVENTS, true);
      else if (s == "-debug-io") debug (DEBUG_FLAG_IO, true);
      else if (s == "-debug-bench") debug (DEBUG_FLAG_BENCH, true);
      else if (s == "-debug-history") debug (DEBUG_FLAG_HISTORY, true);
      else if (s == "-debug-qt") debug (DEBUG_FLAG_QT, true);
      else if (s == "-debug-qt-widgets") debug (DEBUG_FLAG_QT_WIDGETS, true);
      else if (s == "-debug-keyboard") debug (DEBUG_FLAG_KEYBOARD, true);
      else if (s == "-debug-packrat") debug (DEBUG_FLAG_PACKRAT, true);
      else if (s == "-debug-flatten") debug (DEBUG_FLAG_FLATTEN, true);
      else if (s == "-debug-parser") debug (DEBUG_FLAG_PARSER, true);
      else if (s == "-debug-correct") debug (DEBUG_FLAG_CORRECT, true);
      else if (s == "-debug-convert") debug (DEBUG_FLAG_CONVERT, true);
      else if (s == "-debug-remote") debug (DEBUG_FLAG_REMOTE, true);
      else if (s == "-debug-live") debug (DEBUG_FLAG_LIVE, true);
      else if (s == "-debug-all") {
        debug (DEBUG_FLAG_EVENTS, true);
        debug (DEBUG_FLAG_STD, true);
        debug (DEBUG_FLAG_IO, true);
        debug (DEBUG_FLAG_HISTORY, true);
        debug (DEBUG_FLAG_BENCH, true);
        debug (DEBUG_FLAG_QT, true);
        debug (DEBUG_FLAG_QT_WIDGETS, true);
      }
      else if (s == "-disable-error-recovery") disable_error_recovery= true;
      else if ((s == "-fn") || (s == "-font")) {
        i++;
        if (i < argc) the_default_font= argv[i];
      }
      else if ((s == "-g") || (s == "-geometry")) {
        i++;
        if (i < argc) {
          string g= argv[i];
          int    j= 0, j1, j2, j3;
          for (j= 0; j < N (g); j++)
            if (g[j] == 'x') break;
          j1= j;
          if (j < N (g)) j++;
          for (; j < N (g); j++)
            if ((g[j] == '+') || (g[j] == '-')) break;
          j2= j;
          if (j < N (g)) j++;
          for (; j < N (g); j++)
            if ((g[j] == '+') || (g[j] == '-')) break;
          j3= j;
          if (j1 < N (g)) {
            geometry_w= max (as_int (g (0, j1)), 320);
            geometry_h= max (as_int (g (j1 + 1, j2)), 200);
          }
          if (j3 < N (g)) {
            if (g[j2] == '-') geometry_x= as_int (g (j2, j3)) - 1;
            else geometry_x= as_int (g (j2 + 1, j3));
            if (g[j3] == '-') geometry_y= as_int (g (j3, N (g))) - 1;
            else geometry_y= as_int (g (j3 + 1, N (g)));
          }
        }
      }
      else if ((s == "-b") || (s == "-initialize-buffer")) {
        i++;
        if (i < argc) tm_init_buffer_file= url_system (argv[i]);
      }
      else if ((s == "-i") || (s == "-initialize")) {
        i++;
        if (i < argc) tm_init_file= url_system (argv[i]);
      }
      else if ((s == "-v") || (s == "-version")) {
        cout << "\n";
        cout << "TeXmacs version " << TEXMACS_VERSION << "\n";
        cout << "SVN version " << TEXMACS_REVISION << "\n";
        cout << TEXMACS_COPYRIGHT << "\n";
        cout << "\n";
        exit (0);
      }
      else if ((s == "-p") || (s == "-path")) {
        cout << get_env ("TEXMACS_PATH") << "\n";
        exit (0);
      }
      else if ((s == "-bp") || (s == "-binpath")) {
        cout << get_env ("TEXMACS_BIN_PATH") << "\n";
        exit (0);
      }
      else if ((s == "-q") || (s == "-quit"))
        my_init_cmds= my_init_cmds * " (quit-TeXmacs)";
      else if ((s == "-r") || (s == "-reverse")) set_reverse_colors (true);
      else if (s == "-no-retina") {
        retina_manual= true;
        retina_factor= 1;
        retina_zoom  = 1;
        retina_icons = 1;
        retina_scale = 1.0;
      }
      else if ((s == "-R") || (s == "-retina")) {
        retina_manual= true;
#ifdef MACOSX_EXTENSIONS
        retina_factor= 2;
        retina_zoom  = 1;
        retina_scale = 1.4;
#elif defined(OS_WASM)
        retina_factor= 2;
        retina_zoom  = 2;
        retina_scale = (tm_style_sheet == "" ? 1.0 : 1.6666);
#else
        retina_factor= 1;
        retina_zoom  = 2;
        retina_scale = (tm_style_sheet == "" ? 1.0 : 1.6666);
#endif
        retina_icons= 2;
      }
      else if (s == "-no-retina-icons") {
        retina_iman = true;
        retina_icons= 1;
      }
      else if (s == "-retina-icons") {
        retina_iman = true;
        retina_icons= 2;
      }
      else if ((s == "-c") || (s == "-convert")) {
        i+= 2;
        if (i < argc) {
          url in ("$PWD", argv[i - 1]);
          url out ("$PWD", argv[i]);
          my_init_cmds= my_init_cmds * " " * "(load-buffer " *
                        scm_quote (as_string (in)) * " :strict) " *
                        "(export-buffer " * scm_quote (as_string (out)) * ")";
        }
      }
      else if ((s == "-x") || (s == "-execute")) {
        i++;
        if (i < argc) my_init_cmds= (my_init_cmds * " ") * argv[i];
      }
      else if (s == "-server") start_server_flag= true;
      else if (s == "-log-file") i++;
      else if ((s == "-Oc") || (s == "-no-char-clipping")) char_clip= false;
      else if ((s == "+Oc") || (s == "-char-clipping")) char_clip= true;
      else if ((s == "-S") || (s == "-setup") || (s == "-delete-cache") ||
               (s == "-delete-font-cache") || (s == "-delete-style-cache") ||
               (s == "-delete-file-cache") || (s == "-delete-doc-cache") ||
               (s == "-delete-plugin-cache") || (s == "-delete-server-data") ||
               (s == "-delete-databases") || (s == "-headless"))
        ;
      else if (s == "-build-manual") {
        if ((++i) < argc)
          extra_init_cmd << "(build-manual " << scm_quote (argv[i])
                         << " delayed-quit)";
      }
      else if (s == "-reference-suite") {
        if ((++i) < argc)
          extra_init_cmd << "(build-ref-suite " << scm_quote (argv[i])
                         << " delayed-quit)";
      }
      else if (s == "-test-suite") {
        if ((++i) < argc)
          extra_init_cmd << "(run-test-suite " << scm_quote (argv[i])
                         << "delayed-quit)";
      }
      else if (starts (s, "-psn"))
        ;
      else {
        cout << "\n";
        cout << "Options for TeXmacs:\n\n";
        cout << "  -b [file]  Specify scheme buffers initialization file\n";
        cout << "  -c [i] [o] Convert file 'i' into file 'o'\n";
        cout << "  -d         For debugging purposes\n";
        cout << "  -fn [font] Set the default TeX font\n";
        cout << "  -g [geom]  Set geometry of window in pixels\n";
        cout << "  -h         Display this help message\n";
        cout << "  -i [file]  Specify scheme initialization file\n";
        cout << "  -p         Get the TeXmacs path\n";
        cout << "  -q         Shortcut for -x \"(quit-TeXmacs)\"\n";
        cout << "  -r         Reverse video mode\n";
        cout << "  -s         Suppress information messages\n";
        cout << "  -S         Rerun TeXmacs setup program before starting\n";
        cout << "  -v         Display current TeXmacs version\n";
        cout << "  -V         Show some informative messages\n";
        cout << "  -x [cmd]   Execute scheme command\n";
        cout << "  -Oc        TeX characters bitmap clipping off\n";
        cout << "  +Oc        TeX characters bitmap clipping on (default)\n";
        cout << "\nPlease report bugs to <bugs@texmacs.org>\n";
        cout << "\n";
        exit (0);
      }
    }
  if (flag) debug (DEBUG_FLAG_AUTO, true);

  // Further options via environment variables
  if (get_env ("TEXMACS_RETINA") == "off") {
    retina_manual= true;
    retina_factor= 1;
    retina_icons = 1;
    retina_scale = 1.0;
  }
  if (get_env ("TEXMACS_RETINA") == "on") {
    retina_manual= true;
#ifdef MACOSX_EXTENSIONS
    retina_factor= 2;
    retina_zoom  = 1;
    retina_scale = 1.4;
#else
    retina_factor= 1;
    retina_zoom  = 2;
    retina_scale = (tm_style_sheet == "" ? 1.0 : 1.6666);
#endif
    retina_icons= 2;
  }
  if (get_env ("TEXMACS_RETINA_ICONS") == "off") {
    retina_iman = true;
    retina_icons= 1;
  }
  if (get_env ("TEXMACS_RETINA_ICONS") == "on") {
    retina_iman = true;
    retina_icons= 2;
  }
  // End options via environment variables

  // Further user preferences
  string native= (gui_version () == "qt4" ? string ("on") : string ("off"));
  string unify = (gui_version () == "qt4" ? string ("on") : string ("off"));
  string mini  = (os_macos () ? string ("off") : string ("on"));
  if (tm_style_sheet != "") mini= "off";
  use_native_menubar = get_preference ("use native menubar", native) == "on";
  use_unified_toolbar= get_preference ("use unified toolbar", unify) == "on";
  use_mini_bars      = get_preference ("use minibars", mini) == "on";
  if (!use_native_menubar) use_unified_toolbar= false;
  // End user preferences

  if (DEBUG_STD) debug_boot << "Installing internal plug-ins...\n";
  bench_start ("initialize plugins");
  init_plugins ();
  bench_cumul ("initialize plugins");
  if (DEBUG_STD) debug_boot << "Opening display...\n";

#if defined(X11TEXMACS) && defined(MACOSX_EXTENSIONS)
    // init_mac_application ();
#endif

  gui_open (argc, argv);
  set_default_font (the_default_font);
  if (DEBUG_STD) debug_boot << "Starting server...\n";
  { // opening scope for server sv
    server sv (app_type::RESEARCH);
    string where= "";
    for (i= 1; i < argc; i++) {
      if (argv[i] == NULL) break;
      string s= argv[i];
      if ((N (s) >= 2) && (s (0, 2) == "--")) s= s (1, N (s));
      if ((s[0] != '-') && (s[0] != '+')) {
        if (DEBUG_STD) debug_boot << "Loading " << s << "...\n";
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
      if (DEBUG_STD) debug_boot << "Loading welcome message...\n";
      string cmd= "(load-help-article \"about/welcome/new-welcome\")";
      // FIXME: force to load welcome message into new window
      exec_delayed (scheme_cmd (cmd));
    }
    else if (install_status == 2) {
      if (DEBUG_STD) debug_boot << "Loading upgrade message...\n";
      url    u  = "tmfs://help/plain/tm/doc/about/changes/changes-recent.en.tm";
      string b  = scm_quote (as_string (u));
      string cmd= "(load-buffer " * b * " " * where * ")";
      where     = " :new-window";
      exec_delayed (scheme_cmd (cmd));
    }
    if (number_buffers () == 0) {
      if (DEBUG_STD) debug_boot << "Creating 'no name' buffer...\n";
      open_window ();
    }

    bench_print (std_bench);
    bench_reset ("initialize texmacs");
    bench_reset ("initialize plugins");
    bench_reset ("initialize scheme");

#ifdef QTTEXMACS
    if (!headless_mode) init_style_sheet (qtmapp);
#endif

    if (DEBUG_STD) debug_boot << "Starting event loop...\n";
    texmacs_started= true;
    if (!disable_error_recovery) signal (SIGSEGV, clean_exit_on_segfault);
    if (start_server_flag) server_start ();
    release_boot_lock ();
    if (N (extra_init_cmd) > 0) exec_delayed (scheme_cmd (extra_init_cmd));
    gui_start_loop ();

    if (DEBUG_STD) debug_boot << "Stopping server...\n";
  } // ending scope for server sv

  if (DEBUG_STD) debug_boot << "Closing display...\n";
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
  init_texmacs_home_path (argc, argv);
  if (is_empty (get_env ("TEXMACS_HOME_PATH"))) return;
  for (int i= 1; i < argc; i++) {
    string s= argv[i];
    if ((N (s) >= 2) && (s (0, 2) == "--")) s= s (1, N (s));
    if ((s == "-S") || (s == "-setup")) {
      remove (url ("$TEXMACS_HOME_PATH/system/settings.scm"));
      remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
      remove (get_tm_cache_path () * url_wildcard ("*"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-database.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-features.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-characteristics.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
    }
    else if (s == "-delete-cache")
      remove (get_tm_cache_path () * url_wildcard ("*"));
    else if (s == "-delete-style-cache")
      remove (get_tm_cache_path () * url_wildcard ("__*"));
    else if (s == "-delete-font-cache") {
      remove (get_tm_cache_path () * url ("font_cache.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-database.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-features.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-characteristics.scm"));
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
