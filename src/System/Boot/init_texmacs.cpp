
/******************************************************************************
* MODULE     : init_texmacs.cpp
* DESCRIPTION: Initialization of TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boot.hpp"
#include "preferences.hpp"
#include "file.hpp"
#include "tm_file.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "convert.hpp"
#include "block.hpp"
#include "merge_sort.hpp"
#include "drd_std.hpp"
#include "language.hpp"
#ifdef OS_WIN
#include <process.h>
#else
#include <unistd.h>
#endif
#ifdef OS_MINGW
#include <time.h>
#include <direct.h>
#endif
#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#include <QCoreApplication>
#include <QDir>
#endif
#include "Metafont/load_tex.hpp"

int  install_status   = 0;

/******************************************************************************
 * Texmacs paths
 ******************************************************************************/
void
init_texmacs_path (int& argc, char** argv) {
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

#ifdef OS_GNU_LINUX
  if (is_empty (current_texmacs_path) && exists (exedir * "../share/Xmacs")) {
    set_env ("TEXMACS_PATH", as_string (exedir * "../share/Xmacs"));
  }
#endif

#ifdef OS_MACOS
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

#if defined(AQUATEXMACS) || defined(OS_MACOS) || \
    (defined(X11TEXMACS) && defined(MACOSX_EXTENSIONS))
  // Mac bundle environment initialization
  // We set some environment variables when the executable
  // is in a .app bundle on MacOSX
  if (is_empty (current_texmacs_path)) {
    set_env ("TEXMACS_PATH", as_string (exedir * "../Resources/share/Xmacs"));
  }
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

void
init_texmacs_home_path (int& argc, char** argv) {
  if (!is_empty (get_env ("TEXMACS_HOME_PATH"))) return;

  if (os_mingw () || os_win ()) {
    set_env ("TEXMACS_HOME_PATH", get_env ("APPDATA") * "\\LiiiLabs");
  } else if (os_macos ()) {
    set_env ("TEXMACS_HOME_PATH", get_env ("HOME") * "/Library/Application Support/LiiiLabs");
  } else if (os_wasm ()) {
    set_env ("TEXMACS_HOME_PATH", "/.LiiiLabs");
  } else {
#if defined(OS_HAIKU)
    set_env ("TEXMACS_HOME_PATH", get_env ("HOME") * "/config/settings/TeXmacs");
#else
    string xdg_data_home= get_env ("XDG_DATA_HOME");
    if (is_empty (xdg_data_home))
      xdg_data_home= get_env ("HOME") * "/.local/share";
    set_env ("TEXMACS_HOME_PATH", xdg_data_home * "/LiiiLabs");
#endif
  }
}


/******************************************************************************
* Subroutines for paths
******************************************************************************/

static url
get_env_path (string which) {
  return url ("$" * which);
}

static void
set_env_path (string which, url val) {
  //cout << which << " := " << val << "\n";
  if ((!is_none (val)) && (val->t != ""))
    set_env (which, as_string (val));
}

static url
get_env_path (string which, url def) {
  url val= get_env_path (which);
  if (is_none (val) || (val->t == "")) {
    set_env_path (which, def);
    return def;
  }
  return val;
}

static url
plugin_path (string which) {
  url base= "$TEXMACS_HOME_PATH:/etc/TeXmacs:$TEXMACS_PATH:/usr/share/TeXmacs";
  url search= base * "plugins" * url_wildcard ("*") * which;
  return expand (complete (search, "r"));
}

scheme_tree
plugin_list () {
  bool flag;
  array<string> a= read_directory ("$TEXMACS_PATH/plugins", flag);
  a << read_directory ("/etc/TeXmacs/plugins", flag);
  a << read_directory ("$TEXMACS_HOME_PATH/plugins", flag);
  a << read_directory ("/usr/share/TeXmacs/plugins", flag);
  merge_sort (a);
  int i, n= N(a);
  tree t (TUPLE);
  for (i=0; i<n; i++)
    if ((a[i] != ".") && (a[i] != "..") && ((i==0) || (a[i] != a[i-1])))
      t << a[i];
  return t;
}

/******************************************************************************
* Initialize main paths
******************************************************************************/

static void
init_main_paths () {
#if defined(OS_MINGW) || defined(OS_WIN)
  if (is_none (get_env_path ("TEXMACS_HOME_PATH", get_env ("APPDATA") * "/LiiiLabs"))) {
#else
  if (is_none (get_env_path ("TEXMACS_HOME_PATH", "~/.TeXmacs"))) {
#endif
    boot_error << "\n";
    boot_error << "Installation problem: please send a bug report.\n";
    boot_error << "'TEXMACS_HOME_PATH' could not be set to '~/.TeXmacs'.\n";
    boot_error << "You may try to set this environment variable manually\n";
    boot_error << "\n";
    TM_FAILED ("installation problem");
    exit (1);
  }
}

/******************************************************************************
* Directory for temporary files
******************************************************************************/

bool
process_running (int pid) {
  string cmd= "ps -p " * as_string (pid);
  string ret= eval_system (cmd);
  return occurs ("texmacs", ret) && occurs (as_string (pid), ret);
}

static void
clean_temp_dirs () {
  bool err= false;
  url main_tmp_dir= url_system ("$TMP") * url (".lolly");
  array<string> a= read_directory (main_tmp_dir, err);
  for (int i=0; i<N(a); i++)
    if (is_int (a[i]))
      if (!process_running (as_int (a[i])))
        if (a[i] != as_string ((int) getpid ()))
          rmdir (url (main_tmp_dir) * url (a[i]));
}

/******************************************************************************
* Make user directories
******************************************************************************/

static void
init_user_dirs () {
  make_dir ("$TEXMACS_HOME_PATH");
  make_dir ("$TEXMACS_HOME_PATH/bin");
  make_dir ("$TEXMACS_HOME_PATH/doc");
  make_dir ("$TEXMACS_HOME_PATH/doc/about");
  make_dir ("$TEXMACS_HOME_PATH/doc/about/changes");
  make_dir ("$TEXMACS_HOME_PATH/fonts");
  make_dir ("$TEXMACS_HOME_PATH/fonts/enc");
  make_dir ("$TEXMACS_HOME_PATH/fonts/error");
  make_dir ("$TEXMACS_HOME_PATH/fonts/pk");
  make_dir ("$TEXMACS_HOME_PATH/fonts/tfm");
  make_dir ("$TEXMACS_HOME_PATH/fonts/truetype");
  make_dir ("$TEXMACS_HOME_PATH/fonts/type1");
  make_dir ("$TEXMACS_HOME_PATH/fonts/unpacked");
  make_dir ("$TEXMACS_HOME_PATH/fonts/virtual");
  make_dir ("$TEXMACS_HOME_PATH/langs");
  make_dir ("$TEXMACS_HOME_PATH/langs/mathematical");
  make_dir ("$TEXMACS_HOME_PATH/langs/mathematical/syntax");
  make_dir ("$TEXMACS_HOME_PATH/langs/natural");
  make_dir ("$TEXMACS_HOME_PATH/langs/natural/dic");
  make_dir ("$TEXMACS_HOME_PATH/langs/natural/hyphen");
  make_dir ("$TEXMACS_HOME_PATH/langs/programming");
  make_dir ("$TEXMACS_HOME_PATH/misc");
  make_dir ("$TEXMACS_HOME_PATH/misc/patterns");
  make_dir ("$TEXMACS_HOME_PATH/misc/pixmaps");
  make_dir ("$TEXMACS_HOME_PATH/misc/themes");
  make_dir ("$TEXMACS_HOME_PATH/packages");
  make_dir ("$TEXMACS_HOME_PATH/plugins");
  make_dir ("$TEXMACS_HOME_PATH/progs");
  make_dir ("$TEXMACS_HOME_PATH/server");
  make_dir ("$TEXMACS_HOME_PATH/styles");
  make_dir ("$TEXMACS_HOME_PATH/system");
  make_dir ("$TEXMACS_HOME_PATH/system/bib");
  make_dir (get_tm_cache_path ());
  make_dir ("$TEXMACS_HOME_PATH/system/database");
  make_dir ("$TEXMACS_HOME_PATH/system/database/bib");
  make_dir ("$TEXMACS_HOME_PATH/system/make");
  make_dir ("$TEXMACS_HOME_PATH/system/tmp");
  make_dir ("$TEXMACS_HOME_PATH/texts");
  make_dir ("$TEXMACS_HOME_PATH/users");
  clean_temp_dirs ();
}

/******************************************************************************
* Boot locks
******************************************************************************/

static void
acquire_boot_lock () {
  //cout << "Acquire lock\n";
  url lock_file= "$TEXMACS_HOME_PATH/system/boot_lock";
  if (exists (lock_file)) {
    remove (url ("$TEXMACS_HOME_PATH/system/settings.scm"));
    remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
    remove (get_tm_cache_path () * url_wildcard ("*"));
    remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));    
  }
  else save_string (lock_file, "", false);
}

void
release_boot_lock () {
  //cout << "Release lock\n";
  url lock_file= "$TEXMACS_HOME_PATH/system/boot_lock";
  remove (lock_file);
}

/******************************************************************************
* Detection of scheme code
******************************************************************************/

static void
init_scheme() {
  url guile_path= "$TEXMACS_PATH/progs:$GUILE_LOAD_PATH";
  guile_path= guile_path | "$TEXMACS_HOME_PATH/progs" | plugin_path ("progs");
  set_env_path ("GUILE_LOAD_PATH", guile_path);
}

/******************************************************************************
* Set additional environment variables
******************************************************************************/

static void
init_env_vars () {
  // Handle binary, library and guile paths for plugins
  url bin_path= get_env_path ("PATH") | plugin_path ("bin");
#if defined(OS_MINGW) || defined(OS_WIN)
  bin_path= bin_path | url ("$TEXMACS_PATH/bin");
  if (has_user_preference ("manual path"))
    bin_path= url_system (get_user_preference ("manual path")) | bin_path;
#endif

  set_env_path ("PATH", bin_path);
  url lib_path= get_env_path ("LD_LIBRARY_PATH") | plugin_path ("lib");
  set_env_path ("LD_LIBRARY_PATH", lib_path);

  // Get TeXmacs style and package paths
  url style_root=
    get_env_path ("TEXMACS_STYLE_ROOT",
                  "$TEXMACS_HOME_PATH/styles:$TEXMACS_PATH/styles" |
                  plugin_path ("styles"));
  url package_root=
    get_env_path ("TEXMACS_PACKAGE_ROOT",
                  "$TEXMACS_HOME_PATH/packages:$TEXMACS_PATH/packages" |
                  plugin_path ("packages"));
  url all_root= style_root | package_root;
  url style_path=
    get_env_path ("TEXMACS_STYLE_PATH",
                  search_sub_dirs (all_root));
  url text_root=
    get_env_path ("TEXMACS_TEXT_ROOT",
                  "$TEXMACS_HOME_PATH/texts:$TEXMACS_PATH/texts" |
                  plugin_path ("texts"));
  url text_path=
    get_env_path ("TEXMACS_TEXT_PATH",
                  search_sub_dirs (text_root));

  // Get other data paths
  (void) get_env_path ("TEXMACS_FILE_PATH",text_path | style_path);
  (void) set_env_path ("TEXMACS_DOC_PATH",
                       get_env_path ("TEXMACS_DOC_PATH") |
                       "$TEXMACS_HOME_PATH/doc:$TEXMACS_PATH/doc" |
                       plugin_path ("doc"));
  (void) set_env_path ("TEXMACS_SECURE_PATH",
                       get_env_path ("TEXMACS_SECURE_PATH") |
                       "$TEXMACS_PATH:$TEXMACS_HOME_PATH");
  (void) get_env_path ("TEXMACS_PATTERN_PATH",
                       "$TEXMACS_HOME_PATH/misc/patterns" |
                       url ("$TEXMACS_PATH/misc/patterns") |
                       url ("$TEXMACS_PATH/misc/pictures") |
                       plugin_path ("misc/patterns"));
  (void) get_env_path ("TEXMACS_PIXMAP_PATH",
                       "$TEXMACS_HOME_PATH/misc/pixmaps" |
                       url ("$TEXMACS_PATH/misc/pixmaps/modern/32x32/settings") |
                       url ("$TEXMACS_PATH/misc/pixmaps/modern/24x24/main") |
                       url ("$TEXMACS_PATH/misc/pixmaps/modern/20x20/mode") |
                       url ("$TEXMACS_PATH/misc/pixmaps/modern/16x16/focus") |
                       plugin_path ("misc/pixmaps"));
  (void) get_env_path ("TEXMACS_DIC_PATH",
                       "$TEXMACS_HOME_PATH/langs/natural/dic" |
                       url ("$TEXMACS_PATH/langs/natural/dic"));
  (void) get_env_path ("TEXMACS_THEME_PATH",
                       url ("$TEXMACS_PATH/misc/themes") |
                       url ("$TEXMACS_HOME_PATH/misc/themes") |
                       plugin_path ("misc/themes"));
#ifdef OS_WIN
  set_env ("TEXMACS_SOURCE_PATH", "");
#else
  set_env ("TEXMACS_SOURCE_PATH", TEXMACS_SOURCES);
#endif
}

/******************************************************************************
* Miscellaneous initializations
******************************************************************************/

static void
init_misc () {
  // Set extra environment variables for Cygwin
#ifdef OS_CYGWIN
  set_env ("CYGWIN", "check_case:strict");
  set_env ("COMSPEC", "");
  set_env ("ComSpec", "");
#endif

}

/******************************************************************************
* First installation
******************************************************************************/

void
setup_texmacs () {
  url settings_file= "$TEXMACS_HOME_PATH/system/settings.scm";
  debug_boot << "Welcome to TeXmacs " TEXMACS_VERSION "\n";
  debug_boot << HRULE;

  set_setting ("VERSION", TEXMACS_VERSION);
  setup_tex ();
  
  string s= scheme_tree_to_block (texmacs_settings);
  //cout << "settings_t= " << texmacs_settings << "\n";
  //cout << "settings_s= " << s << "\n";
  if (save_string (settings_file, s) || load_string (settings_file, s, false)) {
    failed_error << HRULE;
    failed_error << "I could not save or reload the file\n\n";
    failed_error << "\t" << settings_file << "\n\n";
    failed_error << "Please give me full access control over this file and\n";
    failed_error << "rerun 'TeXmacs'.\n";
    failed_error << HRULE;
    TM_FAILED ("unable to write settings");
  }
  
  debug_boot << HRULE;
  debug_boot << "Installation completed successfully !\n";
  debug_boot << "I will now start up the editor\n";
  debug_boot << HRULE;
}

/******************************************************************************
* Initialization of TeXmacs
******************************************************************************/

void
init_texmacs () {
  //cout << "Initialize -- Main paths\n";
  init_main_paths ();
  //cout << "Initialize -- User dirs\n";
  init_user_dirs ();
  //cout << "Initialize -- Boot lock\n";
  acquire_boot_lock ();
  //cout << "Initialize -- Succession status table\n";
  init_succession_status_table ();
  //cout << "Initialize -- Succession standard DRD\n";
  init_std_drd ();
  //cout << "Initialize -- User preferences\n";
  load_user_preferences ();
  //cout << "Initialize -- Guile\n";
  init_scheme ();
  //cout << "Initialize -- Environment variables\n";
  init_env_vars ();
  //cout << "Initialize -- Miscellaneous\n";
  init_misc ();
}

/******************************************************************************
* Initialization of built-in plug-ins
******************************************************************************/

void
init_plugins () {
  url old_settings= "$TEXMACS_HOME_PATH/system/TEX_PATHS";
  url new_settings= "$TEXMACS_HOME_PATH/system/settings.scm";

  install_status= 0;
  string s;
  if (load_string (new_settings, s, false)) {
    if (load_string (old_settings, s, false)) {
      setup_texmacs ();
      install_status= 1;
    }
    else get_old_settings (s);
  }
  else texmacs_settings= block_to_scheme_tree (s);

  if (get_setting ("VERSION") != TEXMACS_VERSION) {
    init_upgrade ();
    url ch ("$TEXMACS_HOME_PATH/doc/about/changes/changes-recent.en.tm");
    install_status= exists (ch)? 2: 0;
  }
  init_tex ();
}
