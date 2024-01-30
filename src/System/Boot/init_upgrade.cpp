
/******************************************************************************
 * MODULE     : init_upgrade.cpp
 * DESCRIPTION: initializations which are only necessary when
 *              you just upgraded your TeXmacs distribution
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "boot.hpp"
#include "data_cache.hpp"
#include "file.hpp"
#include "path.hpp"
#include "preferences.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"

/******************************************************************************
 * Check for old Init.scm and Init-buffer.scm files
 ******************************************************************************/

static void
init_upgrade_scheme () {
#ifndef OS_WIN
  url    u   = "$TEXMACS_HOME_PATH/progs";
  string prgs= as_string (u);
  if (exists (u * "Init.scm") && (!exists (u * "my-init-texmacs.scm"))) {
    system ("sed 's/Init.scm/init-texmacs.scm/'", u * "Init.scm", ">",
            u * "my-init-texmacs.scm");
    remove (u * "Init.scm");
  }
  if (exists (u * "Init-buffer.scm") && (!exists (u * "my-init-buffer.scm"))) {
    system ("sed 's/Init-buffer.scm/init-buffer.scm/'", u * "Init-buffer.scm",
            ">", u * "my-init-buffer.scm");
    remove (u * "Init-buffer.scm");
  }
#endif
}

/******************************************************************************
 * Upgrading TeXmacs
 ******************************************************************************/

void
init_upgrade () {
  string install_version= get_setting ("VERSION");

  debug_boot << HRULE;
  debug_boot << "Your disk contains a configuration file for Mogan Stem Suite ";
  debug_boot << install_version << LF;
  debug_boot << "I will now perform the upgrade to version ";
  debug_boot << XMACS_VERSION << LF;
  debug_boot << HRULE;

  url new_settings= "$TEXMACS_HOME_PATH/system/settings.scm";
  remove (new_settings);

  setup_texmacs ();
  init_upgrade_scheme ();

  remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
  remove (get_tm_cache_path () * url_wildcard ("__*"));
  remove (url ("$TEXMACS_HOME_PATH/fonts/font-database.scm"));
  remove (url ("$TEXMACS_HOME_PATH/fonts/font-features.scm"));
  remove (url ("$TEXMACS_HOME_PATH/fonts/font-characteristics.scm"));
  remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
  cache_refresh ();
}
