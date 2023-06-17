
/******************************************************************************
 * MODULE     : init_glue_plugins.cpp
 * DESCRIPTION: Plugins Glue for linking TeXmacs commands to scheme
 * COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "init_glue_plugins.hpp"
#include "object_l1.hpp"
#include "object_l2.hpp"
#include "s7_tm.hpp"

#include "Updater/tm_updater.hpp"
#include "glue_updater.cpp"

#include "Bibtex/bibtex.hpp"
#include "Bibtex/bibtex_functions.hpp"
#include "glue_bibtex.cpp"

#include "Database/database.hpp"
#include "glue_tmdb.cpp"

void
initialize_glue_plugins () {
  initialize_glue_updater ();
  initialize_glue_bibtex ();
  initialize_glue_tmdb ();
}

