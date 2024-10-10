
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
#include "object.hpp"
#include "object_l1.hpp"
#include "object_l2.hpp"
#include "s7_tm.hpp"

#include "Updater/tm_updater.hpp"
#include "glue_updater.cpp"

#include "LaTeX_Preview/latex_preview.hpp"
#include "Tex/tex.hpp"
#include "glue_tex.cpp"

#ifdef USE_PLUGIN_BIBTEX
#include "Bibtex/bibtex.hpp"
#include "Bibtex/bibtex_functions.hpp"
#include "glue_bibtex.cpp"
#endif

#include "Database/database.hpp"
#include "glue_tmdb.cpp"

#include "Xml/xml.hpp"
#include "glue_xml.cpp"

bool
use_plugin_bibtex () {
#ifdef USE_PLUGIN_BIBTEX
  return true;
#else
  return false;
#endif
}

#ifndef OS_WASM
bool
supports_native_pdf () {
#ifdef USE_PLUGIN_PDF
  return true;
#else
  return false;
#endif
}

bool
supports_ghostscript () {
#ifdef USE_GS
  return true;
#else
  return false;
#endif
}

string
pdfhummus_version () {
  return string (PDFHUMMUS_VERSION);
}

#include "glue_ghostscript.cpp"
#include "glue_pdf.cpp"
#endif

#include "glue_plugin.cpp"

void
initialize_glue_plugins () {
  initialize_glue_plugin ();
#ifdef USE_PLUGIN_BIBTEX
  initialize_glue_bibtex ();
#endif
  initialize_glue_tex ();
  initialize_glue_tmdb ();
  initialize_glue_updater ();
  initialize_glue_xml ();
#ifndef OS_WASM
  initialize_glue_ghostscript ();
  initialize_glue_pdf ();
#endif
}
