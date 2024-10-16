
/******************************************************************************
 * MODULE     : pdf.cpp
 * DESCRIPTION: Basics of the PDF format
 * COPYRIGHT  : (C) 2010-2012 David Michel, Joris van der Hoeven, Denis Raux
 *                  2023    Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "pdf.hpp"
#include "scheme.hpp"

string
default_pdf_version () {
  return "1.4";
}

string
pdf_version () {
  string version= get_preference ("texmacs->pdf:version");
  if (version == "1.4" || version == "1.5" || version == "1.6" ||
      version == "1.7")
    return version;
  return default_pdf_version ();
}
