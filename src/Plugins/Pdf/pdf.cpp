
/******************************************************************************
 * MODULE     : pdf.cpp
 * DESCRIPTION: Basics of the PDF format
 * COPYRIGHT  : (C) 2010-2012 David Michel, Joris van der Hoeven, Denis Raux
 *                  2023      Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "pdf.hpp"
#include "file.hpp"
#include "scheme.hpp"
#include "tm_debug.hpp"

#include "PDFWriter/PDFParser.h"

string
default_pdf_version () {
  return "1.7";
}

string
pdf_version () {
  string version= get_preference ("texmacs->pdf:version");
  if (version == "1.4" || version == "1.5" || version == "1.6" ||
      version == "1.7")
    return version;
  return default_pdf_version ();
}

string
pdf_version (url path) {
  if (suffix (path) != "pdf") {
    std_error << "Suffix of " << as_string (path) << " must be pdf!" << LF;
    return default_pdf_version ();
  }
  if (!is_local_and_single (path)) {
    std_error << as_string (path) << " must be a local and single file!" << LF;
    return default_pdf_version ();
  }

  return default_pdf_version ();
  // InputFile  pdfFile;
  // PDFParser* parser= new PDFParser ();
  // pdfFile.OpenFile (as_charp (concretize (image)));
  // EStatusCode status= parser->StartPDFParsing (pdfFile.GetInputStream ());
  // if (status != PDFHummus::eFailure) {
}
