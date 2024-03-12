
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
#include "preferences.hpp"
#include "scheme.hpp"
#include "tm_debug.hpp"

#include "PDFWriter/EStatusCode.h"
#include "PDFWriter/InputFile.h"
#include "PDFWriter/PDFParser.h"

using PDFHummus::EStatusCode;

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

string
pdf_version (url u) {
  c_string path (as_string (u));
  if (suffix (u) != "pdf") {
    std_error << "Suffix of " << path << " must be pdf!" << LF;
    return default_pdf_version ();
  }
  if (!is_local_and_single (u)) {
    std_error << path << " must be a local and single file!" << LF;
    return default_pdf_version ();
  }

  InputFile pdfFile;
  pdfFile.OpenFile (std::string (path));
  PDFParser   parser;
  EStatusCode status= parser.StartPDFParsing (pdfFile.GetInputStream ());
  if (status != PDFHummus::eFailure) {
    double version= parser.GetPDFLevel ();
    return as_string (version);
  }
  else {
    std_error << "Failed to StartPDFParsing for " << path << LF;
  }

  return default_pdf_version ();
}
