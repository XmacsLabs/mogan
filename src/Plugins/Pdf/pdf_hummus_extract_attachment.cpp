/******************************************************************************
 * MODULE     : pdf_hummus_extract_attachment.cpp
 * DESCRIPTION: Interface for extract attachment file in pdf
 * COPYRIGHT  : (C) 2023 Tangdouer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/
#include "PDFWriter/PDFWriter.h"
#include "PDFWriter/SafeBufferMacrosDefs.h"
#include "PDFWriter/Trace.h"

#include "PDFWriter/InputFileStream.h"
#include "PDFWriter/OutputBufferedStream.h"
#include "PDFWriter/OutputFileStream.h"
#include "PDFWriter/OutputStreamTraits.h"
#include "PDFWriter/PDFArray.h"
#include "PDFWriter/PDFLiteralString.h"
#include "PDFWriter/PDFObjectCast.h"
#include "PDFWriter/PDFParser.h"
#include "PDFWriter/PDFStreamInput.h"
using namespace PDFHummus;
#include <utility>
using namespace IOBasicTypes;
#include "pdf_hummus_extract_attachment.hpp"
#include "tm_debug.hpp"

bool
extract_attachments_from_pdf (url pdf_path, list<url>& names) {
  EStatusCode status= PDFHummus::eSuccess;
  InputFile   pdfFile;
  PDFParser   parser;
  do {
    status= pdfFile.OpenFile (as_charp (as_string (pdf_path)));
    if (status != PDFHummus::eSuccess) {
      if (DEBUG_CONVERT)
        debug_convert << "fail to open " << as_string (pdf_path) << LF;
      break;
    }
    parser.StartPDFParsing (pdfFile.GetInputStream ());
    PDFObjectCastPtr<PDFDictionary> catalog (
        parser.QueryDictionaryObject (parser.GetTrailer (), "Root"));
    // return 0;
    if (!catalog) {
      if (DEBUG_CONVERT) debug_convert << "Can't find catalog. fail\n";
      status= PDFHummus::eFailure;
      break;
    }

    PDFObjectCastPtr<PDFDictionary> d_1 (catalog->QueryDirectObject ("Names"));
    if (!d_1) {
      if (DEBUG_CONVERT) debug_convert << "Can't find d1. fail\n";
      status= PDFHummus::eFailure;
      break;
    }
    PDFObjectCastPtr<PDFDictionary> d_2 (
        d_1->QueryDirectObject ("EmbeddedFiles"));
    if (!d_2) {
      if (DEBUG_CONVERT) debug_convert << "Can't find d2. fail\n";
      status= PDFHummus::eFailure;
      break;
    }

    PDFObjectCastPtr<PDFArray> arr (d_2->QueryDirectObject ("Names"));
    if (!arr) {
      if (DEBUG_CONVERT) debug_convert << "Can't find arr. fail\n";
      status= PDFHummus::eFailure;
      break;
    }
    unsigned long n= arr->GetLength ();
    if (n & 1) {
      if (DEBUG_CONVERT) debug_convert << "n is wrong\n";
      break;
    }
    for (unsigned long i= 0; i < n; i+= 2) {
      PDFObjectCastPtr<PDFLiteralString> name (arr->QueryObject (i));
      if (!name) {
        if (DEBUG_CONVERT) debug_convert << "Can't find name\n";
        status= PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFDictionary> arr_d1 (arr->QueryObject (i + 1));
      if (!arr_d1) {
        if (DEBUG_CONVERT) debug_convert << "Can't find arr_d1\n";
        status= PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFDictionary> arr_d2 (arr_d1->QueryDirectObject ("EF"));
      if (!arr_d2) {
        if (DEBUG_CONVERT) debug_convert << "Can't find arr_d2\n";
        status= PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFStreamInput> stream (
          parser.QueryDictionaryObject (arr_d2.GetPtr (), "F"));
      if (!stream) {
        if (DEBUG_CONVERT) debug_convert << "Can't find stream\n";
        status= PDFHummus::eFailure;
        break;
      }
      PDFDictionary* dir= stream->QueryStreamDictionary ();

      IByteReader* streamReader=
          parser.CreateInputStreamReader (stream.GetPtr ());
      if (!streamReader) {
        if (DEBUG_CONVERT) debug_convert << "Can't find streamReader\n";
        status= PDFHummus::eFailure;
        break;
      }

      url attachment_path=
          relative (pdf_path, url (string (name->GetValue ().c_str ())));
      OutputFile attachment_file;
      status= attachment_file.OpenFile (
          std::string (as_charp (as_string (attachment_path))));
      if (status != PDFHummus::eSuccess) {
        if (DEBUG_CONVERT)
          debug_convert << "fail to open " << as_string (attachment_path) << LF;
        break;
      }
      pdfFile.GetInputStream ()->SetPosition (stream->GetStreamContentStart ());
      OutputStreamTraits copy_help (
          (IByteWriter*) attachment_file.GetOutputStream ());
      status= copy_help.CopyToOutputStream (streamReader);
      if (status != PDFHummus::eSuccess) {
        if (DEBUG_CONVERT) debug_convert << "Can't CopyToOutputStream\n";
        break;
      }
      status= attachment_file.CloseFile ();
      if (status != PDFHummus::eSuccess) {
        if (DEBUG_CONVERT)
          debug_convert << "fail to close " << as_string (attachment_path)
                        << LF;
        break;
      }

      names= names * attachment_path;
      delete streamReader;
    }
  } while (0);
  if (status == PDFHummus::eFailure) return false;
  else return true;
}

bool
scm_extract_attachments (url pdf_path) {
  list<url> attachments_paths;
  return extract_attachments_from_pdf (pdf_path, attachments_paths);
}
