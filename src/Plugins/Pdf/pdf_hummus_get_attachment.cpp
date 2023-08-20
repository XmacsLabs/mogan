/******************************************************************************
 * MODULE     : pdf_hummus_get_attachment.cpp
 * DESCRIPTION: Interface for getting attachment file in pdf
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
#include "pdf_hummus_get_attachment.hpp"

bool
get_tm_attachments_in_pdf (url pdf_path, array<url>& names) {
  EStatusCode status= PDFHummus::eSuccess;
  InputFile   pdfFile;
  PDFParser   parser;
  do {
    status= pdfFile.OpenFile (as_charp (as_string (pdf_path)));
    if (status != PDFHummus::eSuccess) {
      cout << "fail to open " << as_string (pdf_path) << LF;
      break;
    }
    parser.StartPDFParsing (pdfFile.GetInputStream ());
    PDFObjectCastPtr<PDFDictionary> catalog (
        parser.QueryDictionaryObject (parser.GetTrailer (), "Root"));
    // return 0;
    if (!catalog) {
      cout << "Can't find catalog. fail\n";
      status= PDFHummus::eFailure;
      break;
    }

    PDFObjectCastPtr<PDFDictionary> d_1 (catalog->QueryDirectObject ("Names"));
    if (!d_1) {
      cout << "Can't find d1. fail\n";
      status= PDFHummus::eFailure;
      break;
    }
    PDFObjectCastPtr<PDFDictionary> d_2 (
        d_1->QueryDirectObject ("EmbeddedFiles"));
    if (!d_2) {
      cout << "Can't find d2. fail\n";
      status= PDFHummus::eFailure;
      break;
    }

    PDFObjectCastPtr<PDFArray> arr (d_2->QueryDirectObject ("Names"));
    if (!arr) {
      cout << "Can't find arr. fail\n";
      status= PDFHummus::eFailure;
      break;
    }
    unsigned long n= arr->GetLength ();
    if (n & 1) {
      cout << "n is wrong\n";
      break;
    }
    for (unsigned long i= 0; i < n; i+= 2) {
      PDFObjectCastPtr<PDFLiteralString> name (arr->QueryObject (i));
      if (!name) {
        cout << "Can't find name\n";
        status= PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFDictionary> arr_d1 (arr->QueryObject (i + 1));
      if (!arr_d1) {
        cout << "Can't find arr_d1\n";
        status= PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFDictionary> arr_d2 (arr_d1->QueryDirectObject ("EF"));
      if (!arr_d2) {
        cout << "Can't find arr_d2\n";
        status= PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFStreamInput> stream (
          parser.QueryDictionaryObject (arr_d2.GetPtr (), "F"));
      if (!stream) {
        cout << "Can't find stream\n";
        status= PDFHummus::eFailure;
        break;
      }
      PDFDictionary* dir= stream->QueryStreamDictionary ();

      IByteReader* streamReader=
          parser.CreateInputStreamReader (stream.GetPtr ());
      if (!streamReader) return false;

      url attachment_path=
          relative (pdf_path, url (string (name->GetValue ().c_str ())));
      OutputFile attachment_file;
      attachment_file.OpenFile (
          std::string (as_charp (as_string (attachment_path))));
      OutputStreamTraits copy_help (attachment_file.GetOutputStream ());
      status= copy_help.CopyToOutputStream (streamReader);
      if (status == PDFHummus::eFailure) {
        cout << "Can't CopyToOutputStream\n";
        break;
      }
      attachment_file.CloseFile ();

      // char   buffer[0xffff];
      // string tmp;
      // if (streamReader) {
      //   pdfFile.GetInputStream ()->SetPosition (
      //       stream->GetStreamContentStart ());

      //   while (streamReader->NotEnded ()) {
      //     LongBufferSizeType readAmount=
      //         streamReader->Read ((Byte*) buffer, sizeof (buffer));
      //     CopyToOutputStream(streamReader);
      //     tmp << string (buffer, readAmount);
      //   }
      // }
      // s    = append (tmp, s);
      // names= append (string (name->GetValue ().c_str ()), names);
      names= append (attachment_path, names);
      delete streamReader;
    }
  } while (0);
  if (status == PDFHummus::eFailure) return false;
  else return true;
}

bool
get_tm_attachment_in_pdf (url pdf_path, url& name) {
  array<url> names;
  if (get_tm_attachments_in_pdf (pdf_path, names)) {
    if (N (names) != 1) {
      cout << "TODO: many attachment" << LF;
      return false;
    }
    name= names[0];
    return true;
  }
  else {
    return false;
  }
}
