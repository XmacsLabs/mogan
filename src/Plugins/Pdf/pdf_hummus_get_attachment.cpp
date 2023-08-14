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
get_tm_attachment_in_pdf (url pdf_path, array<string>& names,
                          array<string>& s) {
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

      char   buffer[0xffff];
      string tmp;
      if (streamReader) {
        pdfFile.GetInputStream ()->SetPosition (
            stream->GetStreamContentStart ());
        while (streamReader->NotEnded ()) {
          LongBufferSizeType readAmount=
              streamReader->Read ((Byte*) buffer, sizeof (buffer));
          tmp << string (buffer, readAmount);
        }
      }
      s    = append (tmp, s);
      names= append (string (name->GetValue ().c_str ()), names);
      delete streamReader;
    }
  } while (0);
  if (status == PDFHummus::eFailure) return false;
  else return true;
}

bool
get_tm_attachment_in_pdf (url pdf_path, string& s) {
  array<string> names, ss;
  if (load_tm_attachment_in_pdf (pdf_path, names, ss)) {
    if (N (ss) != 1) {
      cout << "TODO: many attachment" << LF;
      return false;
    }
    s= ss[0];
    return true;
  }
  else {
    return false;
  }
}