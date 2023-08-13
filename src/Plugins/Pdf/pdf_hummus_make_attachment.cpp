/******************************************************************************
* MODULE     : pdf_hummus_renderer.hpp
* DESCRIPTION: Interface for embedding text files into pdf files
* COPYRIGHT  : (C) 2023 Tangdouer
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#include "pdf_hummus_make_attachment.hpp"

#include "PDFWriter/DictionaryContext.h"
#include "PDFWriter/ObjectsContext.h"
#include "PDFWriter/PDFRectangle.h"
#include "PDFWriter/PDFStream.h"
#include "PDFWriter/PDFWriter.h"
#include "PDFWriter/SafeBufferMacrosDefs.h"
#include "PDFWriter/Trace.h"
#include "PDFWriter/PDFObjectCast.h"
#include "PDFWriter/PDFParser.h"
#include "PDFWriter/PDFArray.h"
#include "PDFWriter/PDFLiteralString.h"
#include "PDFWriter/PDFStreamInput.h"
#include "PDFWriter/InputFileStream.h"
#include "PDFWriter/OutputBufferedStream.h"
#include "PDFWriter/OutputFileStream.h"
#include "PDFWriter/OutputStreamTraits.h"
using namespace PDFHummus;
#include <utility>

using namespace IOBasicTypes;
class PDFAttachment {
public:
  PDFAttachment (void);
  PDFAttachment (string attachment_path);
  PDFAttachment (Byte inByte[], size_t inLenth, string inName);
  ~PDFAttachment (void);

  Byte*  file_content;
  size_t lenth;
  string name;
};

class PDFWriter;

typedef hashmap<PDFAttachment*, ObjectIDType> PDFAttachmentToObjectIDTypeMap;

class PDFAttachmentWriter : public DocumentContextExtenderAdapter {
public:
  PDFAttachmentWriter (PDFWriter* inPDFWriter);
  ~PDFAttachmentWriter (void);

  EStatusCode AttachToAllPage (PDFAttachment* inAttachment);

  virtual EStatusCode
  OnCatalogWrite (CatalogInformation* inCatalogInformation,
                  DictionaryContext*  inCatalogDictionaryContext,
                  ObjectsContext*     inPDFWriterObjectContext,
                  DocumentContext*    inPDFWriterDocumentContext);

private:
  PDFWriter*                     mPDFWriter;
  PDFAttachmentToObjectIDTypeMap mAttachment;
  void                           ListenOnCatalogWrite ();
  EStatusCodeAndObjectIDType     WriteAttachment (PDFAttachment* inAttachment);
  void                           CleanupAttachment ();
};
bool
pdf_hummus_make_attachments (string pdf_path, list<string> attachment_paths,
                             string out_path) {
  PDFWriter   pdfWriter;
  EStatusCode status;
  do {
    status= pdfWriter.ModifyPDF (as_charp (pdf_path), ePDFVersion16,
                                 as_charp (out_path));

    if (status != eSuccess) {
      cout << "start fail\n";
      break;
    }
    PDFAttachmentWriter attachmentWriter (&pdfWriter);

    for (int i= 0; i < (int) N (attachment_paths); i++) {
      string          attachment_path= attachment_paths[i];
      InputFileStream tm_file_stream;
      status= tm_file_stream.Open (as_charp (attachment_path));

      if (status != PDFHummus::eSuccess) {
        cout << "failed to open " << attachment_path << "\n";
        continue;
      }
      else {
        cout << "success to open " << attachment_path << "\n";
      }

      LongFilePositionType file_size= tm_file_stream.GetFileSize ();

      Byte* file_content= new Byte[file_size + 16];
      tm_file_stream.Read (file_content, file_size);

      int j, last_slash_index= 0;
      for (j= 0; j < N (attachment_path); j++) {
        if (attachment_path[j] == '/') last_slash_index= j;
      }
      string attachment_name=
          attachment_path (last_slash_index + 1, N (attachment_path));

      PDFAttachment* aAttachment=
          new PDFAttachment (file_content, file_size, attachment_name);
      status= attachmentWriter.AttachToAllPage (aAttachment);
      // return status;
      if (status != eSuccess) {
        cout << "fail to attach " << attachment_path << "\n";
        continue;
      }
      else {
        cout << "success to attach " << attachment_path << "\n";
        continue;
      }
    }
    status= pdfWriter.EndPDF ();
  } while (false);

  if (eSuccess == status) {
    cout << "Succeeded in creating PDF file\n";
    return true;
  }
  else {
    cout << "Failed in creating PDF file\n";
    return false;
  }
}

bool
pdf_hummus_make_attachment (url pdf_path, url attachment_path, url out_path) {
  if ((suffix (pdf_path) == "pdf"))
    return pdf_hummus_make_attachments (
        as_string (pdf_path), list<string> (as_string (attachment_path)),
        as_string (out_path));
  else return false;
}
PDFAttachment::PDFAttachment (void) {
  file_content= NULL;
  lenth       = 0;
}

PDFAttachment::PDFAttachment (string attachment_path) {
  InputFileStream tm_file_stream;
  EStatusCode     status= tm_file_stream.Open (as_charp (attachment_path));
  if (status != PDFHummus::eSuccess) {
    cout << "failed to open tm\n";
    return;
  }

  lenth= (size_t) tm_file_stream.GetFileSize ();

  file_content= new Byte[lenth + 16];
  tm_file_stream.Read (file_content, lenth);

  int i, last_slash_index= 0;
  for (i= 0; i < N (attachment_path); i++) {
    if (attachment_path[i] == '/') last_slash_index= i;
  }
  name= attachment_path (last_slash_index + 1, N (attachment_path));
}
PDFAttachment::PDFAttachment (Byte inByte[], size_t inLenth, string inName) {
  file_content= inByte;
  lenth       = inLenth;
  name        = inName;
}

PDFAttachment::~PDFAttachment (void) {
  if (file_content) delete file_content;
}

PDFAttachmentWriter::PDFAttachmentWriter (PDFWriter* inPDFWriter) {
  mPDFWriter= inPDFWriter;
  ListenOnCatalogWrite ();
}

void
PDFAttachmentWriter::ListenOnCatalogWrite () {
  mPDFWriter->GetDocumentContext ().AddDocumentContextExtender (this);
}

PDFAttachmentWriter::~PDFAttachmentWriter (void) {
  if (N (mAttachment) != 0)
    TRACE_LOG ("PDFAttachmentWriter::~PDFAttachmentWriter, Exception. Has "
               "attachement that were not associated with pages");
  CleanupAttachment ();
}

EStatusCode
PDFAttachmentWriter::OnCatalogWrite (
    CatalogInformation* inCatalogInformation,
    DictionaryContext*  inCatalogDictionaryContext,
    ObjectsContext*     inPDFWriterObjectContext,
    DocumentContext*    inPDFWriterDocumentContext) {
  if (N (mAttachment) == 0) return eSuccess;

  inCatalogDictionaryContext->WriteKey ("Names");

  DictionaryContext* dictionaryContext_0=
      inPDFWriterObjectContext->StartDictionary ();
  dictionaryContext_0->WriteKey ("EmbeddedFiles");
  DictionaryContext* dictionaryContext_1=
      inPDFWriterObjectContext->StartDictionary ();
  dictionaryContext_0->WriteKey ("Names");
  inPDFWriterObjectContext->StartArray ();

  iterator<PDFAttachment*> it= iterate (mAttachment);

  while (it->busy ()) {
    PDFAttachment* cur_attachment= it->next ();
    inPDFWriterObjectContext->WriteLiteralString (
        as_charp (cur_attachment->name));
    DictionaryContext* dictionaryContext_2=
        inPDFWriterObjectContext->StartDictionary ();
    dictionaryContext_2->WriteKey ("EF");
    DictionaryContext* dictionaryContext_3=
        inPDFWriterObjectContext->StartDictionary ();
    dictionaryContext_3->WriteKey ("F");
    inPDFWriterObjectContext->WriteIndirectObjectReference (
        mAttachment[cur_attachment]);
    inPDFWriterObjectContext->EndDictionary (dictionaryContext_3);
    dictionaryContext_2->WriteKey ("F");
    dictionaryContext_2->WriteLiteralStringValue (
        as_charp (cur_attachment->name));
    dictionaryContext_2->WriteKey ("Type");
    dictionaryContext_2->WriteNameValue ("F");
    inPDFWriterObjectContext->EndDictionary (dictionaryContext_2);
  }
  inPDFWriterObjectContext->EndArray ();
  inPDFWriterObjectContext->EndDictionary (dictionaryContext_1);
  inPDFWriterObjectContext->EndDictionary (dictionaryContext_0);
  CleanupAttachment ();
  return eSuccess;
}

void
PDFAttachmentWriter::CleanupAttachment () {

  iterator<PDFAttachment*> it= iterate (mAttachment);
  while (it->busy ()) {
    delete it->next ();
  }
  mAttachment= PDFAttachmentToObjectIDTypeMap ();
}

EStatusCode
PDFAttachmentWriter::AttachToAllPage (PDFAttachment* inAttachment) {
  return WriteAttachment (inAttachment).first;
}

EStatusCodeAndObjectIDType
PDFAttachmentWriter::WriteAttachment (PDFAttachment* inAttachment) {
  EStatusCodeAndObjectIDType result;
  if (mAttachment->contains (inAttachment)) {
    result.first = eSuccess;
    result.second= mAttachment[inAttachment];
    return result;
  }

  do {
    ObjectsContext& objectsContext= mPDFWriter->GetObjectsContext ();

    result.second= objectsContext.StartNewIndirectObject ();

    DictionaryContext* dictionaryContext= objectsContext.StartDictionary ();
    dictionaryContext->WriteKey ("Type");
    dictionaryContext->WriteNameValue ("EmbeddedFile");
    PDFStream*   pdfStream= objectsContext.StartPDFStream (dictionaryContext);
    IByteWriter* mwriter  = pdfStream->GetWriteStream ();
    mwriter->Write (inAttachment->file_content, inAttachment->lenth);
    objectsContext.EndPDFStream (pdfStream);

    mAttachment (inAttachment)= result.second;
    result.first              = eSuccess;
  } while (false);
  return result;
}



bool load_tm_attachment_in_pdf(url pdf_path ,array<string> &names, array<string> &s)
{
  EStatusCode status = PDFHummus::eSuccess;
  InputFile pdfFile;
  PDFParser parser;
  do
  {
    status = pdfFile.OpenFile(as_charp(as_string(pdf_path)));
    if(status != PDFHummus::eSuccess)
		{
			cout<<"fail to open " << as_string(pdf_path) << LF;
			break;
		}
    parser.StartPDFParsing(pdfFile.GetInputStream());
    PDFObjectCastPtr<PDFDictionary> catalog(parser.QueryDictionaryObject(parser.GetTrailer(), "Root"));
    //return 0;
    if (!catalog)
    {
      cout << "Can't find catalog. fail\n";
      status = PDFHummus::eFailure;
      break;
    }

    PDFObjectCastPtr<PDFDictionary> d_1(catalog->QueryDirectObject("Names"));
    if (!d_1)
    {
      cout << "Can't find d1. fail\n";
      status = PDFHummus::eFailure;
      break;
    }
    PDFObjectCastPtr<PDFDictionary> d_2(d_1->QueryDirectObject("EmbeddedFiles"));
    if (!d_2)
    {
      cout << "Can't find d2. fail\n";
      status = PDFHummus::eFailure;
      break;
    }

    PDFObjectCastPtr<PDFArray> arr(d_2->QueryDirectObject("Names"));
    if (!arr)
    {
      cout << "Can't find arr. fail\n";
      status = PDFHummus::eFailure;
      break;
    }
    unsigned long n = arr->GetLength();
    if (n & 1)
    {
      cout << "n is wrong\n";
      break;
    }
    for (unsigned long i = 0; i < n; i += 2)
    {
      PDFObjectCastPtr<PDFLiteralString> name(arr->QueryObject(i));
      if (!name)
      {
        cout << "Can't find name\n";
        status = PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFDictionary> arr_d1(arr->QueryObject(i + 1));
      if (!arr_d1)
      {
        cout << "Can't find arr_d1\n";
        status = PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFDictionary> arr_d2(arr_d1->QueryDirectObject("EF"));
      if (!arr_d2)
      {
        cout << "Can't find arr_d2\n";
        status = PDFHummus::eFailure;
        break;
      }
      PDFObjectCastPtr<PDFStreamInput> stream(parser.QueryDictionaryObject(arr_d2.GetPtr(), "F"));
      if (!stream)
      {
        cout << "Can't find stream\n";
        status = PDFHummus::eFailure;
        break;
      }
      PDFDictionary* dir = stream -> QueryStreamDictionary();

      IByteReader* streamReader = parser.CreateInputStreamReader(stream.GetPtr());
      if(! streamReader)
          return false;

      char buffer[0xffff];
      string tmp;
      if(streamReader)
      {
          pdfFile.GetInputStream()->SetPosition(stream->GetStreamContentStart());
          while(streamReader->NotEnded())
          {
            LongBufferSizeType readAmount = streamReader->Read((Byte*)buffer,sizeof(buffer));
            tmp << string(buffer, readAmount);
          }
        
      }
      s = append(tmp, s);
      names = append(string(name -> GetValue().c_str()), names);
      delete streamReader;
    }
  } while (0);
  if(status == PDFHummus::eFailure)
    return false;
  else
    return true;
}

bool get_tm_attachment_in_pdf(url pdf_path, string &s)
{
  array<string> names, ss;
  if(load_tm_attachment_in_pdf(pdf_path ,names, ss))
  {
    if(N(ss) != 1)
    {
      cout << "TODO: many attachment" << LF;
      return false;
    }
    s = ss[0];
    return true;
  }else
  {
    return false;
  }
}