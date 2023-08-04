<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel|comment>>

<\body>
  <tmdoc-title|Fix corrupted Metadata in PDF export>

  <section|Bug metadata>

  <\description>
    <item*|Owner>jingkaimori
  </description>

  <section|Description>

  If a tm document does not set title in metadata, e.g. has no
  <explain-macro|doc-data> item, then exported PDF file of this document will
  be corruped.

  <\unfolded-comment|+2WswXp3M2EUs8qr2|+2WswXp3M2EUs8qr3|comment|Darcy
  Shen|1684246691|>
    For PDF export, here is the related <hlink|code
    snippet|https://gitee.com/texmacs/texmacs/blob/v2.1.2/src/Plugins/Pdf/pdf_hummus_renderer.cpp#L2446-2464>:

    <\cpp-code>
      void

      pdf_hummus_renderer_rep::flush_metadata () {

      \ \ if (N(metadata) == 0) return;

      \ \ DocumentContext& documentContext= pdfWriter.GetDocumentContext();

      \ \ TrailerInformation& trailerInfo=
      documentContext.GetTrailerInformation();

      \ \ InfoDictionary& info= trailerInfo.GetInfo();

      \ \ if (metadata-\<gtr\>contains ("title"))

      \ \ \ \ info.Title= as_hummus_string (metadata ["title"]);

      \ \ if (metadata-\<gtr\>contains ("author"))

      \ \ \ \ info.Author= as_hummus_string (metadata ["author"]);

      \ \ if (metadata-\<gtr\>contains ("subject"))

      \ \ \ \ info.Subject= as_hummus_string (metadata ["subject"]);

      \ \ string creator= "TeXmacs " * string (TEXMACS_VERSION);

      \ \ string producer= creator * " + Hummus 4.0";

      \ \ info.Creator= utf8_as_hummus_string (creator);

      \ \ info.Producer= utf8_as_hummus_string (producer);

      \ \ PDFDate date; date.SetToCurrentTime ();

      \ \ info.CreationDate= date;

      }
    </cpp-code>

    In this case, <cpp|as_hummus_string> convert a cork string to a hummus
    string. The result of the metadata should be a UTF-8 string. In this way,
    in src/Plugins/Pdf, there are no cork string.
  </unfolded-comment>

  <tmdoc-copyright|2022|jingkaimori|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>