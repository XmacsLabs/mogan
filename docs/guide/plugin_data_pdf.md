# PDF Data Plugin

## TeXmacs -> PDF
The option to export to PDF is found under "Edit -> Preferences -> Conversion -> PDF".

### Option: `Expand beamer slides`
This option is turned off by default.

When exporting slides to PDF, collapsible objects will be expanded one by one, turning one slide in Mogan into multiple slides.

If a slide contains a session, each paragraph in the session will execute sequentially when exporting to PDF. For example, if there is a Maxima session on one slide with three paragraphs, when exported to PDF, this one page will become three pages, and these three paragraphs will execute sequentially across these three pages.

### Option: `PDF version number`
Defaults to 1.4. It is generally recommended not to switch the PDF version number unless there is a specific need.

## PDF -> TeXmacs
You can use "File -> Export -> PDF with embedded document" to export PDFs while embedding the document as an attachment into the PDF document. Then you can use "File -> Import -> PDF with embedded document" to load this PDF.

The use case for this feature is when a Mogan user creates a TeXmacs document using Mogan and wants to share it with a friend who is not a Mogan user. In this case, the Mogan user can directly export the editable PDF document, share it with their friend, and if their friend has a need to edit the PDF content, they can recommend that they download Mogan.
