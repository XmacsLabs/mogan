
/******************************************************************************
 * MODULE     : pdf_hummus_make_attachment.hpp
 * DESCRIPTION: Renderer for printing pdf graphics using the PDFHummus library
 * COPYRIGHT  : (C) 2012  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef PDF_HUMMUS_MAKE_ATTACHMENT_H
#define PDF_HUMMUS_MAKE_ATTACHMENT_H

#include "hashmap.hpp"
#include "iterator.hpp"
#include "list.hpp"
#include "string.hpp"
#include "tm_ostream.hpp"
#include "url.hpp"

bool pdf_hummus_make_attachments (string pdf_path, list<string> attachment_path,
                                  string out_path);

bool pdf_hummus_make_attachment (url pdf_path, url attachment_path,
                                 url out_path);

#endif // ifdef PDF_HUMMUS_MAKE_ATTACHMENT_H
