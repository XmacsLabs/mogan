/******************************************************************************
 * MODULE     : pdf_hummus_extract_attachment.cpp
 * DESCRIPTION: Interface for extract attachment file in pdf
 * COPYRIGHT  : (C) 2023 Tangdouer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/
#ifndef PDF_HUMMUS_GET_ATTACHMENT_H
#define PDF_HUMMUS_GET_ATTACHMENT_H
#include "hashmap.hpp"
#include "iterator.hpp"
#include "list.hpp"
#include "string.hpp"
#include "tm_ostream.hpp"
#include "url.hpp"
#include "tree.hpp"

/**

Extracts attachments from a PDF file.
@param pdf_path The path of the PDF file.
@param names A reference to a list that will store the paths of extracted
attachments.
@return Returns true if the extraction is successful, false otherwise.
*/
bool extract_attachments_from_pdf (url pdf_path, list<url>& names);

/**

Extracts attachments from a PDF file in a simplified way for SCM glue
operations.
@param pdf_path The path of the PDF file.
@return Returns true if the extraction is successful, false otherwise.
*/
bool scm_extract_attachments (url pdf_path);

array<url> get_linked_file_paths(tree t, url path);

tree replace_with_relative_path(tree t, url path);

url get_main_tm(url pdf_path);

#endif // ifdef PDF_HUMMUS_MAKE_ATTACHMENT_H
