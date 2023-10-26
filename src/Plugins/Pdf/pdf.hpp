
/******************************************************************************
 * MODULE     : pdf.hpp
 * DESCRIPTION: Basics of the PDF format
 * COPYRIGHT  : (C) 2010-2012 David Michel, Joris van der Hoeven, Denis Raux
 *                  2023    Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef PDF_H
#define PDF_H

#include "string.hpp"
#include "url.hpp"

/**
 * @brief the default PDF version when exporting to PDF specified by the user
 *
 * @return the default PDF version, eg. 1.7.0
 */
string pdf_version ();

/**
 * @brief the default PDF version when exporting to PDF
 *
 * @return the default PDF version, eg. 1.7.0
 */
string default_pdf_version ();

/**
 * @brief the pdf version adopt in the file
 *
 * @return the pdf version of the url
 */
string pdf_version (url path);

#endif
