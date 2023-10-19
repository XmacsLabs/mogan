
/******************************************************************************
 * MODULE     : pdf_image.hpp
 * DESCRIPTION: PDF Image on Hummus
 * COPYRIGHT  : (C) 2012 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef PDF_IMAGE_HPP
#define PDF_IMAGE_HPP

#include "PDFWriter/DictionaryContext.h"
#include "PDFWriter/ObjectsBasicTypes.h"
#include "PDFWriter/PDFWriter.h"
#include "basic.hpp"
#include "image_files.hpp"
#include "url.hpp"

class pdf_image_rep : public concrete_struct {
public:
  url          u;
  int          w, h;
  ObjectIDType id;

  pdf_image_rep (url _u, ObjectIDType _id) : u (_u), id (_id) {
    image_size (u, w, h);
  }
  ~pdf_image_rep () {}

  bool flush_jpg (PDFWriter& pdfw, url image);
#ifndef PDFHUMMUS_NO_PNG
  bool flush_png (PDFWriter& pdfw, url image);
#endif
  void flush (PDFWriter& pdfw);

  bool flush_for_pattern (PDFWriter& pdfw);
}; // class pdf_image_ref

class pdf_image {
  CONCRETE_NULL (pdf_image);
  pdf_image (url _u, ObjectIDType _id)
      : rep (tm_new<pdf_image_rep> (_u, _id)){};
};

CONCRETE_NULL_CODE (pdf_image);

class pdf_raw_image_rep : public concrete_struct {
public:
  string       data;
  int          w, h;
  ObjectIDType id;

  pdf_raw_image_rep (string _data, int _w, int _h, ObjectIDType _id)
      : data (_data), w (_w), h (_h), id (_id) {}
  pdf_raw_image_rep () {}

  void flush (PDFWriter& pdfw);
}; // pdf_raw_image_rep

class pdf_raw_image {
  CONCRETE_NULL (pdf_raw_image);
  pdf_raw_image (string _data, int _w, int _h, ObjectIDType _id)
      : rep (tm_new<pdf_raw_image_rep> (_data, _w, _h, _id)){};
};

CONCRETE_NULL_CODE (pdf_raw_image);

void hummus_pdf_image_size (url image, int& w, int& h);

#endif
