/******************************************************************************
 * MODULE     : mupdf_print_renderer.hpp
 * DESCRIPTION: PDF export print renderer using MuPDF
 * COPYRIGHT  : (C) 2025 The Mogan Stem Authors
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef MUPDF_PRINT_RENDERER_H
#define MUPDF_PRINT_RENDERER_H

#include "gui.hpp"
#include "hashmap.hpp"
#include "mupdf_renderer.hpp"
#include "renderer.hpp"
#include "url.hpp"

class mupdf_print_renderer_rep : public mupdf_renderer_rep {

  static const int default_dpi=
      72;         // PDF initial coordinate system corresponds to 72 dpi
  bool   started; // initialisation is OK
  url    pdf_file_name;
  double pdf_scale;

  fz_buffer* contents;
  fz_rect    mediabox;
  pdf_obj*   resources;

  hashmap<string, string>     metadata;
  hashmap<string, mupdf_font> native_fonts;
  pdf_write_options           write_opts;

  void           begin_page ();
  void           end_page ();
  pdf_font_desc* get_font_desc (string fontname);

public:
  mupdf_print_renderer_rep (url pdf_file_name, int dpi, int nr_pages,
                            string ptype, bool landsc, double paper_w,
                            double paper_h);
  ~mupdf_print_renderer_rep ();
  bool is_printer ();
  bool is_started ();
  void next_page ();
  void flush_metadata ();
  void set_metadata (string kind, string val);
};

renderer mupdf_print_renderer (url pdf_file_name, int dpi, int nr_pages= 1,
                               string page_type= "a4", bool landscape= false,
                               double paper_w= 21.0, double paper_h= 29.7);

#endif