/******************************************************************************
 * MODULE     : mupdf_print_renderer.hpp
 * DESCRIPTION: PDF export print renderer using MuPDF
 * COPYRIGHT  : (C) 2025 The Mogan Stem Authors
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "mupdf_print_renderer.hpp"
#include "Freetype/tt_file.hpp"
#include "font.hpp"
#include "mupdf/fitz/context.h"
#include "mupdf/fitz/document.h"
#include "mupdf/fitz/writer.h"
#include "mupdf/pdf/document.h"
#include "mupdf_renderer.hpp"
#include "scheme.hpp"
#include "string.hpp"
#include "tm_configure.hpp"
#include "tm_debug.hpp"
#include "tm_url.hpp"

/******************************************************************************
 * constructors and destructors
 ******************************************************************************/

mupdf_print_renderer_rep::mupdf_print_renderer_rep (
    url pdf_file_name2, int dpi, int nr_pages2, string page_type2,
    bool landscape2, double paper_w, double paper_h)
    : mupdf_renderer_rep (false), pdf_file_name (pdf_file_name2),
      contents (NULL), resources (NULL) {
  fz_context* ctx   = mupdf_context ();
  float       width = default_dpi * paper_w / 2.54;
  float       height= default_dpi * paper_h / 2.54;
  h                 = paper_h * dpi * pixel * retina_factor / 2.54;
  w                 = paper_w * dpi * pixel * retina_factor / 2.54;
  pdf_scale         = (double) default_dpi / dpi / retina_factor;

  mediabox                    = {0, 0, width, height};
  write_opts                  = pdf_default_write_options;
  write_opts.do_compress      = 1;
  write_opts.do_compress_fonts= 1;
  write_opts.do_garbage       = 3;
  fz_try (ctx) {
    doc    = pdf_create_document (ctx);
    started= true;
    begin_page ();
  }
  fz_catch (ctx) { started= false; }
}

mupdf_print_renderer_rep::~mupdf_print_renderer_rep () {
  if (!started) return;
  end_page ();
  flush_metadata ();

  c_string    path (concretize (pdf_file_name));
  fz_context* ctx= mupdf_context ();
  fz_try (ctx) {
    pdf_subset_fonts (ctx, doc, 0, NULL);
    pdf_save_document (ctx, doc, path, &write_opts);
    debug_std << "Finished print pdf\n";
  }
  fz_catch (ctx) { fz_report_error (ctx); }
  pdf_drop_document (mupdf_context (), doc);
}

bool
mupdf_print_renderer_rep::is_printer () {
  return true;
}

bool
mupdf_print_renderer_rep::is_started () {
  return started;
}

void
mupdf_print_renderer_rep::begin_page () {
  fz_context* ctx= mupdf_context ();
  if (dev) {
    end_page ();
  }
  fz_try (ctx) {
    dev          = pdf_page_write (ctx, doc, mediabox, &resources, &contents);
    fz_matrix ctm= fz_make_matrix (pdf_scale, 0, 0, -pdf_scale, 0, 0);
    proc= pdf_new_run_processor (ctx, doc, dev, ctm, -1, "View", NULL, NULL,
                                 NULL);
  }
  fz_catch (ctx) {
    fz_report_error (ctx);
    fz_drop_device (ctx, dev);
    dev= NULL;
    pdf_drop_processor (ctx, proc);
    proc= NULL;
    convert_error << "MuPDF: Failed to create new page\n";
    return;
  }
  fg           = -1;
  bg           = -1;
  lw           = -1;
  current_width= -1.0;
  cfn          = "";
  in_text      = false;
  // outmost save of the graphics state
  proc->op_q (ctx, proc);
  // set scaling suitable for dpi (pdf default is 72)
  proc->op_cm (ctx, proc, 1, 0, 0, 1, 0, 0);

  set_clipping (0, -h, w, 0);
}

void
mupdf_print_renderer_rep::end_page () {
  fz_context* ctx= mupdf_context ();
  pdf_obj*    obj= NULL;
  if (!dev) {
    return;
  }
  end_text ();
  // reset set_clipping calls in order to have well formed PDF.
  if (clip_active) {
    clip_proc->op_Q (ctx, clip_proc);
    clip_active= false;
    clip_proc  = NULL;
  }
  // outmost restore for the graphics state (see begin_page)
  proc->op_Q (ctx, proc);
  fz_try (ctx) {
    pdf_close_processor (ctx, proc);
    fz_close_device (ctx, dev);
    obj= pdf_add_page (ctx, doc, mediabox, 0, resources, contents);
    pdf_insert_page (ctx, doc, -1, obj);
  }
  fz_catch (ctx) {
    fz_report_error (ctx);
    convert_error << "MuPDF: Failed to end page\n";
  }
  pdf_drop_processor (ctx, proc);
  proc= NULL;
  fz_drop_device (ctx, dev);
  dev= NULL;
  pdf_drop_obj (ctx, obj);
  fz_drop_buffer (ctx, contents);
  contents= NULL;
  pdf_drop_obj (ctx, resources);
  resources= NULL;
}

void
mupdf_print_renderer_rep::next_page () {
  end_page ();
  begin_page ();
}

/******************************************************************************
 * Font handler
 ******************************************************************************/

static pdf_font_desc*
load_pdf_font (string fontname, pdf_document* pdoc, fz_device* pdev) {
  int               pos          = search_forwards (":", fontname);
  string            fname        = (pos == -1 ? fontname : fontname (0, pos));
  pair<string, int> f_pair       = font_name_unpack (fname);
  string            font_basename= f_pair.x1;
  int               face_index   = f_pair.x2;
  url               u            = tt_font_find (font_basename);
  fz_context*       ctx          = mupdf_context ();
  if (!is_none (u)) {
    pdf_font_desc* fontdesc= NULL;
    {
      c_string path (concretize (u));
      fz_font* font=
          fz_new_font_from_file (mupdf_context (), NULL, path, face_index, 0);
      if (font) {
        bool is_otf= false;
        if (font->buffer && font->buffer->len > 4 &&
            !memcmp (font->buffer->data, "ttcf", 4)) {
          // TTC Font file
          fz_buffer* ttf_buf= fz_extract_ttf_from_ttc (ctx, font);
          fz_drop_font (ctx, font);
          font= fz_new_font_from_buffer (ctx, NULL, ttf_buf, 0, 0);

          unsigned char* data;
          size_t         size= fz_buffer_storage (ctx, ttf_buf, &data);
          if (size > 12 && !memcmp ("OTTO", data, 4)) {
            is_otf= true;
          }
          fz_drop_buffer (ctx, ttf_buf);
        }
        pdf_obj* fres= pdf_add_cid_font (ctx, pdoc, font);
        fontdesc     = pdf_load_font (ctx, pdoc, NULL, fres);
        pdf_drop_obj (ctx, fres);
        if (is_otf) {
          fz_drop_font (ctx, fontdesc->font);
          fontdesc->font= font;
          fz_keep_font (ctx, font);
        }
        fz_drop_font (ctx, font);
      }
    }
    if (fontdesc != NULL) {
      return fontdesc;
    }
    else {
      convert_warning << "mupdf_print_renderer_rep, font: " << fname
                      << " in file " << u << " cannot be loaded. "
                      << "Will be rendered as pixmaps" << LF;
    }
  }
  return NULL;
}

pdf_font_desc*
mupdf_print_renderer_rep::get_font_desc (string fontname) {
  pdf_font_desc* fontdesc= NULL;
  if (cfn != fontname) {
    // change font
    cfn= fontname;
    // try to find a native font
    if (!native_fonts->contains (fontname)) {
      fontdesc= load_pdf_font (fontname, doc, dev);
      if (fontdesc) {
        native_fonts (fontname)= mupdf_font (fontdesc);
        pdf_drop_font (mupdf_context (), fontdesc);
      }
      else {
        native_fonts (fontname)= mupdf_font (NULL);
        // this means use bitmap glyphs
      }
    }
    else {
      fontdesc= native_fonts (fontname)->fn;
    }
    if (fontdesc) {
      // we have a native font
      fsize= mupdf_font_size (fontname);
      proc->op_Tf (mupdf_context (), proc, "draw", fontdesc,
                   fsize * retina_factor);
    }
  }
  else {
    fontdesc= native_fonts (fontname)->fn;
  }
  return fontdesc;
}

/******************************************************************************
 * Metadata and other miscellaneous
 ******************************************************************************/

void
mupdf_print_renderer_rep::flush_metadata () {
  if (N (metadata) == 0) return;
  fz_context*  ctx = mupdf_context ();
  fz_document* fdoc= &doc->super;
  if (metadata->contains ("title"))
    fz_set_metadata (ctx, fdoc, FZ_META_INFO_TITLE,
                     as_charp (metadata["title"]));
  if (metadata->contains ("author"))
    fz_set_metadata (ctx, fdoc, FZ_META_INFO_AUTHOR,
                     as_charp (metadata["author"]));
  if (metadata->contains ("subject"))
    fz_set_metadata (ctx, fdoc, FZ_META_INFO_SUBJECT,
                     as_charp (metadata["subject"]));
  if (metadata->contains ("keyword"))
    fz_set_metadata (ctx, fdoc, FZ_META_INFO_KEYWORDS,
                     as_charp (metadata["keyword"]));

  string creator = string (STEM_NAME " " XMACS_VERSION);
  string producer= creator * " + MuPDF " * string (FZ_VERSION);
  fz_set_metadata (ctx, fdoc, FZ_META_INFO_CREATOR, as_charp (creator));
  fz_set_metadata (ctx, fdoc, FZ_META_INFO_PRODUCER, as_charp (producer));

  char   buf[32];
  time_t ti;
  time (&ti);
  strftime (buf, sizeof (buf), as_charp ("%Y%m%d%H%M%S"), gmtime (&ti));
  fz_set_metadata (ctx, fdoc, FZ_META_INFO_CREATIONDATE, buf);
}

void
mupdf_print_renderer_rep::set_metadata (string kind, string val) {
  metadata (kind)= val;
}

/******************************************************************************
 * user interface
 ******************************************************************************/

renderer
mupdf_print_renderer (url pdf_file_name, int dpi, int nr_pages,
                      string page_type, bool landscape, double paper_w,
                      double paper_h) {
  debug_std << "MuPDF print to " << pdf_file_name << " at " << dpi << " dpi\n";
  page_type= as_string (call ("standard-paper-size", object (page_type)));
  return tm_new<mupdf_print_renderer_rep> (
      pdf_file_name, dpi, nr_pages, page_type, landscape, paper_w, paper_h);
}