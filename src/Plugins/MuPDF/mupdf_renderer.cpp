
/******************************************************************************
 * MODULE     : mupdf_renderer.cpp
 * DESCRIPTION: Raster device with MuPDF
 * COPYRIGHT  : (C) 2022 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "mupdf_renderer.hpp"
#include "Freetype/tt_face.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "font.hpp"
#include "frame.hpp"
#include "image_files.hpp"

#include "Freetype/free_type.hpp"
#include "Freetype/tt_file.hpp"

#include <mupdf/pdf.h>

#include "mupdf_picture.hpp"
#include "tm_debug.hpp"
#include "tm_url.hpp"
#include "unicode.hpp"

// manage a single global context for fitz
fz_context*
mupdf_context () {
  static fz_context* ctx= NULL;
  if (!ctx) {
    ctx= fz_new_context (NULL, NULL, FZ_STORE_UNLIMITED);
    if (DEBUG_STD) {
      debug_std << "Use MuPDF render(" << FZ_VERSION << ")\n";
    }
  }
  return ctx;
}

// global auxiliary document needed to invoke some functions
pdf_document*
mupdf_document () {
  static pdf_document* doc= NULL;
  if (!doc) {
    doc= pdf_create_document (mupdf_context ());
  }
  return doc;
}

bool           mupdf_renderer_rep::clip_active= false;
pdf_processor* mupdf_renderer_rep::clip_proc;

/******************************************************************************
 * Fitz pixmaps
 ******************************************************************************/

struct mupdf_pixmap_rep : concrete_struct {
  fz_pixmap* img;
  SI         xo, yo;
  int        w, h;
  mupdf_pixmap_rep (fz_pixmap* img2, SI xo2, SI yo2, int w2, int h2)
      : img (img2), xo (xo2), yo (yo2), w (w2), h (h2) {
    fz_keep_pixmap (mupdf_context (), img);
  }
  ~mupdf_pixmap_rep () { fz_drop_pixmap (mupdf_context (), img); }
  friend class mupdf_pixmap;
};

class mupdf_pixmap {
  CONCRETE_NULL (mupdf_pixmap);
  mupdf_pixmap (fz_pixmap* img2, SI xo2, SI yo2, int w2, int h2)
      : rep (tm_new<mupdf_pixmap_rep> (img2, xo2, yo2, w2, h2)) {}
};

CONCRETE_NULL_CODE (mupdf_pixmap);

/******************************************************************************
 * Fitz images
 ******************************************************************************/

struct mupdf_image_rep : concrete_struct {
  int       w, h, xo, yo;
  fz_image* img;
  mupdf_image_rep (fz_image* img2) : img (img2) {
    fz_keep_image (mupdf_context (), img);
    // get pixmap size
    fz_pixmap* pix=
        fz_get_pixmap_from_image (mupdf_context (), img, NULL, NULL, &w, &h);
    fz_drop_pixmap (mupdf_context (), pix);
    xo= yo= 0;
  }
  ~mupdf_image_rep () { fz_drop_image (mupdf_context (), img); }
  friend class mupdf_image;
};

class mupdf_image {
  CONCRETE_NULL (mupdf_image);
  mupdf_image (fz_image* img2) : rep (tm_new<mupdf_image_rep> (img2)) {}
};

CONCRETE_NULL_CODE (mupdf_image);

/******************************************************************************
 * pdf patterns
 ******************************************************************************/

struct mupdf_pattern_rep : concrete_struct {
  pdf_pattern* pat;
  mupdf_pattern_rep (pdf_pattern* _pat) : pat (_pat) {
    pdf_keep_pattern (mupdf_context (), pat);
  }
  ~mupdf_pattern_rep () { pdf_drop_pattern (mupdf_context (), pat); }
  friend class mupdf_pattern;
};

class mupdf_pattern {
  CONCRETE_NULL (mupdf_pattern);
  mupdf_pattern (pdf_pattern* _pat) : rep (tm_new<mupdf_pattern_rep> (_pat)) {}
};

CONCRETE_NULL_CODE (mupdf_pattern);

/******************************************************************************
 * pdf fonts
 ******************************************************************************/

struct mupdf_font_rep : concrete_struct {
  pdf_font_desc* fn;
  mupdf_font_rep (pdf_font_desc* _fn) : fn (_fn) {
    pdf_keep_font (mupdf_context (), fn);
  }
  ~mupdf_font_rep () { pdf_drop_font (mupdf_context (), fn); }
  friend class mupdf_font;
};

class mupdf_font {
  CONCRETE_NULL (mupdf_font);
  mupdf_font (pdf_font_desc* _fn) : rep (tm_new<mupdf_font_rep> (_fn)) {}
};

CONCRETE_NULL_CODE (mupdf_font);

/******************************************************************************
 * Global support variables for all mupdf_renderers
 ******************************************************************************/

// bitmaps of all characters
static hashmap<basic_character, mupdf_image> character_image;

// caches
static hashmap<unsigned long long int, mupdf_image> picture_pool;
static hashmap<tree, mupdf_image>                   image_pool;
static hashmap<tree, mupdf_pattern>                 pattern_pool;
static hashmap<tree, mupdf_image>                   pattern_image_pool;
static hashmap<string, mupdf_font>                  native_fonts;

// flush caches
void
del_obj_mupdf_renderer (void) {
  character_image   = hashmap<basic_character, mupdf_image> ();
  image_pool        = hashmap<tree, mupdf_image> ();
  picture_pool      = hashmap<unsigned long long int, mupdf_image> ();
  pattern_pool      = hashmap<tree, mupdf_pattern> ();
  pattern_image_pool= hashmap<tree, mupdf_image> ();
  native_fonts      = hashmap<string, mupdf_font> ();
}

/******************************************************************************
 * mupdf_renderer
 ******************************************************************************/

mupdf_renderer_rep::mupdf_renderer_rep (int w2, int h2)
    : basic_renderer_rep (true, w2, h2), pixmap (NULL), dev (NULL), proc (NULL),
      fg (-1), bg (-1), lw (-1), in_text (false), cfn ("") {
  reset_zoom_factor ();
}

mupdf_renderer_rep::~mupdf_renderer_rep () { end (); }

void*
mupdf_renderer_rep::get_handle () {
  return (void*) this;
}

void
mupdf_renderer_rep::get_extents (int& w2, int& h2) {
  if (pixmap) {
    w2= fz_pixmap_width (mupdf_context (), pixmap);
    h2= fz_pixmap_height (mupdf_context (), pixmap);
  }
  else {
    w2= w;
    h2= h;
  }
}

void
mupdf_renderer_rep::set_zoom_factor (double zoom) {
  renderer_rep::set_zoom_factor (retina_factor * zoom);
  retina_pixel= pixel * retina_factor;
}

void
mupdf_renderer_rep::begin (void* handle) {
  fz_pixmap* _pixmap= static_cast<fz_pixmap*> (handle);
  if (_pixmap) {
    fz_context* ctx= mupdf_context ();
    if (dev) end ();
    pixmap= _pixmap;
    fz_keep_pixmap (ctx, pixmap);
    w            = fz_pixmap_width (ctx, pixmap);
    h            = fz_pixmap_height (ctx, pixmap);
    dev          = fz_new_draw_device (ctx, fz_identity, pixmap);
    fz_matrix ctm= fz_make_matrix (1, 0, 0, -1, 0, 0);
    proc= pdf_new_run_processor (ctx, mupdf_document (), dev, ctm, -1, "View",
                                 NULL, NULL, NULL);

    fg              = -1;
    bg              = -1;
    lw              = -1;
    cfn             = "";
    in_text         = false;
    transform_active= false;

    // outmost save of the graphics state
    proc->op_q (mupdf_context (), proc);
    // set scaling suitable for dpi (pdf default is 72)
    proc->op_cm (mupdf_context (), proc, 1, 0, 0, 1, 0, 0);

    // set_origin(0, -500);
    // set_origin (0, h*pixel);
    // set_clipping (0, (int) (-h*pixel), (int) (w*pixel), 0);
  }
  else {
    if (DEBUG_STD) {
      debug_std << "mupdf_renderer_rep::begin : invalid pixmap" << LF;
    }
  }
}

void
mupdf_renderer_rep::end () {
  end_text ();

  if (proc) {
    // reset set_clipping calls in order to have well formed PDF.
    if (clip_active) {
      clip_proc->op_Q (mupdf_context (), clip_proc);
      clip_active= false;
      clip_proc  = NULL;
    }
    // outmost restore for the graphics state (see begin_page)
    proc->op_Q (mupdf_context (), proc);

    pdf_close_processor (mupdf_context (), proc);
    pdf_drop_processor (mupdf_context (), proc);
    proc= NULL;
  }
  if (dev) {
    fz_close_device (mupdf_context (), dev);
    fz_drop_device (mupdf_context (), dev);
    dev= NULL;
  }
  if (pixmap) {
    fz_drop_pixmap (mupdf_context (), pixmap);
    pixmap= NULL;
  }
}

void
mupdf_renderer_rep::begin_text () {
  if (!in_text) {
    in_text= true;
    proc->op_BT (mupdf_context (), proc);
    prev_text_x= to_x (0);
    prev_text_y= to_y (0);
    if (transform_active) {
      fz_point after_trans=
          fz_transform_point_xy (prev_text_x, prev_text_y, transform_matrix);
      proc->op_Tm (mupdf_context (), proc, 1, 0, 0, 1, after_trans.x,
                   after_trans.y);
    }
    else {
      proc->op_Tm (mupdf_context (), proc, 1, 0, 0, 1, prev_text_x,
                   prev_text_y);
    }
  }
}

void
mupdf_renderer_rep::end_text () {
  if (in_text) {
    in_text= false;
    proc->op_ET (mupdf_context (), proc);
  }
}

/******************************************************************************
 * Transformations
 ******************************************************************************/

void
mupdf_renderer_rep::set_transformation (frame fr) {
  ASSERT (fr->linear, "only linear transformations have been implemented");

  end_text ();

  SI cx1, cy1, cx2, cy2;
  get_clipping (cx1, cy1, cx2, cy2);
  rectangle oclip (cx1, cy1, cx2, cy2);

  frame cv= scaling (point (pixel, -pixel), point (-ox, -oy));
  frame tr= invert (cv) * fr * cv;
  point o = tr (point (0.0, 0.0));
  point ux= tr (point (1.0, 0.0)) - o;
  point uy= tr (point (0.0, 1.0)) - o;
  // cout << "Set transformation " << o << ", " << ux << ", " << uy << "\n";

  proc->op_q (mupdf_context (), proc);
  transform_matrix= fz_make_matrix (ux[0], ux[1], uy[0], uy[1], o[0], o[1]);
  proc->op_cm (mupdf_context (), proc, transform_matrix.a, transform_matrix.b,
               transform_matrix.c, transform_matrix.d, transform_matrix.e,
               transform_matrix.f);
  transform_matrix= fz_invert_matrix (transform_matrix);
  transform_active= true;
}

void
mupdf_renderer_rep::reset_transformation () {
  end_text ();
  cfn= "";
  proc->op_Q (mupdf_context (), proc);
  transform_active= false;
}

/******************************************************************************
 * Clipping
 ******************************************************************************/

void
mupdf_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);

  end_text ();

  outer_round (x1, y1, x2, y2);
  if (restore) {
    cfn= "";
  }
  if (clip_proc != proc) {
    if (clip_proc != NULL) clip_proc->op_Q (mupdf_context (), clip_proc);
    clip_proc  = proc;
    clip_active= false;
  }
  if (clip_active) {
    proc->op_Q (mupdf_context (), proc);
  }
  else {
    clip_active= true;
  }

  proc->op_q (mupdf_context (), proc);
  SI xx1= to_x (min (x1, x2));
  SI yy1= to_y (min (y1, y2));
  SI xx2= to_x (max (x1, x2));
  SI yy2= to_y (max (y1, y2));
  proc->op_re (mupdf_context (), proc, xx1, yy1, xx2 - xx1, yy2 - yy1);
  proc->op_W (mupdf_context (), proc);
  proc->op_n (mupdf_context (), proc);
}

/******************************************************************************
 * Graphic state management
 ******************************************************************************/

void
mupdf_renderer_rep::select_alpha (int alpha) {
  float da= ((float) alpha) / 1000.0;
  proc->op_gs_ca (mupdf_context (), proc, da);
  proc->op_gs_CA (mupdf_context (), proc, da);
}

void
mupdf_renderer_rep::select_stroke_color (color c) {
  ;
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  r       = ((r * 1000) / 255);
  g       = ((g * 1000) / 255);
  b       = ((b * 1000) / 255);
  a       = ((a * 1000) / 255);
  float dr= ((float) r) / 1000.0;
  float dg= ((float) g) / 1000.0;
  float db= ((float) b) / 1000.0;
  proc->op_RG (mupdf_context (), proc, dr, dg, db); // stroke color
  select_alpha (a);
}

void
mupdf_renderer_rep::select_fill_color (color c) {
  ;
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  r       = ((r * 1000) / 255);
  g       = ((g * 1000) / 255);
  b       = ((b * 1000) / 255);
  a       = ((a * 1000) / 255);
  float dr= ((float) r) / 1000.0;
  float dg= ((float) g) / 1000.0;
  float db= ((float) b) / 1000.0;
  proc->op_rg (mupdf_context (), proc, dr, dg, db); // non-stroking color
  select_alpha (a);
}

static mupdf_image
get_image (url u, int w, int h, tree eff, SI pixel) {
  mupdf_image mpim= mupdf_image ();
  fz_pixmap*  pix = mupdf_load_pixmap (u, w, h, eff, pixel);
  if (pix) {
    fz_image* im= fz_new_image_from_pixmap (mupdf_context (), pix, NULL);
    fz_drop_pixmap (mupdf_context (), pix);
    mpim= mupdf_image (im);
  }
  return mpim;
}

void
mupdf_renderer_rep::register_pattern (brush br, SI pixel) {
  // debug_convert << "register_pattern_image\n";
  if (is_nil (br) || br->get_type () != brush_pattern) {
    convert_warning << "mupdf_renderer_rep::register_pattern_image: "
                    << "brush with pattern expected\n";
    return;
  }
  tree p= br->get_pattern ();
  // debug_convert << p << "\n";
  if (pattern_pool->contains (p)) return;

  url  u;
  SI   w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, pixel);
  tree key= tuple (as_tree (u), as_string (w), as_string (h), eff);

  mupdf_image image_pdf;
  if (pattern_image_pool->contains (key)) image_pdf= pattern_image_pool[key];
  else {
    // debug_convert << "Insert pattern image\n";
    image_pdf= get_image (u, w, h, eff, pixel);
    if (is_nil (image_pdf)) {
      convert_error
          << "mupdf_renderer_rep::register_pattern : Cannot read image file '"
          << u << "'"
          << " with get_image" << LF;
      return;
    }
    if (w != image_pdf->w || h != image_pdf->h) {
      convert_error
          << "mupdf_renderer_rep::register_pattern : Invalid image size '" << u
          << "'"
          << " after get_image" << LF;
      return;
    }
    pattern_image_pool (key)= image_pdf;
  }

  fz_context*   ctx   = mupdf_context ();
  pdf_document* doc   = mupdf_document ();
  pdf_obj*      subres= pdf_new_dict (ctx, doc, 2);
  pdf_obj*      xres  = pdf_new_dict (ctx, doc, 2);
  pdf_obj*      ref   = pdf_add_image (ctx, doc, image_pdf->img);
  pdf_dict_puts (ctx, xres, "pattern-image", ref);
  pdf_dict_puts (ctx, subres, "XObject", xres);
  pdf_drop_obj (ctx, ref);

  fz_buffer* buf= fz_new_buffer (ctx, 0);
  //  fz_output *out= fz_new_output_with_buffer(ctx, buf);
  {
    pdf_processor* pout= pdf_new_buffer_processor (ctx, buf, 0, 0);
    pout->op_q (ctx, pout);
    pout->op_cm (ctx, pout, w, 0, 0, h, 0, 0);
    pout->op_Do_image (ctx, pout, "pattern-image", NULL);
    pout->op_Q (ctx, pout);
    pdf_close_processor (ctx, pout);
  }
  pdf_obj* contents=
      pdf_add_stream (ctx, doc, buf, NULL /* dict */, 0 /* compress */);
  fz_drop_buffer (ctx, buf);
  {
    // make a pdf_pattern
    int   width  = fz_pixmap_width (ctx, pixmap);
    int   height = fz_pixmap_height (ctx, pixmap);
    SI    sx     = width + to_x (0); // FIXME: ??
    SI    sy     = height;           // FIXME: ??
    float scale_x= 1.0;              //((float) default_dpi) / dpi;
    float scale_y= 1.0;              //((float) default_dpi) / dpi;

    // const float matrix[]= { scale_x, 0, 0, scale_y, (float) sx, (float) sy };

    pdf_pattern* pat= fz_malloc_struct (ctx, pdf_pattern);
    pat->document   = doc;
    pat->id         = 0; // pdf_to_num (ctx, dict);
    pat->ismask= 0; // pdf_dict_get_int(ctx, dict, PDF_NAME(PaintType)) == 2;
    pat->xstep = w; // pdf_dict_get_real(ctx, dict, PDF_NAME(XStep));
    pat->ystep = h; // pdf_dict_get_real(ctx, dict, PDF_NAME(YStep));
    pat->bbox  = fz_make_rect (
        0, 0, w, h); // pdf_dict_get_rect(ctx, dict, PDF_NAME(BBox));
    pat->matrix= fz_make_matrix (
        scale_x, 0, 0, scale_y, (float) sx,
        (float) sy);        // pdf_dict_get_matrix(ctx, dict, PDF_NAME(Matrix));
    pat->resources= subres; // we already own it
    // pdf_keep_obj (ctx, pat->resources);
    pat->contents= contents; // we already own it
    // pdf_keep_obj (ctx, pat->contents);

    // debug_convert << "  insert pattern\n";
    // debug_convert << "pdf_pattern " << ox << ", " << oy
    //         << ", " << pixel << ", " << shrinkf
    //       << ", " << zoomf << LF;
    // debug_convert << "            " << to_x(0) << ", " << to_y(0) << LF;
    // debug_convert << "            " << w << ", " << h << LF;

    mupdf_pattern p_pdf (pat);
    pdf_drop_pattern (ctx, pat);
    pattern_pool (p)= p_pdf;
  }
}

void
mupdf_renderer_rep::select_stroke_pattern (brush br) {
  if (is_nil (br) || br->get_type () != brush_pattern) return;
  tree p_tree= br->get_pattern ();
  register_pattern (br, brushpx == -1 ? pixel : brushpx);
  if (!pattern_pool->contains (p_tree)) {
    convert_error << "mupdf_renderer_rep::select_stroke_pattern: "
                  << "cannot find registered pattern\n";
    return;
  }
  mupdf_pattern p= pattern_pool[p_tree];
  proc->op_CS (mupdf_context (), proc, "Pattern",
               fz_device_rgb (mupdf_context ()));
  proc->op_SC_pattern (mupdf_context (), proc, "*stroke-pattern*", p->pat, 0,
                       NULL);
}

void
mupdf_renderer_rep::select_fill_pattern (brush br) {
  if (is_nil (br) || br->get_type () != brush_pattern) return;
  tree p_tree= br->get_pattern ();
  register_pattern (br, brushpx == -1 ? pixel : brushpx);
  if (!pattern_pool->contains (p_tree)) {
    convert_error << "mupdf_renderer_rep::select_fill_pattern: "
                  << "cannot find registered pattern\n";
    return;
  }
  mupdf_pattern p= pattern_pool[p_tree];
  proc->op_CS (mupdf_context (), proc, "Pattern",
               fz_device_rgb (mupdf_context ()));
  proc->op_sc_pattern (mupdf_context (), proc, "*fill-pattern*", p->pat, 0,
                       NULL);
  select_alpha ((1000 * br->get_alpha ()) / 255);
}

void
mupdf_renderer_rep::select_line_width (SI w) {
  float pw= (float) w / pixel;
  proc->op_w (mupdf_context (), proc, pw);
}

void
mupdf_renderer_rep::set_pencil (pencil pen2) {
  // debug_convert << "set_pencil\n";
  pen= pen2;
  lw = pen->get_width ();
  select_line_width (lw);
  color c= pen->get_color ();
  fg     = c;
  select_fill_color (c);
  select_stroke_color (c);
  if (pen->get_type () == pencil_brush) {
    // debug_convert << "pencil has brush type" << LF;
    brush br= pen->get_brush ();
    fg_brush= br;
    select_fill_pattern (br);
    select_stroke_pattern (br);
  }
  if (pen->get_cap () == cap_round)
    proc->op_J (mupdf_context (), proc, 1);    // round cap
  else proc->op_J (mupdf_context (), proc, 2); // square cap
  proc->op_j (mupdf_context (), proc, 1);      // round join
}

void
mupdf_renderer_rep::set_brush (brush br) {
  // debug_convert << "set_brush\n";
  fg_brush= br;
  pen     = pencil (br);
  set_pencil (pen); // FIXME ???
  if (is_nil (br)) return;
  if (br->get_type () == brush_none) {
    pen     = pencil ();
    fg_brush= brush ();
  }
  else {
    select_fill_color (pen->get_color ());
    select_stroke_color (pen->get_color ());
  }
  if (br->get_type () == brush_pattern) {
    tree p_tree= br->get_pattern ();
    register_pattern (br, brushpx == -1 ? pixel : brushpx);
    if (!pattern_pool->contains (p_tree)) {
      convert_error << "mupdf_renderer_rep::set_brush: "
                    << "cannot find registered pattern\n";
      return;
    }
    select_fill_pattern (br);
    select_stroke_pattern (br);
  }
  // select_alpha (br->get_alpha ());
}
void
mupdf_renderer_rep::set_background (brush b) {
  // debug_convert << "set_background\n";
  bg_brush= b;
  bg      = b->get_color ();
}

/******************************************************************************
 * Graphics primitives
 ******************************************************************************/

void
mupdf_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  // debug_convert << "line\n";
  end_text ();
  proc->op_m (mupdf_context (), proc, to_x_double (x1), to_y_double (y1));
  proc->op_l (mupdf_context (), proc, to_x_double (x2), to_y_double (y2));
  proc->op_S (mupdf_context (), proc);
}

void
mupdf_renderer_rep::lines (array<SI> x, array<SI> y) {
  // debug_convert << "lines\n";
  end_text ();
  int i, n= N (x);
  if ((N (y) != n) || (n < 1)) return;
  end_text ();
  proc->op_q (mupdf_context (), proc);
  if (pen->get_cap () == cap_round ||
      (x[N (x) - 1] == x[0] && y[N (y) - 1] == y[0]))
    proc->op_J (mupdf_context (), proc, 1);    // round cap
  else proc->op_J (mupdf_context (), proc, 2); // square cap
  proc->op_j (mupdf_context (), proc, 1);      // round join
  proc->op_m (mupdf_context (), proc, to_x_double (x[0]), to_y_double (y[0]));
  for (i= 1; i < n; i++) {
    proc->op_l (mupdf_context (), proc, to_x_double (x[i]), to_y_double (y[i]));
  }
  proc->op_S (mupdf_context (), proc);
  proc->op_Q (mupdf_context (), proc);
}

void
mupdf_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  end_text ();
  SI xx1= to_x (min (x1, x2));
  SI yy1= to_y (min (y1, y2));
  SI xx2= to_x (max (x1, x2));
  SI yy2= to_y (max (y1, y2));
  // debug_convert << "clear" << xx1 << " " << yy1 << " " << xx2 << " " << yy2
  // << LF;
  proc->op_q (mupdf_context (), proc);
  select_fill_color (bg);
  select_fill_pattern (bg_brush);
  proc->op_re (mupdf_context (), proc, xx1, yy1, xx2 - xx1, yy2 - yy1);
  proc->op_h (mupdf_context (), proc);
  proc->op_f (mupdf_context (), proc);
  select_fill_color (fg);
  select_fill_pattern (fg_brush);
  proc->op_Q (mupdf_context (), proc);
}

void
mupdf_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if ((x1 < x2) && (y1 < y2)) {
    end_text ();
    SI xx1= to_x (min (x1, x2));
    SI yy1= to_y (min (y1, y2));
    SI xx2= to_x (max (x1, x2));
    SI yy2= to_y (max (y1, y2));
    proc->op_re (mupdf_context (), proc, xx1, yy1, xx2 - xx1, yy2 - yy1);
    proc->op_h (mupdf_context (), proc);
    proc->op_f (mupdf_context (), proc); // FIXME Winding
  }
}

void
mupdf_renderer_rep::bezier_arc (SI x1, SI y1, SI x2, SI y2, int alpha,
                                int delta, bool filled) {
  // PDF can describe only cubic bezier paths, so we have to make up the arc
  // with them. Since this is not mathematically exact, we minimize errors by
  // drawing beziers sub-arcs of at most 90??
  end_text ();
  proc->op_q (mupdf_context (), proc); // save graphics state

  float xx1= to_x_double (x1), yy1= to_y_double (y1), xx2= to_x_double (x2),
        yy2= to_y_double (y2);
  float cx= (xx1 + xx2) / 2, cy= (yy1 + yy2) / 2;
  float rx= (xx2 - xx1) / 2, ry= (yy2 - yy1) / 2;
  proc->op_cm (mupdf_context (), proc, 1, 0, 0, 1, cx, cy); // centering
  // we can't apply scale here because in pdf the pen is scaled too

  int i= 1 + abs (delta) / (90 * 64); // number of sub-arcs needed
  if ((abs (delta) % (90 * 64)) == 0)
    i--; // correction needed if exact multiple of 90??
  float phi=
      2.0 * M_PI * (delta) / (i * 360.0 * 64.0); // angular span of each sub-arc
  float a= 2.0 * M_PI * (alpha) / (360.0 * 64.0); // start angle in radians

  // Control points for an arc of radius 1, centered on the x-axis and
  // spanning phi degrees. From: http://www.tinaja.com/glib/bezcirc2.pdf
  float sphi= sin (phi / 2), cphi= cos (phi / 2);
  float bx0= cphi, by0= -sphi;
  float bx1= (4.0 - bx0) / 3.0, by1= (1.0 - bx0) * (3.0 - bx0) / (3.0 * by0);
  float bx2= bx1, by2= -by1;
  float bx3= bx0, by3= -by0;

  // repeatedly draw rotated and scaled sub-arc
  // cannot use user-space transformations with cm util path is painted
  // (otherwise path is lost) so we perform explicit rotation+scaling
  // calculations
  int k;
  for (k= 0; k < i; k++) {
    sphi= sin (phi * (k + 0.5) + a);
    cphi= cos (phi * (k + 0.5) + a);
    if (k == 0) {
      // start point
      proc->op_m (mupdf_context (), proc, rx * (bx0 * cphi - by0 * sphi),
                  ry * (+bx0 * sphi + by0 * cphi));
    }
    proc->op_c (mupdf_context (), proc, rx * (bx1 * cphi - by1 * sphi),
                ry * (+bx1 * sphi + by1 * cphi), rx * (bx2 * cphi - by2 * sphi),
                ry * (+bx2 * sphi + by2 * cphi), rx * (bx3 * cphi - by3 * sphi),
                ry * (+bx3 * sphi + by3 * cphi));
  }

  // paint
  if (filled) {
    // proc->op_l (mupdf_context (), proc, 0.0, 0.0); // for a filled "pie"
    // with vertex at the center
    proc->op_f (mupdf_context (), proc);
  }
  else {
    // here we close the path if it's a full circle
    if (abs (delta) == 360 * 64) proc->op_s (mupdf_context (), proc);
    else proc->op_S (mupdf_context (), proc);
  }
  // restore the graphics state (undoes centering only)
  proc->op_Q (mupdf_context (), proc);
}

void
mupdf_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  // debug_convert << "arc\n";
  end_text ();
  bezier_arc (x1, y1, x2, y2, alpha, delta, false);
}

void
mupdf_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha,
                              int delta) {
  // debug_convert << "fill_arc\n";
  end_text ();
  bezier_arc (x1, y1, x2, y2, alpha, delta, true);
}

void
mupdf_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  // debug_convert << "polygon\n";
  int i, n= N (x);
  if ((N (y) != n) || (n < 1)) return;
  end_text ();

  proc->op_m (mupdf_context (), proc, to_x_double (x[0]), to_y_double (y[0]));
  for (i= 1; i < n; i++)
    proc->op_l (mupdf_context (), proc, to_x_double (x[i]), to_y_double (y[i]));
  proc->op_h (mupdf_context (), proc);
  if (convex) proc->op_f (mupdf_context (), proc); // odd-even
  else proc->op_fstar (mupdf_context (), proc);    // nonzero winding
}

/******************************************************************************
 * Image rendering
 ******************************************************************************/

static void
set_default_gstate (fz_context* ctx, pdf_processor* proc) {
  //  buf << "<< /Type /ExtGState\r\n";
  //  buf << "/LW 1.0\r\n";
  proc->op_w (ctx, proc, 1.0);
  //  buf << "/LC 0\r\n";
  proc->op_J (ctx, proc, 0);
  //  buf << "/LJ 0\r\n";
  proc->op_j (ctx, proc, 0);
  //  buf << "/ML 10.0\r\n";
  proc->op_M (ctx, proc, 10.0);
  //       //buf << "/D [[] 0]\r\n"; // useless
  //  buf << "/RI /RelativeColorimetric\r\n";
  proc->op_ri (ctx, proc, "RelativeColorimetric");
  //  buf << "/OP false\r\n";
  proc->op_gs_OP (ctx, proc, 0);
  //  buf << "/op false\r\n";
  proc->op_gs_op (ctx, proc, 0);
  //  buf << "/FL 1.0\r\n";
  proc->op_i (ctx, proc, 1.0);
  //  buf << "/SA false\r\n"; // Automatic Stroke Adjustement
  // not available in mupdf apparently
  //  buf << "/BM /Normal\r\n";
  proc->op_gs_BM (ctx, proc, "Normal");
  //  buf << "/SMask /None\r\n";
  proc->op_gs_SMask (ctx, proc, NULL, NULL, NULL, 0, NULL);
  //  buf << "/CA 1.0\r\n";
  proc->op_gs_CA (ctx, proc, 1.0);
  //  buf << "/ca 1.0\r\n";
  proc->op_gs_ca (ctx, proc, 1.0);
  //  buf << "/AIS false\r\n"; // Alpha is shape
  // not available in mupdf apparently
  //  buf << "/TK true\r\n"; // text knockout flag
  // not available in mupdf apparently
}

static void
image (fz_context* ctx, pdf_processor* proc, mupdf_image im, int alpha, float a,
       float b, float c, float d, float e, float f) {
  // debug_convert << "mupdf_renderer_rep::image " << u << ", " << w << " x " <<
  // h
  //    << " + (" << x << ", " << y << ")" << LF;
  proc->op_q (ctx, proc);
  set_default_gstate (ctx, proc);
  proc->op_cm (ctx, proc, a, b, c, d, e, f);
  float da= ((float) alpha) / 255.0;
  proc->op_gs_ca (ctx, proc, da);
  proc->op_gs_CA (ctx, proc, da);
  proc->op_Do_image (ctx, proc, "Image", im->img);
  // proc->op_re (ctx, proc, 0, 0, 1, 1);
  // proc->op_S (ctx, proc);
  proc->op_Q (ctx, proc);
}

void
mupdf_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  p                      = as_mupdf_picture (p);
  mupdf_picture_rep* pict= (mupdf_picture_rep*) p->get_handle ();
  if (!pict->im) {
    // let's cache the image representation of the pixmap
    // it will be dropped by the object
    pict->im= fz_new_image_from_pixmap (mupdf_context (), pict->pix, NULL);
  }
  int w= p->get_width (), h= p->get_height ();
  int ox= p->get_origin_x (), oy= p->get_origin_y ();
  end_text ();
  image (mupdf_context (), proc, pict->im, alpha, w, 0, 0, h,
         to_x (x - ox * pixel), to_y (y - oy * pixel));
}

void
mupdf_renderer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  // debug_convert << "pdf renderer, draw_scalable "
  //   << im->get_name () << " at " << x << ", " << y
  //   << " (" << alpha << ")" << LF;
  fz_context* ctx= mupdf_context ();
  if (im->get_type () != scalable_image ||
      (im->get_type () == scalable_image && im->get_effect () != tree ("")))
    renderer_rep::draw_scalable (im, x, y, alpha);
  else {
    url         u     = im->get_name ();
    tree        lookup= as_tree (u);
    mupdf_image im2;
    if (image_pool->contains (lookup)) im2= image_pool[lookup];
    else {
      fz_image* fzim= mupdf_load_image (u);
      if (fzim != NULL) {
        im2= mupdf_image (fzim);
        fz_drop_image (ctx, fzim);
      }
      image_pool (lookup)= im2;
    }
    if (is_nil (im2)) {
      im->draw (this, x, y);
      return;
    };
    rectangle r= im->get_logical_extents ();
    SI        w= r->x2 - r->x1, h= r->y2 - r->y1;
    int       ox= r->x1, oy= r->y1;
    end_text ();
    image (ctx, proc, im2, alpha, ((double) w) / pixel, 0, 0,
           ((double) h) / pixel, to_x (x - ox), to_y (y - oy));
  }
}

/******************************************************************************
 * Glyph rendering
 ******************************************************************************/

#if 0
void
mupdf_renderer_rep::draw_clipped (QImage *im, int w, int h, SI x, SI y) {
  (void) w; (void) h;
  int x1= cx1-ox, y1= cy2-oy, x2= cx2-ox, y2= cy1-oy;
  decode (x , y );
  decode (x1, y1);
  decode (x2, y2);
  y--; // top-left origin to bottom-left origin conversion
       // clear(x1,y1,x2,y2);
  painter->setRenderHints (0);
  painter->drawImage (x, y, *im);
}

void
mupdf_renderer_rep::draw_clipped (QPixmap *im, int w, int h, SI x, SI y) {
  decode (x , y );
  y--; // top-left origin to bottom-left origin conversion
  // clear(x1,y1,x2,y2);
  painter->setRenderHints (0);
  painter->drawPixmap (x, y, w, h, *im);
}

void
mupdf_renderer_rep::draw_bis (int c, font_glyphs fng, SI x, SI y) {
  // draw with background pattern
  SI xo, yo;
  glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
  glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
  int w= gl->width, h= gl->height;
  QImage *im= new QImage (w, h, QImage::Format_ARGB32);
  im->fill (Qt::transparent);

  {
    brush br= pen->get_brush ();
    QImage* pm= get_pattern_image (br, brushpx==-1? pixel: brushpx);
    int pattern_alpha= br->get_alpha ();
    QPainter glim (im);
    glim.setOpacity (qreal (pattern_alpha) / qreal (255));
    if (pm != NULL) {
      SI tx= x- xo*std_shrinkf, ty= y+ yo*std_shrinkf;
      decode (tx, ty); ty--;
      QBrush qbr (*pm);
      QTransform qtf= painter->transform ();
      qbr.setTransform (qtf.translate (-tx, -ty));
      glim.setBrush (qbr);
    }
    glim.setPen (Qt::NoPen);
    glim.drawRect (0, 0, w, h);

    int nr_cols= std_shrinkf*std_shrinkf;
    if (nr_cols >= 64) nr_cols= 64;
    for (int j=0; j<h; j++)
      for (int i=0; i<w; i++) {
        color patcol= im->pixel (i, j);
        int r, g, b, a;
        get_rgb (patcol, r, g, b, a);
        if (get_reverse_colors ()) reverse (r, g, b);
        int col = gl->get_x (i, j);
        im->setPixel (i, j, qRgba (r, g, b, (a*col)/nr_cols));
      }
  }

  draw_clipped (im, w, h, x- xo*std_shrinkf, y+ yo*std_shrinkf);
  delete im;
}
#endif

static pdf_font_desc*
load_pdf_font (string fontname) {
  int               pos          = search_forwards (":", fontname);
  string            fname        = (pos == -1 ? fontname : fontname (0, pos));
  pair<string, int> f_pair       = font_name_unpack (fname);
  string            font_basename= f_pair.x1;
  int               face_index   = f_pair.x2;
  url               u            = tt_font_find (font_basename);
  if (!is_none (u)) {
    pdf_font_desc* fontdesc= NULL;
    {
      // debug_convert << "fz_new_font_from_file "  << u  << LF;
      c_string path (concretize (u));
      fz_font* font=
          fz_new_font_from_file (mupdf_context (), NULL, path, face_index, 0);
      if (font) {
        fontdesc      = pdf_new_font_desc (mupdf_context ());
        fontdesc->font= font;
        fontdesc->encoding=
            pdf_load_system_cmap (mupdf_context (), "Identity-H");
        // FIXME: do we need to care about all the other fields? (seems not)
        // fix the encoding for FreeType
        // see tt_face_rep::tt_face_rep
        FT_Face face= (FT_Face) fontdesc->font->ft_face;
        ft_select_charmap (face, ft_encoding_adobe_custom);
      }
    }
    if (fontdesc != NULL) {
      return fontdesc;
    }
    else {
      convert_warning << "mupdf_renderer, font: " << fname << " in file " << u
                      << " cannot be loaded. "
                      << "Will be rendered as pixmaps" << LF;
    }
  }
  return NULL;
}

static float
font_size (string name) {
  int pos  = search_backwards (".", name);
  int szpos= pos - 1;
  while ((szpos > 0) && is_numeric (name[szpos - 1]))
    szpos--;
  double size= as_double (name (szpos, pos));
  if (size == 0) size= 10;
  int end= pos + 1;
  while (end < N (name) && is_numeric (name[end]))
    end++;
  double dpi= as_double (name (pos + 1, end));
  double mag= (size) * (dpi / 72.0);
  return mag;
}

// copied from tt_face.cpp
inline FT_UInt
decode_index (FT_Face face, int i) {
  if (i < 0xc000000) return ft_get_char_index (face, i);
  return i - 0xc000000;
}

void
mupdf_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  // emoji cache for this renderer instance
  static hashmap<index_type, picture> emoji_cache;
  if (is_emoji_character (c)) {
    if (draw_emoji (c, fng, x, y)) return;
  }

  string         fontname= fng->res_name;
  pdf_font_desc* fontdesc= NULL;

  begin_text ();

  if (cfn != fontname) {
    // change font
    cfn= fontname;
    // try to find a native font
    if (!native_fonts->contains (fontname)) {
      fontdesc= load_pdf_font (fontname);
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
      fsize= font_size (fontname);
      proc->op_Tf (mupdf_context (), proc, "draw", fontdesc,
                   fsize / std_shrinkf);
    }
  }
  else {
    fontdesc= native_fonts (fontname)->fn;
  }
  // draw glyph
  if (fontdesc) {
    proc->op_Td (mupdf_context (), proc, to_x (x) - prev_text_x,
                 to_y (y) - prev_text_y);
    prev_text_x= to_x (x);
    prev_text_y= to_y (y);
    glyph gl   = fng->get (c);
    if (is_nil (gl)) return;
    unsigned int gl_index; // = gl->index;
    {
      // apriori we already have the glyph index in gl->index
      // however we cannot trust this value since it is manipulated
      // in tt_face.cpp to go aroung a problem with glyph mapping in
      // Type1 fonts (for example) for pdf_hummus
      //
      // MuPDF seems to like the glyph value returned by
      // ft_get_char_index on the FT_Face it will use.
      FT_Face face= (FT_Face) fontdesc->font->ft_face;
      gl_index    = decode_index (face, c);
    }
    char glyphs[2]= {(char) (gl_index >> 8), (char) (gl_index)};
    proc->op_Tj (mupdf_context (), proc, glyphs, 2);
    return;
  }
  // we do not have a native font, draw a bitmap
  // we use an "immediate" approach, without trying to build a Type3 font
  // this is appropriate for raster rendering, but we need to change it
  // if we want to render to a PDF file
#if 0
  // FIXME: implement brushes!
  if (pen->get_type () == pencil_brush) {
    draw_bis (c, fng, x, y);
    return;
  }
#endif
  // get the pixmap
  color           fgc= pen->get_color ();
  basic_character xc (c, fng, std_shrinkf, fgc, 0);
  mupdf_image     mi= character_image[xc];
  if (is_nil (mi)) {
    int r, g, b, a;
    get_rgb (fgc, r, g, b, a);
    if (get_reverse_colors ()) reverse (r, g, b);
    SI    xo, yo;
    glyph pre_gl= fng->get (c);
    if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
    int   w= gl->width, h= gl->height;

    unsigned char* samples= (unsigned char*) Memento_label (
        fz_malloc (mupdf_context (), h * w * 4), "glyph_pixmap_data");
    int nr_cols= std_shrinkf * std_shrinkf;
    if (nr_cols >= 64) nr_cols= 64;
    unsigned char* d= samples;
    for (int y= 0; y < h; y++) {
      for (int x= 0; x < w; x++) {
        int col= gl->get_x (x, y);
        // we need to store premultiplied values for fz_pixmap
        int alpha= ((a * col) / nr_cols) & 0xFF;
        d[0]     = (r * alpha) / 255;
        d[1]     = (g * alpha) / 255;
        d[2]     = (b * alpha) / 255;
        d[3]     = alpha;
        d+= 4;
      }
    }
    fz_pixmap* pix= fz_new_pixmap_with_data (mupdf_context (),
                                             fz_device_rgb (mupdf_context ()),
                                             w, h, NULL, 1, w * 4, samples);
    fz_image*  im = fz_new_image_from_pixmap (mupdf_context (), pix, NULL);
    mi            = mupdf_image (im);
    mi->xo        = xo;
    mi->yo        = yo;
    character_image (xc)= mi;
    fz_drop_pixmap (mupdf_context (), pix);
    fz_drop_image (mupdf_context (), im);
  }
  // draw the character
  image (mupdf_context (), proc, mi, 255, mi->w, 0.0, 0.0, mi->h,
         to_x (x - mi->xo * std_shrinkf),
         to_y (y + mi->yo * std_shrinkf - mi->h * pixel));
}

/******************************************************************************
 * Main renderer
 ******************************************************************************/

mupdf_renderer_rep*
the_mupdf_renderer () {
  static mupdf_renderer_rep* the_renderer= NULL;
  if (!the_renderer) {
    the_renderer= tm_new<mupdf_renderer_rep> ();
  }
  return the_renderer;
}

/******************************************************************************
 * Copying regions
 ******************************************************************************/

// fitz does not have a function for low lever copy of pixmaps
// we have to do it ourselves

static void
translate_pixmap (fz_pixmap* dest_pix, fz_irect dest_rect, fz_pixmap* src_pix,
                  int dx, int dy) {
  fz_context* ctx     = mupdf_context ();
  fz_irect    src_rect= fz_translate_irect (dest_rect, -dx, -dy);
  src_rect= fz_intersect_irect (src_rect, fz_pixmap_bbox (ctx, src_pix));
  src_rect= fz_intersect_irect (
      src_rect, fz_translate_irect (fz_pixmap_bbox (ctx, dest_pix), -dx, -dy));
  dest_rect= fz_translate_irect (src_rect, dx, dy);
  if (fz_is_empty_irect (dest_rect)) return;
  // all set up, let's do the work now
  {
    unsigned char* srcp;
    unsigned char* destp;
    unsigned int   y, w;
    size_t         destspan, srcspan;
    w= (unsigned int) (dest_rect.x1 - dest_rect.x0);
    y= (unsigned int) (dest_rect.y1 - dest_rect.y0);

    srcspan= src_pix->stride;
    srcp   = src_pix->samples + srcspan * (src_rect.y0 - src_pix->y) +
          (src_rect.x0 - src_pix->x) * (size_t) src_pix->n;
    destspan= dest_pix->stride;
    destp   = dest_pix->samples + destspan * (dest_rect.y0 - dest_pix->y) +
           (dest_rect.x0 - dest_pix->x) * (size_t) dest_pix->n;

    if (src_pix->n == dest_pix->n) {
      w*= src_pix->n;
      do {
        memcpy (destp, srcp, w);
        srcp+= srcspan;
        destp+= destspan;
      } while (--y);
    }
    else {
      cout << "mupdf_renderer_rep / translate_pixmap : non compatible pixmaps"
           << LF;
    }
  }
}

void
mupdf_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x,
                           SI y) {
  ASSERT (ren != NULL, "invalid situation");
  if (ren->is_printer ()) return;
  mupdf_renderer_rep* src= (mupdf_renderer_rep*) ren->get_handle ();
  if (src->pixmap == pixmap && x1 == x && y1 == y) return;
  outer_round (x1, y1, x2, y2);
  SI X1= x1, Y1= y1;
  x1= max (x1, cx1 - ox);
  y1= max (y1, cy1 - oy);
  x2= min (x2, cx2 - ox);
  y2= min (y2, cy2 - oy);
  decode (X1, Y1);
  decode (x1, y1);
  decode (x2, y2);
  src->decode (x, y);
  x+= x1 - X1;
  y+= y1 - Y1;
  if (x1 < x2 && y2 < y1) {
    //  XCopyArea (dpy, src->win, win, gc, x, y, x2-x1, y1-y2, x1, y2);
    translate_pixmap (pixmap, fz_make_irect (x1, y2, x2, y1), src->pixmap,
                      x1 - x, y1 - y);
  }
}

/******************************************************************************
 * Shadow management methods
 ******************************************************************************/

void
mupdf_renderer_rep::new_shadow (renderer& ren) {
  SI mw, mh, sw, sh;
  get_extents (mw, mh);
  if (ren != NULL) {
    ren->get_extents (sw, sh);
    if (sw != mw || sh != mh) {
      delete_shadow (ren);
      ren= NULL;
    }
  }
  if (ren == NULL) {
    ren           = (renderer) tm_new<mupdf_renderer_rep> (mw, mh);
    fz_pixmap* pix= fz_new_pixmap (
        mupdf_context (), fz_device_rgb (mupdf_context ()), mw, mh, NULL, 1);
    static_cast<mupdf_renderer_rep*> (ren)->begin (pix);
    fz_drop_pixmap (mupdf_context (), pix);
  }
}

void
mupdf_renderer_rep::delete_shadow (renderer& ren) {
  if (ren != NULL) {
    static_cast<mupdf_renderer_rep*> (ren)->end ();
    tm_delete (ren);
    ren= NULL;
  }
}

extern "C" {
// not exported from fitz/pixmap-imp.h
void fz_copy_pixmap_rect (fz_context* ctx, fz_pixmap* dest, fz_pixmap* src,
                          fz_irect r, const fz_default_colorspaces* default_cs);
}

void
mupdf_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  mupdf_renderer_rep* shadow= static_cast<mupdf_renderer_rep*> (ren);
  outer_round (x1, y1, x2, y2);
  x1            = max (x1, cx1 - ox);
  y1            = max (y1, cy1 - oy);
  x2            = min (x2, cx2 - ox);
  y2            = min (y2, cy2 - oy);
  shadow->ox    = ox;
  shadow->oy    = oy;
  shadow->master= this;
  shadow->cx1   = x1 + ox;
  shadow->cy1   = y1 + oy;
  shadow->cx2   = x2 + ox;
  shadow->cy2   = y2 + oy;

  decode (x1, y1);
  decode (x2, y2);
  if (x1 < x2 && y2 < y1) {
    fz_irect rect= fz_make_irect (x1, y2, x2, y1);
    fz_copy_pixmap_rect (mupdf_context (), shadow->pixmap, pixmap, rect, NULL);
  }
}

void
mupdf_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  mupdf_renderer_rep* shadow= static_cast<mupdf_renderer_rep*> (ren);
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1 - ox);
  y1= max (y1, cy1 - oy);
  x2= min (x2, cx2 - ox);
  y2= min (y2, cy2 - oy);
  decode (x1, y1);
  decode (x2, y2);
  if (x1 < x2 && y2 < y1) {
    fz_irect rect= fz_make_irect (x1, y2, x2, y1);
    fz_copy_pixmap_rect (mupdf_context (), pixmap, shadow->pixmap, rect, NULL);
  }
}

void
mupdf_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  if (master == NULL) return;
  if (pixmap == static_cast<mupdf_renderer_rep*> (master)->pixmap) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  static_cast<mupdf_renderer_rep*> (master)->encode (x1, y1);
  static_cast<mupdf_renderer_rep*> (master)->encode (x2, y2);
  master->put_shadow (this, x1, y1, x2, y2);
}

static void
mupdf_pixmap_cleanup_handler (void* info) {
  fz_pixmap* pix= (fz_pixmap*) info;
  if (pix) {
    fz_drop_pixmap (mupdf_context (), pix);
  }
}

QImage
get_QImage_from_pixmap (fz_pixmap* pix) {
  uchar* samples= fz_pixmap_samples (mupdf_context (), pix);
  int    w      = fz_pixmap_width (mupdf_context (), pix);
  int    h      = fz_pixmap_height (mupdf_context (), pix);
  fz_keep_pixmap (mupdf_context (), pix);
  // we keep the samples since QImage will drop it later
  QImage im=
      QImage (samples, w, h, 4 * w, QImage::Format_RGBA8888_Premultiplied,
              mupdf_pixmap_cleanup_handler, pix);
  return im;
}

QTMPixmapOrImage
get_QTMPixmapOrImage_from_pixmap (fz_pixmap* pix) {
  QImage qim= get_QImage_from_pixmap (pix);
  if (headless_mode) {
    return QTMPixmapOrImage (qim);
  }
  else {
    return QTMPixmapOrImage (QPixmap::fromImage (qim));
  }
}

fz_buffer*
mupdf_read_from_url (url u) {
  fz_context* ctx= mupdf_context ();
  fz_buffer*  buf= NULL;
  fz_try (ctx) {
    if (is_ramdisc (u)) {
      string image_data= as_string (u[1][2]);
      buf              = fz_new_buffer_from_copied_data (
          ctx, reinterpret_cast<const unsigned char*> (image_data.begin ()),
          N (image_data));
    }
    else {
      c_string path (concretize (u));
      buf= fz_read_file (ctx, path);
    }
  }
  fz_catch (ctx) fz_report_error (ctx);
  return buf;
}
