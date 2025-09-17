
/******************************************************************************
 * MODULE     : mupdf_picture.cpp
 * DESCRIPTION: Picture objects for MuPDF
 * COPYRIGHT  : (C) 2022 Massimiliano Gubinelli, Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "mupdf_picture.hpp"

#include "editor.hpp"
#include "effect.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "new_view.hpp"
#include "tm_url.hpp"

/******************************************************************************
 * Abstract mupdf pictures
 ******************************************************************************/

mupdf_picture_rep::mupdf_picture_rep (fz_pixmap* _pix, int ox2, int oy2)
    : pix (_pix), im (NULL), w (fz_pixmap_width (mupdf_context (), pix)),
      h (fz_pixmap_height (mupdf_context (), pix)), ox (ox2), oy (oy2) {
  fz_keep_pixmap (mupdf_context (), pix);
}

mupdf_picture_rep::~mupdf_picture_rep () {
  fz_drop_pixmap (mupdf_context (), pix);
  fz_drop_image (mupdf_context (), im);
}

picture_kind
mupdf_picture_rep::get_type () {
  return picture_native;
}
void*
mupdf_picture_rep::get_handle () {
  return (void*) this;
}

int
mupdf_picture_rep::get_width () {
  return w;
}
int
mupdf_picture_rep::get_height () {
  return h;
}
int
mupdf_picture_rep::get_origin_x () {
  return ox;
}
int
mupdf_picture_rep::get_origin_y () {
  return oy;
}
void
mupdf_picture_rep::set_origin (int ox2, int oy2) {
  ox= ox2;
  oy= oy2;
}

color
mupdf_picture_rep::internal_get_pixel (int x, int y) {
  unsigned char* samples= fz_pixmap_samples (mupdf_context (), pix);
  return rgbap_to_argb (((color*) samples)[x + w * (h - 1 - y)]);
}

void
mupdf_picture_rep::internal_set_pixel (int x, int y, color c) {
  unsigned char* samples= fz_pixmap_samples (mupdf_context (), pix);
  ((color*) samples)[x + w * (h - 1 - y)]= argb_to_rgbap (c);
}

picture
mupdf_picture (fz_pixmap* _pix, int ox, int oy) {
  return (picture) tm_new<mupdf_picture_rep, fz_pixmap*, int, int> (_pix, ox,
                                                                    oy);
}

picture
as_mupdf_picture (picture pic) {
  if (pic->get_type () == picture_native) return pic;
  fz_pixmap* pix=
      fz_new_pixmap (mupdf_context (), fz_device_rgb (mupdf_context ()),
                     pic->get_width (), pic->get_height (), NULL, 1);
  picture ret= mupdf_picture (pix, pic->get_origin_x (), pic->get_origin_y ());
  fz_drop_pixmap (mupdf_context (), pix);
  ret->copy_from (pic); // FIXME: is this inefficient???
  return ret;
}

#ifdef USE_MUPDF_RENDERER
picture
as_native_picture (picture pict) {
  return as_mupdf_picture (pict);
}

picture
native_picture (int w, int h, int ox, int oy) {
  fz_pixmap* pix= fz_new_pixmap (
      mupdf_context (), fz_device_rgb (mupdf_context ()), w, h, NULL, 1);
  fz_clear_pixmap (mupdf_context (), pix);
  picture p= mupdf_picture (pix, ox, oy);
  fz_drop_pixmap (mupdf_context (), pix);
  return p;
}
#endif

/******************************************************************************
 * Rendering on images
 ******************************************************************************/

class mupdf_image_renderer_rep : public mupdf_renderer_rep {
public:
  picture pict;

public:
  mupdf_image_renderer_rep (picture pict, double zoom);
  void* get_data_handle ();
};

mupdf_image_renderer_rep::mupdf_image_renderer_rep (picture p, double zoom)
    : mupdf_renderer_rep (), pict (p) {
  zoomf  = zoom;
  shrinkf= (int) tm_round (std_shrinkf / zoomf);
  pixel  = (SI) tm_round ((std_shrinkf * PIXEL) / zoomf);
  thicken= (shrinkf >> 1) * PIXEL;

  int pw = p->get_width ();
  int ph = p->get_height ();
  int pox= p->get_origin_x ();
  int poy= p->get_origin_y ();

  ox= pox * pixel;
  oy= poy * pixel;
  /*
  cx1= 0;
  cy1= 0;
  cx2= pw * pixel;
  cy2= ph * pixel;
  */
  cx1= 0;
  cy1= -ph * pixel;
  cx2= pw * pixel;
  cy2= 0;

  mupdf_picture_rep* handle= (mupdf_picture_rep*) pict->get_handle ();
  begin (handle->pix);
}

void*
mupdf_image_renderer_rep::get_data_handle () {
  return (void*) this;
}

#ifdef USE_MUPDF_RENDERER
renderer
picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<mupdf_image_renderer_rep> (p, zoomf);
}
#endif

/******************************************************************************
 * Loading pictures
 ******************************************************************************/

fz_pixmap*
mupdf_load_pdf_image (fz_buffer* pdf_data, fz_matrix scale) {
  fz_context*   ctx   = mupdf_context ();
  pdf_document* doc   = NULL;
  pdf_page*     page  = NULL;
  fz_pixmap*    pix   = NULL;
  fz_stream*    stream= NULL;
  fz_try (ctx) {
    stream        = fz_open_buffer (ctx, pdf_data);
    doc           = pdf_open_document_with_stream (ctx, stream);
    int page_count= pdf_count_pages (ctx, doc);
    if (page_count > 0) {
      page= pdf_load_page (ctx, doc, 0);
      pix = pdf_new_pixmap_from_page_with_usage (
          ctx, page, scale, fz_device_rgb (ctx), 0, "View", FZ_TRIM_BOX);
    }
  }
  fz_catch (ctx) { fz_report_error (ctx); }
  pdf_drop_page (ctx, page);
  pdf_drop_document (ctx, doc);
  fz_drop_stream (ctx, stream);
  return pix;
}

fz_image*
mupdf_load_image (url u) {
  fz_context* ctx   = mupdf_context ();
  fz_image*   im    = NULL;
  fz_buffer*  buffer= mupdf_read_from_url (u);
  if (buffer == NULL) {
    return NULL;
  }
  string suf= suffix (u);
  // Set the zoom to 200% in MuPDF render(144 DPI) when convert vector graphics
  // into pixmap. Test ok in MacOS, with 2x HiDPI enabled(4K screen)
  fz_matrix ctm= fz_scale (2.0, 2.0);
  if (suf == "svg") {
    fz_xml_doc* xml_doc= NULL;
    fz_xml*     xml    = NULL;
    fz_image*   tmp_im = NULL;
    fz_pixmap*  tmp_pix= NULL;
    fz_try (ctx) {
      xml_doc= fz_parse_xml (ctx, buffer, 0);
      xml    = fz_xml_find (xml_doc, "svg");
      tmp_im = fz_new_image_from_svg_xml (ctx, xml_doc, xml, NULL, NULL);
      tmp_pix= fz_get_pixmap_from_image (ctx, tmp_im, NULL, &ctm, NULL, NULL);
      im     = fz_new_image_from_pixmap (ctx, tmp_pix, NULL);
    }
    fz_catch (ctx) { fz_report_error (ctx); }
    fz_drop_xml (ctx, xml);
    fz_drop_xml (ctx, xml_doc);
    fz_drop_image (ctx, tmp_im);
    fz_drop_pixmap (ctx, tmp_pix);
  }
  else if (suf == "xpm") {
    // try to load higher definition png equivalent if available
    url png_equiv= glue (unglue (u, 4), "_x4.png");
    if (exists (png_equiv)) {
      return mupdf_load_image (png_equiv);
    }
    png_equiv= glue (unglue (u, 4), "_x2.png");
    if (exists (png_equiv)) {
      return mupdf_load_image (png_equiv);
    }
    png_equiv= glue (unglue (u, 4), ".png");
    if (exists (png_equiv)) {
      return mupdf_load_image (png_equiv);
    }
    // ok, try to load the xpm finally
    picture    xp = as_mupdf_picture (load_xpm (u));
    fz_pixmap* pix= ((mupdf_picture_rep*) xp->get_handle ())->pix;
    im            = fz_new_image_from_pixmap (ctx, pix, NULL);
  }
  else if (suf == "pdf") {
    fz_pixmap* pix= mupdf_load_pdf_image (buffer, ctm);
    if (pix != NULL) {
      im= fz_new_image_from_pixmap (mupdf_context (), pix, NULL);
      fz_drop_pixmap (mupdf_context (), pix);
    }
  }
  else {
    // Othre format.
    fz_try (ctx) { im= fz_new_image_from_buffer (ctx, buffer); }
    fz_catch (ctx) { fz_report_error (ctx); }
  }
  fz_drop_buffer (ctx, buffer);
  if (im == NULL) {
    // attempt to convert to png
    url temp= url_temp (".png");
    image_to_png (u, temp, 0, 0);
    if (exists (temp)) {
      c_string path (as_string (temp));
      fz_try (ctx) { im= fz_new_image_from_file (ctx, path); }
      fz_catch (ctx) { fz_report_error (ctx); }
      remove (temp);
    }
  }
  return im;
}

fz_pixmap*
mupdf_load_pixmap (url u, int w, int h, tree eff, SI pixel) {
  fz_context* ctx= mupdf_context ();
  fz_image*   im = mupdf_load_image (u);

  // Error Handling
  if (im == NULL) {
    return NULL;
  }

  fz_pixmap* pix= fz_get_pixmap_from_image (ctx, im, NULL, NULL, NULL, NULL);
  // Scaling
  if (im->w != w || im->h != h) {
    fz_pixmap* scaled= fz_scale_pixmap (ctx, pix, 0, 0, w, h, NULL);
    fz_drop_pixmap (ctx, pix);
    pix= scaled;
  }
  fz_drop_image (ctx, im); // we do not need it anymore

  // Build effect
  if (eff != "") {
    effect         e  = build_effect (eff);
    picture        src= mupdf_picture (pix, 0, 0);
    array<picture> a;
    a << src;
    picture            pic = e->apply (a, pixel);
    picture            dest= as_mupdf_picture (pic);
    mupdf_picture_rep* rep = (mupdf_picture_rep*) dest->get_handle ();
    fz_pixmap*         tpix= rep->pix;
    fz_drop_pixmap (ctx, pix);
    pix= tpix;
  }
  return pix;
}

/******************************************************************************
 * Image size
 ******************************************************************************/

bool
mupdf_supports (string extension) {
  if (fz_lookup_image_type (c_string (extension)) != FZ_IMAGE_UNKNOWN) {
    return true;
  }
  if (extension == "pdf" || extension == "jpg" || extension == "tif" ||
      extension == "svg") {
    return true;
  }
  return false;
}

bool
mupdf_normal_image_size (url image, int& w, int& h) { // w, h in points
  if (DEBUG_CONVERT) debug_convert << "mupdf_normal_image_size :" << LF;
  fz_context* ctx= mupdf_context ();
  fz_image*   im = NULL;
  fz_buffer*  buf= mupdf_read_from_url (image);
  if (buf != NULL) {
    fz_try (ctx) { im= fz_new_image_from_buffer (ctx, buf); }
    fz_catch (ctx) fz_report_error (ctx);
    fz_drop_buffer (ctx, buf);
  }
  if (im == NULL) {
    convert_error << "Cannot read image file '" << image << "'"
                  << " in mupdf_normal_image_size" << LF;
    w= 35;
    h= 35;
    return false;
  }
  else {
    SI pt= get_current_editor ()->as_length ("1pt");
    SI px= get_current_editor ()->as_length ("1px");
    w    = (int) im->w * px * 1.0 / pt;
    h    = (int) im->h * px * 1.0 / pt;
    fz_drop_image (ctx, im);
    if (DEBUG_CONVERT)
      debug_convert << "mupdf_normal_image_size (pt): " << w << " x " << h
                    << LF;
    return true;
  }
}

bool
mupdf_pdf_image_size (url image, int& w, int& h) {
  if (DEBUG_CONVERT) debug_convert << "mupdf_pdf_image_size :" << LF;
  fz_context* ctx= mupdf_context ();
  fz_buffer*  buf= mupdf_read_from_url (image);
  fz_pixmap*  im = NULL;
  if (buf != NULL) {
    im= mupdf_load_pdf_image (buf, fz_scale (1.0, 1.0));
    fz_drop_buffer (ctx, buf);
  }
  if (im == NULL) {
    convert_error << "Cannot read image file '" << image << "'"
                  << " in mupdf_pdf_image_size" << LF;
    w= 35;
    h= 35;
    return false;
  }
  else {
    SI pt= get_current_editor ()->as_length ("1pt");
    SI px= get_current_editor ()->as_length ("1px");
    w    = (int) im->w * px * 1.0 / pt;
    h    = (int) im->h * px * 1.0 / pt;
    fz_drop_pixmap (ctx, im);
    if (DEBUG_CONVERT)
      debug_convert << "mupdf_pdf_image_size (pt): " << w << " x " << h << LF;
  }
  return true;
}

string
mupdf_load_and_parse_image (const char* path, int& w, int& h,
                            string extension) {
  fz_context*    ctx= mupdf_context ();
  fz_buffer*     buf= NULL;
  unsigned char* data;
  fz_try (ctx) { buf= fz_read_file (ctx, path); }
  fz_catch (ctx) {
    fz_report_error (ctx);
    buf= fz_new_buffer (ctx, 0);
  }
  int    len= fz_buffer_storage (ctx, buf, &data);
  string res (reinterpret_cast<const char*> (data), len);
  fz_drop_buffer (ctx, buf);
  url image= url_ramdisc (res) * extension;
  if (extension == "pdf") {
    mupdf_pdf_image_size (image, w, h);
  }
  else if (extension == "eps" || extension == "ps" || extension == "svg") {
    w= 0;
    h= 0;
  }
  else {
    mupdf_normal_image_size (image, w, h);
  }

  return res;
}

#ifdef USE_MUPDF_RENDERER
picture
load_picture (url u, int w, int h, tree eff, int pixel) {
  fz_pixmap* pix= mupdf_load_pixmap (u, w, h, eff, pixel);
  if (pix == NULL) return error_picture (w, h);
  picture p= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context (), pix);
  return p;
}

void
save_picture (url dest, picture p) {
  if (suffix (dest) != "png") {
    cout << "TeXmacs] warning: cannot save " << concretize (dest)
         << ", format not supported\n";
    return;
  }
  picture            q   = as_mupdf_picture (p);
  mupdf_picture_rep* pict= (mupdf_picture_rep*) q->get_handle ();
  if (exists (dest)) remove (dest);
  c_string   path= concretize (dest);
  fz_output* out = fz_new_output_with_path (mupdf_context (), path, 0);
  fz_write_pixmap_as_png (mupdf_context (), out, pict->pix);
  fz_close_output (mupdf_context (), out);
  fz_drop_output (mupdf_context (), out);
}
#endif
