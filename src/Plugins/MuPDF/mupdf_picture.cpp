
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

#include "effect.hpp"
#include "file.hpp"
#include "image_files.hpp"
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

fz_image*
mupdf_load_image (url u) {
  fz_image* im = NULL;
  string    suf= suffix (u);
  c_string  path (concretize (u));
  // Set the zoom to 200% in MuPDF render(144 DPI) when convert vector graphics
  // into pixmap. Test ok in MacOS, with 2x HiDPI enabled(4K screen)
  fz_matrix ctm= fz_scale (2.0, 2.0);
  if (suf == "svg") {
    fz_buffer*  buffer = fz_read_file (mupdf_context (), path);
    fz_xml_doc* xml_doc= NULL;
    fz_xml*     xml    = NULL;
    fz_image*   tmp_im = NULL;
    fz_pixmap*  tmp_pix= NULL;
    fz_try (mupdf_context ()) {
      xml_doc= fz_parse_xml (mupdf_context (), buffer, 0);
      xml    = fz_xml_find (xml_doc, "svg");
      tmp_im = fz_new_image_from_svg_xml (mupdf_context (), xml_doc, xml, NULL,
                                          NULL);
      tmp_pix= fz_get_pixmap_from_image (mupdf_context (), tmp_im, NULL, &ctm,
                                         NULL, NULL);
      im     = fz_new_image_from_pixmap (mupdf_context (), tmp_pix, NULL);
    }
    fz_catch (mupdf_context ()) { fz_report_error (mupdf_context ()); }
    fz_drop_xml (mupdf_context (), xml);
    fz_drop_xml (mupdf_context (), xml_doc);
    fz_drop_buffer (mupdf_context (), buffer);
    fz_drop_image (mupdf_context (), tmp_im);
    fz_drop_pixmap (mupdf_context (), tmp_pix);
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
    im            = fz_new_image_from_pixmap (mupdf_context (), pix, NULL);
  }
  else if (suf == "pdf") {
    pdf_document* doc = NULL;
    pdf_page*     page= NULL;
    fz_pixmap*    pix = NULL;
    fz_try (mupdf_context ()) {
      doc           = pdf_open_document (mupdf_context (), path);
      int page_count= pdf_count_pages (mupdf_context (), doc);
      if (page_count > 0) {
        page= pdf_load_page (mupdf_context (), doc, 0);
        pix = pdf_new_pixmap_from_page_with_usage (
            mupdf_context (), page, ctm, fz_device_rgb (mupdf_context ()), 0,
            "View", FZ_TRIM_BOX);
        im= fz_new_image_from_pixmap (mupdf_context (), pix, NULL);
      }
    }
    fz_catch (mupdf_context ()) { fz_report_error (mupdf_context ()); }
    fz_drop_pixmap (mupdf_context (), pix);
    pdf_drop_document (mupdf_context (), doc);
  }
  else {
    // Othre format.
    fz_try (mupdf_context ()) {
      im= fz_new_image_from_file (mupdf_context (), path);
    }
    fz_catch (mupdf_context ()) { fz_report_error (mupdf_context ()); }
  }
  return im;
}

fz_pixmap*
mupdf_load_pixmap (url u, int w, int h, tree eff, SI pixel) {
  fz_image* im= mupdf_load_image (u);

  if (im == NULL) {
    // attempt to convert to png
    url temp= url_temp (".png");
    image_to_png (u, temp, w, h);
    c_string path (as_string (temp));
    im= fz_new_image_from_file (mupdf_context (), path);
    remove (temp);
  }

  // Error Handling
  if (im == NULL) {
    cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
    return NULL;
  }

  fz_pixmap* pix=
      fz_get_pixmap_from_image (mupdf_context (), im, NULL, NULL, NULL, NULL);
  // Scaling
  if (im->w != w || im->h != h) {
    fz_pixmap* scaled=
        fz_scale_pixmap (mupdf_context (), pix, 0, 0, w, h, NULL);
    fz_drop_pixmap (mupdf_context (), pix);
    pix= scaled;
  }
  fz_drop_image (mupdf_context (), im); // we do not need it anymore

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
    fz_drop_pixmap (mupdf_context (), pix);
    pix= tpix;
  }
  return pix;
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
