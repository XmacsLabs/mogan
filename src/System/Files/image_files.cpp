
/******************************************************************************
 * MODULE     : image_files.cpp
 * DESCRIPTION: image file handling
 * COPYRIGHT  : (C) 1999-2016  Joris van der Hoeven & the TeXmacs team
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 *******************************************************************************
 *
 * This module centralizes functions that pick the best way to perfom
 * common operations on external images depending on their type and the
 * availability of various other modules (Qt, gs, pdf renderer ...) :
 *
 * -determining original image size (and caching it)
 * -various conversions using (preferentially) internal methods or
 *    calling external tools (including converters defined in the
 *    scheme code, and in last resort ImageMagick )
 *
 * Other modules (let say gs or pdf-renderer) should use only functions
 * implemented here (implement new ones if needed) and refrain from
 * calling directly other tools or converters (for instance qt,
 * scheme converters or imagemagick) because this leads to fragmentary
 * special cases and code duplication that are hard to maintain and debug
 *
 ******************************************************************************/

#include "image_files.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "hashmap.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tm_url.hpp"
#include "web_files.hpp"

#include "Xml/xml.hpp"
#include "scheme.hpp"

#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#endif

#ifdef USE_PLUGIN_GS
#include "Ghostscript/gs_utilities.hpp"
#endif

#ifdef USE_PLUGIN_PDF
#include "Pdf/pdf_hummus_renderer.hpp"
#endif

typedef struct {
  int w;
  int h;
  int xmin;
  int ymin;
} imgbox;
static hashmap<tree, imgbox> img_box;

// cache for storing image sizes
// (for ps/eps we also store the image offset so that we have the full bbox
// info)

/******************************************************************************
 * Loading xpm pixmaps
 ******************************************************************************/

tree
xpm_load (url u) {
  string s;
  load_string ("$TEXMACS_PIXMAP_PATH" * u, s, false);
  if (s == "") load_string ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm", s, true);

  int  i, j;
  tree t (URL_TUPLE);
  for (i= 0; i < N (s); i++)
    if (s[i] == '\x22') {
      i++;
      j= i;
      while ((i < N (s)) && (s[i] != '\x22'))
        i++;
      t << s (j, i);
    }
  if (N (t) == 0) return xpm_load ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm");
  return t;
}

void
xpm_size (url u, int& w, int& h) {
  static hashmap<string, string> xpm_size_table ("");
  string                         file_name= as_string (u);
  if (!xpm_size_table->contains (file_name)) {
    tree t                    = xpm_load (u);
    xpm_size_table (file_name)= t[0]->label;
  }

  int    i= 0;
  bool   ok;
  string s= xpm_size_table[file_name];
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  if (!ok) {
    failed_error << "File name= " << file_name << "\n";
    TM_FAILED ("invalid xpm");
  }
}

array<string>
xpm_colors (tree t) {
  array<string> res (0);
  string        s= t[0]->label;
  int           ok, i= 0, j, k, w, h, c, b;
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  ASSERT (ok && N (t) > c && c > 0, "invalid xpm tree");

  for (k= 0; k < c; k++) {
    string s  = as_string (t[k + 1]);
    string def= "none";
    if (N (s) < b) i= N (s);
    else i= b;

    skip_spaces (s, i);
    if ((i < N (s)) && (s[i] == 's')) {
      i++;
      skip_spaces (s, i);
      while ((i < N (s)) && (s[i] != ' ') && (s[i] != '\t'))
        i++;
      skip_spaces (s, i);
    }
    if ((i < N (s)) && (s[i] == 'c')) {
      i++;
      skip_spaces (s, i);
      j= i;
      while ((i < N (s)) && (s[i] != ' ') && (s[i] != '\t'))
        i++;
      def= locase_all (s (j, i));
    }
    res << def;
  }
  return res;
}

array<SI>
xpm_hotspot (tree t) {
  array<SI> res (0);
  string    s= t[0]->label;
  int       ok, i= 0, w, h, c, b, x, y;
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  ASSERT (ok && N (t) > c && c > 0, "invalid xpm tree");

  skip_spaces (s, i);
  ok= read_int (s, i, x) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, y) && ok;
  if (ok) {
    res << x;
    res << y;
  }
  return res;
}

/******************************************************************************
 * Loading postscript files (possibly triggering conversion to postscript)
 ******************************************************************************/

string
ps_load (url image, bool conv) {
  if (DEBUG_CONVERT) debug_convert << "ps_load " << image << LF;

  url name= sys_concretize (image);
  if (is_none (name)) name= "$TEXMACS_PATH/misc/pixmaps/unknown.png";

  if (is_ramdisc (name)) name= get_from_ramdisc (name);

  string s= "", suf= suffix (image);
  if (suf == "ps" || suf == "eps") {
    load_string (name, s, false);
  }
  else {
    if (conv)
      s= image_to_psdoc (image); // call converters, then load resulting ps
  }

  if (is_empty (s))
    load_string ("$TEXMACS_PATH/misc/pixmaps/unknown.png", s, true);
  return s;
}

// returns true when size could be determined either
// from cache or reading it, or using default size
bool
ps_bounding_box (url image, int& x1, int& y1, int& x2, int& y2,
                 bool set_default) {
  tree lookup= as_tree (image);
  if (img_box->contains (lookup)) {
    imgbox box= img_box[lookup];
    x1        = box.xmin;
    y1        = box.ymin;
    x2        = box.xmin + box.w;
    y2        = box.ymin + box.h;
    if (DEBUG_CONVERT)
      debug_convert << "bbox in cache for " << image << LF << " : " << x1
                    << " , " << y1 << " , " << x2 << " , " << y2 << LF;
    return true;
  }
  else {
    string s= ps_load (image, false);
    if (!ps_read_bbox (s, x1, y1, x2, y2)) {
      if (set_default) {
        x1= y1= 0;
        x2    = 596;
        y2    = 842;
      }
      else {
        if (DEBUG_CONVERT)
          debug_convert << "cannot read bbox for " << image << LF;
        return false;
      }
    }
    set_imgbox_cache (lookup, x2 - x1, y2 - y1, x1, y1);
    return true;
  }
}

bool
ps_read_bbox (string buf, int& x1, int& y1, int& x2, int& y2) {
  int pos= search_forwards ("\n%%BoundingBox:", buf);
  if (pos < 0) pos= search_forwards ("%%BoundingBox:", buf);
  if (pos < 0) return false;
  if (buf[pos] == '\n') pos++;
  bool   ok= read (buf, pos, "%%BoundingBox:");
  double X1, Y1, X2, Y2;
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, X1) && ok;
  x1= (int) floor (X1);
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, Y1) && ok;
  y1= (int) floor (Y1);
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, X2) && ok;
  x2= (int) ceil (X2);
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, Y2) && ok;
  y2= (int) ceil (Y2);
  if (DEBUG_CONVERT)
    debug_convert << "bbox found : " << ok << " : " << x1 << " , " << y1
                  << " , " << x2 << " , " << y2 << LF;
  if (!ok) return false;
  return true;
}

void
set_imgbox_cache (tree t, int w, int h, int xmin, int ymin) {
  img_box (t)= imgbox{w, h, xmin, ymin};
}

void
clear_imgbox_cache (tree t) {
  img_box->reset (t);
}

void
clearall_imgbox_cache () {
  img_box= hashmap<tree, imgbox> ();
}
/******************************************************************************
 * Getting the original size of an image, using internal plug-ins if possible
 ******************************************************************************/
void image_size_sub (url image, int& w, int& h);

void
image_size (url image, int& w, int& h) {
  /* Get original image size (in pt units) using cached result if possible,
   * otherwise actually fetch image size and cache it.
   * Caching is super important because the typesetter calls image_size */
  tree lookup= as_tree (image);
  if (img_box->contains (lookup)) {
    imgbox box= img_box[lookup];
    w         = box.w;
    h         = box.h;
    if (DEBUG_CONVERT)
      debug_convert << "image_size in cache for " << image << LF << w << " x "
                    << h << LF;
  }
  else {
    w= h= 0;
    image_size_sub (image, w, h);
    if ((w <= 0) || (h <= 0)) {
      if (suffix (image) == "svg") {
        debug_convert << "bad image size for '" << image << "'"
                      << " setting 35x35 " << LF;
      }
      else {
        convert_error << "bad image size for '" << image << "'"
                      << " setting 35x35 " << LF;
      }
      w= 35;
      h= 35;
    }
    // for ps and eps images the imgbox should have been cached
    // during the image_size_sub call
    if (img_box->contains (lookup)) return;
    set_imgbox_cache (lookup, w, h);
  }
}

void
image_size_sub (url p_image, int& w,
                int& h) { // returns w,h in units of pt (1/72 inch)
  if (DEBUG_CONVERT)
    debug_convert << "image_size not cached for :" << p_image << LF;
  url    image= concretize (p_image);
  string suf  = suffix (image);
  if (suf == "pdf") {
    pdf_image_size (image, w, h);
    return;
  }
  if (suf == "svg") {
    svg_image_size (image, w, h);
    return;
  }
  if (suf == "eps" || suf == "ps") {
    int x1, y1, x2, y2;
    if (ps_bounding_box (image, x1, y1, x2, y2, false)) {
      w= x2 - x1;
      h= y2 - y1;
      if (DEBUG_CONVERT)
        debug_convert << "size from ps_bounding_box : " << w << " x " << h
                      << "\n";
      return;
    }
  }
#ifdef QTTEXMACS
  if (qt_supports (image)) { // native support by Qt : most bitmaps & svg
    qt_image_size (image, w, h);
    return;
  }
#endif
#ifdef USE_PLUGIN_GS
  if (gs_supports (image)) { // this is for handling ps without explicit bbox
    gs_image_size (image, w, h);
    return;
  }
#endif
  if (imagemagick_image_size (image, w, h)) {
    if (DEBUG_CONVERT)
      debug_convert << "image_size imagemagick : " << w << " x " << h << "\n";
    return;
  }

  convert_error
      << "could not determine size of '" << image << "'\n"
      << "you may consider :\n"
      << " - checking the file is valid,\n"
      << " - converting to a more standard format,\n"
      << " - defining an appropriate converter (see documentation).\n";
}

void
pdf_image_size (url image, int& w, int& h) {
// we have two ways of finding pdf sizes
// centralize here to ensure consistent determination;
// prefer internal method (avoid calling gs)
#ifdef USE_PLUGIN_PDF
  hummus_pdf_image_size (image, w, h);
  return;
#endif
#ifdef USE_PLUGIN_GS
  gs_PDFimage_size (image, w, h);
  return;
#endif
  // if above methods are absent :-(, fallback to
  imagemagick_image_size (image, w, h, true);
}

void
svg_image_size (url image, int& w, int& h) {
  string content;
  bool   err= tm_load_string (image, content, false);
  if (!err) {
    tree   t         = parse_xml (content);
    tree   result    = find_first_element_by_name (t, "svg");
    string width     = get_attr_from_element (result, "width", "");
    string height    = get_attr_from_element (result, "height", "");
    int    try_width = parse_xml_length (width);
    int    try_height= parse_xml_length (height);
    if (try_width > 0) w= try_width;
    if (try_height > 0) h= try_height;
  }
}

/******************************************************************************
 * Converting any image format to the only three we need for
 * displaying and printing : png, eps, pdf
 ******************************************************************************/

bool
wrap_qt_supports (url image) {
#ifdef QTTEXMACS
  return qt_supports (image);
#else
  (void) image;
  return false;
#endif
}

string
image_to_psdoc (url image) {
  if (DEBUG_CONVERT) debug_convert << "image_to_psdoc " << image << LF;

  url psfile= url_temp ("eps");
  if (!call_scm_converter (image, psfile)) {
    convert_error << "Failed to convert " << suffix (image) << " to eps" << LF;
  }
  string psdoc;
  load_string (psfile, psdoc, false);
  remove (psfile);
  return psdoc;
}

// mostly the same code as image_to_eps
void
image_to_pdf (url image, url pdf, int w_pt, int h_pt, int dpi) {
  if (DEBUG_CONVERT) debug_convert << "image_to_pdf ... ";
  string s= suffix (image);
  // First try to preserve "vectorialness"
  if ((s == "svg" || s == "eps") && call_scm_converter (image, pdf)) return;
    // converters below will yield only raster images.
#ifdef QTTEXMACS
  if (qt_supports (image)) {
    if (DEBUG_CONVERT) debug_convert << " using qt " << LF;
    qt_image_to_pdf (image, pdf, w_pt, h_pt, dpi);
    return;
  }
#endif
  if ((s != "svg") && call_scm_converter (image, pdf)) return;
}

void
image_to_png (url p_image, url png, int w, int h) { // IN PIXEL UNITS!
  url image= concretize (p_image);

#ifdef QTTEXMACS
  if (qt_supports (image)) {
    if (DEBUG_CONVERT) debug_convert << "image_to_png using qt " << LF;
    qt_convert_image (image, png, w, h);
    return;
  }
#endif
  if (call_scm_converter (image, png, w, h)) return;
  if (!exists (png)) {
    convert_error << p_image << " could not be converted to png" << LF;
    copy ("$TEXMACS_PATH/misc/pixmaps/unknown.png", png);
  }
}

bool
call_scm_converter (url image, url dest, int w, int h) {
  if (DEBUG_CONVERT) debug_convert << "Image conversion using scm" << LF;
  if (as_bool (call ("file-converter-exists?", "x." * suffix (image),
                     "x." * suffix (dest)))) {
    call ("file-convert", object (image), object (dest),
          cons (symbol_object ("width"), object (w)),
          cons (symbol_object ("height"), object (h)));
    bool success= exists (dest);
    if (success && DEBUG_CONVERT)
      debug_convert << "scm file-convert " << concretize (image) << " -> "
                    << concretize (dest) << LF;
    return success;
  }
  return false;
}

/******************************************************************************
 * Imagemagick stuff
 * last resort solution -- should rarely be useful.
 ******************************************************************************/

url
find_binary_convert () {
  eval ("(use-modules (binary convert))");
  return as_url (eval ("(find-binary-convert)"));
}

static url
find_binary_identify () {
  eval ("(use-modules (binary identify))");
  return as_url (eval ("(find-binary-identify)"));
}

bool
imagemagick_image_size (url image, int& w, int& h, bool pt_units) {
  url binary_identify= find_binary_identify ();
  if (is_none (binary_identify)) return false;
  else {
    string cmd= sys_concretize (binary_identify);
    cmd << " -ping -format \"%w %h %x\\n%y\"";
    string sz= eval_system (cmd, image);
    int    w_px, h_px, ok= true, pos= 0;
    string unit;
    ok= read_int (sz, pos, w_px);
    skip_spaces (sz, pos);
    ok= ok && read_int (sz, pos, h_px);
    if (!ok) return false;
    else if (!pt_units) { // return numbers of pixels
      w= w_px;
      h= h_px;
      return true;
    }
    else {
      double densityx= 72, densityy= 72, ptperpix= 1;
      skip_spaces (sz, pos);
      ok= ok && read_double (sz, pos, densityx);
      if (densityx == 0) return false;
      else {
        ok= ok && read_line (sz, pos, unit);
        // according to the IM doc, units should be in
        // [PixelsPerCentimeter,PixelsPerInch,Undefined] When "Undefined" IM
        // gives the nonsensical default value of 72 "dots per Undefined" svg is
        // SCALABLE and hence logically gives "Undefined"; in that case we
        // assume 90 dpi so that the physical image size matches that of svg
        // created with inkscape
        if (unit == "PixelsPerCentimeter") ptperpix= 72 / (2.54 * densityx);
        else if (unit == "PixelsPerInch") ptperpix= 72 / densityx;
        else if (unit == "Undefined") ptperpix= 90 / densityx;
        w = (int) w_px * ptperpix;
        h = (int) h_px * ptperpix;
        ok= ok && read_double (sz, pos, densityy);
        if ((densityy != 0) && (densityy != densityx))
          h= (int) (h * densityx / densityy);
        return ok;
      }
    }
  }
}

/******************************************************************************
 * Applying a TeXmacs effect to a picture
 ******************************************************************************/

#ifdef QTTEXMACS
bool qt_native_image_size (url image, int& w, int& h);
void qt_apply_effect (tree eff, array<url> src, url dest, int w, int h);
#endif

void
native_image_size (url image, int& w, int& h) {
#ifdef QTTEXMACS
  if (qt_native_image_size (image, w, h)) return;
#endif
  // Scale to 300 dpi
  image_size (image, w, h);
  double scale= 30000.0 / (2834 * 2.54);
  w           = (int) round (scale * w);
  h           = (int) round (scale * h);
}

void
apply_effect (tree eff, array<url> src, url dest, int w, int h) {
#ifdef QTTEXMACS
  qt_apply_effect (eff, src, dest, w, h);
#endif
}
