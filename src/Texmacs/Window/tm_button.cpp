
/******************************************************************************
 * MODULE     : tm_button.cpp
 * DESCRIPTION: Text widgets for output only
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Boxes/construct.hpp"
#include "boxes.hpp"
#include "font.hpp"
#include "message.hpp"
#include "tm_buffer.hpp"
#include "tm_frame.hpp"
#ifdef AQUATEXMACS
#include "Cocoa/aqua_simple_widget.h"
#else
#ifdef QTTEXMACS
#include "Qt/qt_simple_widget.hpp"
#else
#include "Widkit/simple_wk_widget.hpp"
#endif
#endif

/******************************************************************************
 * Getting extents of a typesetted tree
 * Application: get window size for widget tree
 ******************************************************************************/

#include "Format/format.hpp"
#include "convert.hpp"
#include "formatter.hpp"
#include "gui.hpp"
#include "new_style.hpp"

#include <moebius/drd/drd_info.hpp>
#include <moebius/drd/drd_std.hpp>

using moebius::drd::std_drd;
using namespace moebius;

void use_modules (tree t);

void
initialize_environment (edit_env& env, tree doc, drd_info& drd) {
  env->write_default_env ();
  bool                  ok;
  tree                  t, style= extract (doc, "style");
  hashmap<string, tree> H;
  style_get_cache (style, H, t, ok);
  if (ok) {
    env->patch_env (H);
    ok= drd->set_locals (t);
    drd->set_environment (H);
  }
  if (!ok) {
    if (!is_tuple (style)) TM_FAILED ("tuple expected as style");
    H  = get_style_env (style);
    drd= get_style_drd (style);
    style_set_cache (style, H, drd->get_locals ());
    env->patch_env (H);
    drd->set_environment (H);
  }
  use_modules (env->read (THE_MODULES));
  tree init= extract (doc, "initial");
  for (int i= 0; i < N (init); i++)
    if (is_func (init[i], ASSOCIATE, 2) && is_atomic (init[i][0]))
      env->write (init[i][0]->label, init[i][1]);
  // env->write (PAGE_WIDTH_MARGIN, "true");
  // env->write (PAR_WIDTH, "400px");
  // env->write (DPI, "720");
  // env->write (ZOOM_FACTOR, "1.2");
  // env->write (PAGE_TYPE, "a5");
  if (retina_zoom == 2) {
    double mag= 2.0 * env->get_double (MAGNIFICATION);
    env->write (MAGNIFICATION, as_string (mag));
  }
  env->update ();
}

tree
tree_extents (tree doc) {
  drd_info              drd ("none", std_drd);
  hashmap<string, tree> h1 (UNINIT), h2 (UNINIT);
  hashmap<string, tree> h3 (UNINIT), h4 (UNINIT);
  hashmap<string, tree> h5 (UNINIT), h6 (UNINIT);
  edit_env              env (drd, "none", h1, h2, h3, h4, h5, h6);
  initialize_environment (env, doc, drd);
  tree   t = extract (doc, "body");
  lazy   lz= make_lazy (env, t, path ());
  format vf=
      make_query_vstream_width (array<line_item> (), array<line_item> ());
  format rf= lz->query (LAZY_BOX, vf);
  SI     w = ((format_vstream) rf)->width;
  box    b = (box) lz->produce (LAZY_BOX, make_format_width (w));
  SI     h = b->h ();
  w+= env->get_length (PAGE_SCREEN_LEFT);
  w+= env->get_length (PAGE_SCREEN_RIGHT);
  h+= env->get_length (PAGE_SCREEN_TOP);
  h+= env->get_length (PAGE_SCREEN_BOT);
  return tuple (as_tree ((w / (5 * PIXEL)) + 1),
                as_tree ((h / (5 * PIXEL)) + 1));
}

/******************************************************************************
 * Typesetted boxes as widgets
 ******************************************************************************/

class box_widget_rep : public simple_widget_rep {
  box                     b;
  color                   bg;
  bool                    transparent;
  double                  zoomf;
  double                  magf;
  SI                      dw, dh;
  SI                      last_x, last_y;
  bool                    lpressed;
  static constexpr double display_scale= 0.4;

public:
  box_widget_rep (box b, color bg, bool trans, double zoom, SI dw, SI dh);
  operator tree ();
  bool is_embedded_widget ();

  void handle_get_size_hint (SI& w, SI& h);
  void handle_repaint (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void handle_mouse (string kind, SI x, SI y, int m, time_t t,
                     array<double> data);
};

box_widget_rep::box_widget_rep (box b2, color bg2, bool trans2, double zoom,
                                SI dw2, SI dh2)
    : simple_widget_rep (), b (b2), bg (bg2), transparent (trans2),
      zoomf (zoom), magf (zoom / std_shrinkf), dw (dw2 + 2 * PIXEL),
      dh (dh2 + 2 * PIXEL), last_x (0), last_y (0), lpressed (false) {}

box_widget_rep::operator tree () { return tree (TUPLE, "box", (tree) b); }

bool
box_widget_rep::is_embedded_widget () {
  return true;
}

void
box_widget_rep::handle_get_size_hint (SI& w, SI& h) {
  SI X1= b->x1, Y1= b->y1;
  SI X2= b->x2, Y2= b->y2;
  w= ((SI) ceil ((X2 - X1) * magf * display_scale)) + 2 * dw;
  h= ((SI) ceil ((Y2 - Y1) * magf * display_scale)) + 2 * dh;
  abs_round (w, h);
}

void
box_widget_rep::handle_repaint (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  SI w, h;
  handle_get_size_hint (w, h);
  if (!transparent) {
    ren->set_background (bg);
    ren->set_pencil (bg);
    ren->fill (x1, y1, x2, y2);
  }
  ren->set_zoom_factor (zoomf * display_scale);
  rectangles l (rectangle (0, 0, w, h));
  SI         x= ((((SI) (w / magf / display_scale)) - b->w ()) >> 1) - b->x1;
  SI         y= ((((SI) (h / magf / display_scale)) - b->h ()) >> 1) - b->y1 -
        ((SI) (h / magf / display_scale));
  b->redraw (ren, path (), l, x, y);
  ren->reset_zoom_factor ();
}

void
box_widget_rep::handle_mouse (string kind, SI x, SI y, int m, time_t t,
                              array<double> data) {
  (void) m;
  (void) t;
  (void) data;
  // cout << "Mouse  : " << kind << ", "
  //      << (x/PIXEL) << ", " << (y/PIXEL) << "\n";
  // cout << "Extents: "
  //      << b->x1/PIXEL << ", " << b->y1/PIXEL << "; "
  //      << b->x2/PIXEL << ", " << b->y2/PIXEL << "\n";
  SI ox= b->x1;
  SI oy= -b->y2;
  SI xx= ((SI) (4.15 * x)) - ox;
  SI yy= ((SI) (4.15 * y)) - oy;
  // cout << "Point  : " << xx/PIXEL << ", " << yy/PIXEL << LF;

  rectangles rs;
  bool       found_flag= false;
  path       old_p= b->find_box_path (last_x, last_y, 0, false, found_flag);
  found_flag      = false;
  path new_p      = b->find_box_path (xx, yy, 0, false, found_flag);
  if (path_up (old_p) != path_up (new_p)) {
    b->message ("leave", last_x, last_y, rs);
    b->message ("enter", xx, yy, rs);
  }
  last_x= xx;
  last_y= yy;

  if (kind == "move" && lpressed) b->message ("drag", xx, yy, rs);
  if (kind == "press-left") {
    lpressed= true;
    b->message ("click", xx, yy, rs);
  }
  if (kind == "release-left") {
    lpressed= false;
    b->message ("select", xx, yy, rs);
  }

  if (N (rs) > 0) {
    send_invalidate_all (this);
    // while (!is_nil (rs)) {
    //   send_invalidate (rs->item->x1-pixel, rs->item->y1-pixel,
    //                    rs->item->x2+pixel, rs->item->y2+pixel);
    //   rs= rs->next;
    // }
  }
}

/******************************************************************************
 * Interface
 ******************************************************************************/

widget
box_widget (box b, bool tr) {
  color  col = light_grey;
  double zoom= 4.0;
  return widget (
      tm_new<box_widget_rep> (b, col, tr, zoom, 3 * PIXEL, 3 * PIXEL));
}

widget
box_widget (scheme_tree p, string s, color col, bool trans, bool ink) {
  if (get_reverse_colors () ^ occurs ("dark", tm_style_sheet))
    col= reverse (col);
  string family  = "roman";
  string fn_class= "mr";
  string series  = "medium";
  string shape   = "normal";
  int    sz      = 10;
  int    dpi     = 600;
  int    n       = arity (p);
  if ((n >= 1) && is_atomic (p[0])) family= as_string (p[0]);
  if ((n >= 2) && is_atomic (p[1])) fn_class= as_string (p[1]);
  if ((n >= 3) && is_atomic (p[2])) series= as_string (p[2]);
  if ((n >= 4) && is_atomic (p[3])) shape= as_string (p[3]);
  if ((n >= 5) && is_atomic (p[4])) sz= as_int (p[4]);
  if ((n >= 6) && is_atomic (p[5])) dpi= as_int (p[5]);
  font fn= find_font (family, fn_class, series, shape, sz, dpi);
  box  b = text_box (decorate (), 0, s, fn, col);
  if (ink) b= resize_box (decorate (), b, b->x3, b->y3, b->x4, b->y4, true);
  return box_widget (b, trans);
}

tree enrich_embedded_document (tree body, tree style);

static bool
is_transparent (tree init) {
  for (int i= 0; i < N (init); i++)
    if (is_func (init[i], ASSOCIATE, 2) && init[i][0] == BG_COLOR) return false;
  return true;
}

widget
texmacs_output_widget (tree doc, tree style) {
  doc= enrich_embedded_document (doc, style);
  drd_info              drd ("none", std_drd);
  hashmap<string, tree> h1 (UNINIT), h2 (UNINIT);
  hashmap<string, tree> h3 (UNINIT), h4 (UNINIT);
  hashmap<string, tree> h5 (UNINIT), h6 (UNINIT);
  tree                  prj= extract (doc, "project");
  if (is_atomic (prj) && exists (url_system (prj->label))) {
    tm_buffer buf= concrete_buffer_insist (url_system (prj->label));
    if (!is_nil (buf)) {
      h1= copy (buf->data->ref);
      h3= copy (buf->data->aux);
      h5= copy (buf->data->att);
    }
  }
  edit_env env (drd, "none", h1, h2, h3, h4, h5, h6);
  initialize_environment (env, doc, drd);
  tree   t = extract (doc, "body");
  lazy   lz= make_lazy (env, t, path ());
  format vf=
      make_query_vstream_width (array<line_item> (), array<line_item> ());
  format rf= lz->query (LAZY_BOX, vf);
  SI     w = ((format_vstream) rf)->width;
  box    b = (box) lz->produce (LAZY_BOX, make_format_width (w));
  // cout << (b->w()>>8) << ", " << (b->h()>>8) << "\n";
  // SI dw1= env->get_length (PAGE_SCREEN_LEFT);
  // SI dw2= env->get_length (PAGE_SCREEN_RIGHT);
  // SI dh1= env->get_length (PAGE_SCREEN_BOT);
  // SI dh2= env->get_length (PAGE_SCREEN_TOP);
  color col= env->get_color (BG_COLOR);
  if (env->get_string (BG_COLOR) == "white" &&
      is_transparent (extract (doc, "body")))
#ifdef QTTEXMACS
    col= rgb_color (236, 236, 236);
#else
    col= light_grey;
#endif
  double zoom= (retina_zoom == 2 ? 1.0 : 1.2);
  return widget (tm_new<box_widget_rep> (b, col, false, zoom, 0, 0));
}

array<SI>
get_texmacs_widget_size (widget wid) {
  array<SI> ret;
  SI        w, h;
  ((simple_widget_rep*) wid.rep)->handle_get_size_hint (w, h);
  ret << w << h;
  return ret;
}
