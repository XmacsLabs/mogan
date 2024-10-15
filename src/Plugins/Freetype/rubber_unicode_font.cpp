
/******************************************************************************
 * MODULE     : rubber_unicode_font.cpp
 * DESCRIPTION: Rubber unicode fonts
 * COPYRIGHT  : (C) 2015  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Freetype/tt_face.hpp"
#include "analyze.hpp"
#include "array.hpp"
#include "converter.hpp"
#include "font.hpp"
#include "hashmap.hpp"
#include "lolly/data/numeral.hpp"
#include "string.hpp"
#include "translator.hpp"
#include <cstddef>
#include <lolly/data/unicode.hpp>
using lolly::data::as_hexadecimal;
using lolly::data::decode_from_utf8;

bool supports_big_operators (string res_name); // from poor_rubber.cpp

/******************************************************************************
 * True Type fonts
 ******************************************************************************/

struct rubber_unicode_font_rep : font_rep {
  font        base;
  bool        big_flag;
  array<bool> initialized;
  array<font> subfn;
  bool        big_sums;

  tt_face    math_face;
  translator virt;

  hashmap<string, int>    mapper;
  hashmap<string, string> rewriter;

  rubber_unicode_font_rep (string name, font base, tt_face face= nullptr);
  font get_font (int nr);
  int  search_font_sub (string s, string& rew);
  bool search_font_sub_bis (string s, string& rew, int& nr);
  int  search_font_cached (string s, string& rew);
  font search_font (string& s);

  bool  supports (string c);
  void  get_extents (string s, metric& ex);
  void  get_xpositions (string s, SI* xpos);
  void  get_xpositions (string s, SI* xpos, SI xk);
  void  draw_fixed (renderer ren, string s, SI x, SI y);
  void  draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font  magnify (double zoomx, double zoomy);
  glyph get_glyph (string s);
  int   index_glyph (string s, font_metric& fnm, font_glyphs& fng);

  double get_left_slope (string s);
  double get_right_slope (string s);
  SI     get_left_correction (string s);
  SI     get_right_correction (string s);
  SI     get_lsub_correction (string s);
  SI     get_lsup_correction (string s);
  SI     get_rsub_correction (string s);
  SI     get_rsup_correction (string s);
  SI     get_wide_correction (string s, int mode);
};

/******************************************************************************
 * Initialization of main font parameters
 ******************************************************************************/

rubber_unicode_font_rep::rubber_unicode_font_rep (string name, font base2,
                                                  tt_face face)
    : font_rep (name, base2), base (base2),
      big_flag (supports_big_operators (base2->res_name)), math_face (face) {
  this->copy_math_pars (base);
  big_sums= false;
  if (base->supports ("<sum>")) {
    metric ex;
    base->get_extents ("<sum>", ex);
    // cout << base->res_name << " -> "
    //<< ((double) (ex->y2-ex->y1)) / base->yx << LF;
    if ((((double) (ex->y2 - ex->y1)) / base->yx) >= 1.55) big_sums= true;
  }
  for (int i= 0; i < 6; i++) {
    initialized << false;
    subfn << base;
  }
  if (!is_nil (math_face) && !is_nil (math_face->math_table)) {
    big_flag= true;
    big_sums= true;
  }

  string vname= "opentype_virtual[" * res_name * "]";
  virt        = translator (vname);
  virt->virt_def << tree (); // 0 glyph
}

font
rubber_unicode_font_rep::get_font (int nr) {
  ASSERT (nr < N (subfn), "wrong font number");
  if (initialized[nr]) return subfn[nr];
  initialized[nr]= true;
  switch (nr) {
  case 0:
    break;
  case 1:
    subfn[nr]= base->magnify (sqrt (0.5));
    break;
  case 2:
    subfn[nr]= base->magnify (sqrt (2.0));
    break;
  case 3:
    subfn[nr]= base->magnify (2.0);
    break;
  case 4:
    subfn[nr]= rubber_assemble_font (base);
    break;
  case 5:
    break;
  }
  return subfn[nr];
}

/******************************************************************************
 * Find the font
 ******************************************************************************/

int
parse_variant (string s, string& r, string& rg) {
  int var  = 0;
  int n    = N (s);
  int start= search_forwards ("-", 0, s);
  int end  = search_forwards ("-", n, s);
  if (start == end) {
    end= n - 1;
    var= 0;
  }
  else {
    var= max (0, as_int (s (end + 1, n)));
  }
  r = s (start + 1, end);
  rg= s (0, start);
  return var;
}

string
normalized_cork_to_utf8 (string s) {
  if (N (s) < 3) return s;
  static hashmap<string, string> mapper;
  if (N (mapper) == 0) {
    mapper ("<tilde>")        = "<#303>";
    mapper ("<check>")        = "<#30C>";
    mapper ("<bar>")          = "<#305>";
    mapper ("<vect>")         = "<#20D7>";
    mapper ("<breve>")        = "<#306>";
    mapper ("<invbreve>")     = "<#311>";
    mapper ("<punderbrace>")  = "<#23DD>";
    mapper ("<punderbrace*>") = "<#23DD>";
    mapper ("<underbrace>")   = "<#23DF>";
    mapper ("<underbrace*>")  = "<#23DF>";
    mapper ("<squnderbrace>") = "<#23B5>";
    mapper ("<squnderbrace*>")= "<#23B5>";
    mapper ("<poverbrace>")   = "<#23DC>";
    mapper ("<poverbrace*>")  = "<#23DC>";
    mapper ("<overbrace>")    = "<#23DE>";
    mapper ("<overbrace*>")   = "<#23DE>";
    mapper ("<sqoverbrace>")  = "<#23B4>";
    mapper ("<sqoverbrace*>") = "<#23B4>";
  }
  string r= mapper->contains (s) ? mapper[s] : s;
  return strict_cork_to_utf8 (r);
}

bool
rubber_unicode_font_rep::search_font_sub_bis (string s, string& rew, int& nr) {
  // look up opentype math table
  string r;
  string rg;
  int    var= 0;
  bool   hor= false; // horizontal or vertical
  rew       = s;
  nr        = 0;

  if (starts (s, "<big-")) {
    var= parse_variant (s, r, rg);
    var= max (0, var - 1);
    hor= false;
  }
  else if (starts (s, "<large-") || starts (s, "<mid-") ||
           starts (s, "<right-") || starts (s, "<left-")) {
    var= parse_variant (s, r, rg);
    hor= true;
  }
  else if (starts (s, "<wide-")) {
    var= parse_variant (s, r, rg);
    hor= true;
  }
  else {
    return false;
  }

  if (r == "") return false;

  string       uu     = normalized_cork_to_utf8 ("<" * r * ">");
  int          j      = 0;
  unsigned int u      = decode_from_utf8 (uu, j);
  unsigned int glyphID= ft_get_char_index (math_face->ft_face, u);

  auto variant= hor ? math_face->math_table->hor_glyph_variants
                    : math_face->math_table->ver_glyph_variants;

  if (variant->contains (glyphID)) {
    auto& v= variant (glyphID);
    if (var < N (v)) {
      auto res= v[var];
      rew     = "<@" * as_hexadecimal (res, 4) * ">";
      nr      = 0;
      cout << "Variant for " << uu << " -> " << glyphID << " -> " << rew << LF;
      return true;
    }
  }
  cout << "No variant for " << uu << " -> " << glyphID << LF;
  return false;
}

int
rubber_unicode_font_rep::search_font_sub (string s, string& rew) {
  if (starts (s, "<big-") && ends (s, "-1>")) {
    string r= s (5, N (s) - 3);
    if (ends (r, "lim")) r= r (0, N (r) - 3);
    if (starts (r, "up")) r= r (2, N (r));
    r= "<" * r * ">";
    if (base->supports (r)) {
      rew= r;
      if (r == "<sum>" || r == "<prod>" || ends (r, "int>"))
        if (big_sums) return 0;
      return 2;
    }
  }
  if (starts (s, "<big-") && ends (s, "-2>")) {
    if (big_flag && base->supports (s)) {
      rew= s;
      return 0;
    }
    string r= s (5, N (s) - 3);
    if (ends (r, "lim")) r= r (0, N (r) - 3);
    if (starts (r, "up")) r= r (2, N (r));
    if (big_flag && base->supports ("<big-" * r * "-1>")) {
      rew= "<big-" * r * "-1>";
      return 2;
    }
    r= "<" * r * ">";
    if (base->supports (r)) {
      rew= r;
      if (r == "<sum>" || r == "<prod>" || ends (r, "int>"))
        if (big_sums) return 2;
      return 3;
    }
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N (s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N (s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N (s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N (s), s);
    if (pos > 6) {
      if (s[pos - 1] == '-') pos--;
      string r= s (6, pos);
      if (r == ".") {
        rew= "";
        return 0;
      }
      if ((r == "(" && base->supports ("<#239C>")) ||
          (r == ")" && base->supports ("<#239F>")) ||
          (r == "[" && base->supports ("<#23A2>")) ||
          (r == "]" && base->supports ("<#23A5>")) ||
          ((r == "{" || r == "}") && base->supports ("<#23AA>")) ||
          (r == "sqrt" && base->supports ("<#23B7>"))) {
        rew= s;
        return 4;
      }
      rew= r;
      if (N (rew) > 1) rew= "<" * rew * ">";
      if (ends (s, "-0>")) return 0;
      return 0;
    }
  }
  rew= s;
  return 0;
}

int
rubber_unicode_font_rep::search_font_cached (string s, string& rew) {
  if (mapper->contains (s)) {
    rew= rewriter[s];
    return mapper[s];
  }
  // try opentype math table
  int nr= 0;
  // if (!is_nil (math_face) && !is_nil (math_face->math_table) &&
  //     !search_font_sub_bis (s, rew, nr)) {
    nr= search_font_sub (s, rew);
  // }

  mapper (s)  = nr;
  rewriter (s)= rew;
  // cout << s << " -> " << nr << ", " << rew << LF;
  return nr;
}

font
rubber_unicode_font_rep::search_font (string& s) {
  string rew;
  int    nr= search_font_cached (s, rew);
  s        = rew;
  return get_font (nr);
}

/******************************************************************************
 * Getting extents and drawing strings
 ******************************************************************************/

bool
rubber_unicode_font_rep::supports (string s) {
  if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
    string r= s (5, N (s) - 3);
    if (ends (r, "lim")) r= r (0, N (r) - 3);
    if (starts (r, "up")) r= r (2, N (r));
    if (N (r) > 1) r= "<" * r * ">";
    return base->supports (r);
  }
  if (starts (s, "<mid-")) s= "<left-" * s (5, N (s));
  if (starts (s, "<right-")) s= "<left-" * s (7, N (s));
  if (starts (s, "<large-")) s= "<left-" * s (7, N (s));
  if (starts (s, "<left-")) {
    int pos= search_backwards ("-", N (s), s);
    if (pos > 6) {
      if (s[pos - 1] == '-') pos--;
      string r= s (6, pos);
      if (r == ".") return true;
      if (r == "sqrt") return base->supports ("<#23B7>");
      if (N (r) > 1) r= "<" * r * ">";
      if (!base->supports (r)) return false;
      if (ends (s, "-0>")) return true;
      if (r == "(") return base->supports ("<#239C>");
      if (r == ")") return base->supports ("<#239F>");
      if (r == "[") return base->supports ("<#23A2>");
      if (r == "]") return base->supports ("<#23A5>");
      if (r == "{" || r == "}") return base->supports ("<#23AA>");
      return true;
    }
  }
  return base->supports (s);
}

void
rubber_unicode_font_rep::get_extents (string s, metric& ex) {
  font fn= search_font (s);
  fn->get_extents (s, ex);
}

void
rubber_unicode_font_rep::get_xpositions (string s, SI* xpos) {
  if (s == "") return;
  string r = s;
  font   fn= search_font (r);
  if (r == s) fn->get_xpositions (s, xpos);
  else if (N (r) != 1) font_rep::get_xpositions (s, xpos);
  else {
    int i, n= N (s);
    for (i= 1; i < n; i++)
      xpos[i]= 0;
    fn->get_xpositions (r, xpos + n - 1);
  }
}

void
rubber_unicode_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  if (s == "") return;
  string r = s;
  font   fn= search_font (r);
  if (r == s) fn->get_xpositions (s, xpos, xk);
  else if (N (r) != 1) font_rep::get_xpositions (s, xpos, xk);
  else {
    int i, n= N (s);
    for (i= 0; i < n; i++)
      xpos[i]= 0;
    fn->get_xpositions (r, xpos + n - 1, xk);
  }
}

void
rubber_unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y);
}

void
rubber_unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y,
                                     SI xk) {
  font fn= search_font (s);
  fn->draw_fixed (ren, s, x, y, xk);
}

font rubber_unicode_font (font base, tt_face face);
font
rubber_unicode_font_rep::magnify (double zoomx, double zoomy) {
  return rubber_unicode_font (base->magnify (zoomx, zoomy));
}

glyph
rubber_unicode_font_rep::get_glyph (string s) {
  font fn= search_font (s);
  return fn->get_glyph (s);
}

int
rubber_unicode_font_rep::index_glyph (string s, font_metric& fnm,
                                      font_glyphs& fng) {
  font fn= search_font (s);
  return fn->index_glyph (s, fnm, fng);
}

/******************************************************************************
 * Metric properties
 ******************************************************************************/

double
rubber_unicode_font_rep::get_left_slope (string s) {
  font fn= search_font (s);
  return fn->get_left_slope (s);
}

double
rubber_unicode_font_rep::get_right_slope (string s) {
  font fn= search_font (s);
  return fn->get_right_slope (s);
}

SI
rubber_unicode_font_rep::get_left_correction (string s) {
  font fn= search_font (s);
  return fn->get_left_correction (s);
}

SI
rubber_unicode_font_rep::get_right_correction (string s) {
  font fn= search_font (s);
  return fn->get_right_correction (s);
}

SI
rubber_unicode_font_rep::get_lsub_correction (string s) {
  font fn= search_font (s);
  return fn->get_lsub_correction (s);
}

SI
rubber_unicode_font_rep::get_lsup_correction (string s) {
  font fn= search_font (s);
  return fn->get_lsup_correction (s);
}

SI
rubber_unicode_font_rep::get_rsub_correction (string s) {
  font fn= search_font (s);
  return fn->get_rsub_correction (s);
}

SI
rubber_unicode_font_rep::get_rsup_correction (string s) {
  font fn= search_font (s);
  return fn->get_rsup_correction (s);
}

SI
rubber_unicode_font_rep::get_wide_correction (string s, int mode) {
  font fn= search_font (s);
  return fn->get_wide_correction (s, mode);
}

/******************************************************************************
 * Interface
 ******************************************************************************/

font
rubber_unicode_font (font base) {
  string name= "rubberunicode[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_unicode_font_rep> (name, base));
}

font
rubber_unicode_font (font base, tt_face face) {
  string name= "rubberunicode[" * base->res_name * "]";
  return make (font, name, tm_new<rubber_unicode_font_rep> (name, base, face));
}
