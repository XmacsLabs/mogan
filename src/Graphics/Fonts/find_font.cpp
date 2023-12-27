
/******************************************************************************
 * MODULE     : find_font.cpp
 * DESCRIPTION: decoding font names
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Freetype/tt_file.hpp"
#include "analyze.hpp"
#include "font.hpp"
#include "hashmap.hpp"
#include "tm_debug.hpp"
#include "tree.hpp"

hashmap<string, tree> font_conversion ("rule");

/******************************************************************************
 * Declare a new rule
 ******************************************************************************/

void
font_rule (tree which, tree by) {
  if ((arity (which) * arity (by) == 0) || is_compound (which[0])) return;
  if (!font_conversion->contains (which[0]->label))
    font_conversion (which[0]->label)=
        tree (TUPLE, tree (ASSOCIATE, which, by));
  else font_conversion (which[0]->label) << tree (ASSOCIATE, which, by);
}

/******************************************************************************
 * Find a font
 ******************************************************************************/

string
strip_suffix (string name) {
  if (occurs (".", name)) {
    int pos= search_backwards (".", name);
    name   = name (0, pos);
  }
  return name;
}

static bool
matches (tree t, tree which, hashmap<string, tree>& H) {
  int i, n= arity (which);
  if (arity (t) != n) return false;
  for (i= 0; i < n; i++) {
    if (which[i]->label[0] == '$') H (which[i]->label)= t[i];
    else if (t[i] != which[i]) return false;
  }
  return true;
}

static tree
substitute (tree by, hashmap<string, tree>& H) {
  if (is_atomic (by)) return copy (by);
  else {
    int  i, n= N (by);
    tree r (by, n);
    for (i= 0; i < n; i++) {
      if (is_atomic (by[i]) && starts (by[i]->label, "$"))
        r[i]= H[by[i]->label];
      else r[i]= substitute (by[i], H);
    }
    return r;
  }
}

font
find_font_bis (tree t) {
  // cout << "Find " << t << "\n";

  if ((arity (t) == 0) || is_compound (t[0])) return font ();

  if (is_tuple (t, "compound")) return compound_font (t (1, N (t)), 1.0, 1.0);

  if (is_tuple (t, "truetype", 3))
    return tt_font (as_string (t[1]), as_int (t[2]), as_int (t[3]));

  if (is_tuple (t, "unicode", 3))
    return unicode_font (as_string (t[1]), as_int (t[2]), as_int (t[3]));

  if (is_tuple (t, "unimath", 5)) {
    font up = find_font (t[1]);
    font it = find_font (t[2]);
    font bup= find_font (t[3]);
    font bit= find_font (t[4]);
    font rb = find_font (t[5]);
    if (is_nil (up)) return up;
    if (is_nil (it)) return it;
    if (is_nil (bup)) return bup;
    if (is_nil (bit)) return bit;
    if (is_nil (rb)) return rb;
    return unicode_math_font (up, it, bup, bit, rb);
  }

  if (is_tuple (t, "x", 3))
    return x_font (as_string (t[1]), as_int (t[2]), as_int (t[3]));

  if (is_tuple (t, "qt", 3))
    return qt_font (as_string (t[1]), as_int (t[2]), as_int (t[3]));

  if (is_tuple (t, "tex", 3))
    return tex_font (as_string (t[1]), as_int (t[2]), as_int (t[3]));

  if (is_tuple (t, "tex", 4))
    return tex_font (as_string (t[1]), as_int (t[2]), as_int (t[3]),
                     as_int (t[4]));

  if (is_tuple (t, "cm", 3))
    return tex_cm_font (as_string (t[1]), as_int (t[2]), as_int (t[3]));

  if (is_tuple (t, "cm", 4))
    return tex_cm_font (as_string (t[1]), as_int (t[2]), as_int (t[3]),
                        as_int (t[4]));

  if (is_tuple (t, "ec", 3))
    return tex_ec_font (as_string (t[1]), as_int (t[2]), as_int (t[3]));

  if (is_tuple (t, "ec", 4))
    return tex_ec_font (as_string (t[1]), as_int (t[2]), as_int (t[3]),
                        as_int (t[4]));

  if (is_tuple (t, "la", 3))
    return tex_la_font (as_string (t[1]), as_int (t[2]) * 100, as_int (t[3]),
                        1000);

  if (is_tuple (t, "la", 4))
    return tex_la_font (as_string (t[1]), as_int (t[2]) * 100, as_int (t[3]),
                        as_int (t[4]) * 100);

  if (is_tuple (t, "gr", 3))
    return tex_gr_font (as_string (t[1]), as_int (t[2]) * 100, as_int (t[3]),
                        1000);

  if (is_tuple (t, "gr", 4))
    return tex_gr_font (as_string (t[1]), as_int (t[2]) * 100, as_int (t[3]),
                        as_int (t[4]) * 100);

  if (is_tuple (t, "adobe", 3))
    return tex_adobe_font (as_string (t[1]), as_int (t[2]), as_int (t[3]));

  if (is_tuple (t, "adobe", 4))
    return tex_adobe_font (as_string (t[1]), as_int (t[2]), as_int (t[3]),
                           as_int (t[4]));

  if (is_tuple (t, "tex-rubber", 4))
    return tex_rubber_font (as_string (t[1]), as_string (t[2]), as_int (t[3]),
                            as_int (t[4]));

  if (is_tuple (t, "tex-rubber", 5))
    return tex_rubber_font (as_string (t[1]), as_string (t[2]), as_int (t[3]),
                            as_int (t[4]), as_int (t[5]));

  if (is_tuple (t, "tex-dummy-rubber", 1)) {
    font fn= find_font (t[1]);
    if (is_nil (fn)) return fn;
    return tex_dummy_rubber_font (fn);
  }

  if (is_tuple (t, "error", 1)) {
    font fn= find_font (t[1]);
    if (is_nil (fn)) return fn;
    return error_font (fn);
  }

  if (is_tuple (t, "math", 4) && is_tuple (t[1]) && is_tuple (t[2])) {
    font fn= find_font (t[3]);
    if (is_nil (fn)) return fn;
    font error_fn= error_font (find_font (t[4]));
    if (is_nil (error_fn)) error_fn= error_font (fn);
    return math_font (t, fn, error_fn, 1.0, 1.0);
  }

  if (!font_conversion->contains (t[0]->label)) {
    font_database_load ();
    if (is_tuple (t) && N (t) == 6) {
      string        family = as_string (t[0]);
      string        variant= as_string (t[1]);
      string        series = as_string (t[2]);
      string        shape  = as_string (t[3]);
      array<string> a= font_database_search (family, variant, series, shape);
      // cout << t << " -> " << a << "\n";
      for (int i= 0; i < N (a); i++) {
        string            font_name    = basename (url_system (a[i]));
        pair<string, int> f_pair       = font_name_unpack (font_name);
        string            font_basename= f_pair.x1;
        if (tt_font_exists (font_basename))
          return unicode_font (font_name, as_int (t[4]), as_int (t[5]));
      }
    }
    return font ();
  }

  tree rule= font_conversion[t[0]->label];
  int  i, n= N (rule);
  for (i= 0; i < n; i++) {
    hashmap<string, tree> H ("?");
    if (matches (t, rule[i][0], H))
      return find_font (substitute (rule[i][1], H));
  }

  return font ();
}

font
find_font (tree t) {
  bench_start ("find font");
  font fn= find_font_bis (t);
  bench_cumul ("find font");
  return fn;
}

font
find_magnified_font (tree t, double zoomx, double zoomy) {
  font fn= find_font (t);
  if (is_nil (fn)) return fn;
  return fn->magnify (zoomx, zoomy);
}

/******************************************************************************
 * User interface
 ******************************************************************************/

font
find_font (string family, string variant, string series, string shape, int sz,
           int dpi) {
  string s= family * "-" * variant * "-" * series * "-" * shape * "-" *
            as_string (sz) * "-" * as_string (dpi);
  if (font::instances->contains (s)) return font (s);

  if (ends (shape, "-poorit")) {
    string shape2= shape (0, N (shape) - 7);
    font   fn    = find_font (family, variant, series, shape2, sz, dpi);
    if (!is_nil (fn)) {
      font nafn= fn->magnify (5.0 / 6.0, 1.0);
      font itfn= poor_italic_font (nafn, 0.25001);
      // NOTE: precise value 0.25001 also used in 'concat_math'
      font::instances (s)= (pointer) itfn.rep;
      return itfn;
    }
  }
  else if (ends (shape, "-poorsc")) {
    string shape2= shape (0, N (shape) - 7);
    font   fn    = find_font (family, variant, series, shape2, sz, dpi);
    if (!is_nil (fn)) {
      font scfn          = poor_smallcaps_font (fn);
      font::instances (s)= (pointer) scfn.rep;
      return scfn;
    }
  }
  else if (ends (series, "-poorbf")) {
    string series2= series (0, N (series) - 7);
    font   fn     = find_font (family, variant, series2, shape, sz, dpi);
    if (!is_nil (fn)) {
      font bffn          = poor_bold_font (fn);
      font::instances (s)= (pointer) bffn.rep;
      return bffn;
    }
  }
  else if (ends (variant, "-poorbbb")) {
    string variant2= variant (0, N (variant) - 8);
    font   fn      = find_font (family, variant2, series, shape, sz, dpi);
    if (!is_nil (fn)) {
      font bbbfn         = poor_bbb_font (fn);
      font::instances (s)= (pointer) bbbfn.rep;
      return bbbfn;
    }
  }

  string family2= family;
  if (family == "sys-chinese") family2= default_chinese_font_name ();
  if (family == "sys-japanese") family2= default_japanese_font_name ();
  if (family == "sys-korean") family2= default_korean_font_name ();
  if (family2 != family) {
    font fn= find_font (family2, variant, series, shape, sz, dpi);
    if (!is_nil (fn)) {
      font::instances (s)= (pointer) fn.rep;
      return fn;
    }
  }

  tree t1 (TUPLE, 6);
  t1[0]  = family;
  t1[1]  = variant;
  t1[2]  = series;
  t1[3]  = shape;
  t1[4]  = as_string (sz);
  t1[5]  = as_string (dpi);
  font fn= find_font (t1);
  if (!is_nil (fn)) {
    font::instances (s)= (pointer) fn.rep;
    return fn;
  }

  tree t2 (TUPLE, 5);
  t2[0]= family;
  t2[1]= variant;
  t2[2]= series;
  t2[3]= as_string (sz);
  t2[4]= as_string (dpi);
  fn   = find_font (t2);
  if (!is_nil (fn)) {
    font::instances (s)= (pointer) fn.rep;
    return fn;
  }

  tree t3 (TUPLE, 4);
  t3[0]= family;
  t3[1]= variant;
  t3[2]= as_string (sz);
  t3[3]= as_string (dpi);
  fn   = find_font (t3);
  if (!is_nil (fn)) {
    font::instances (s)= (pointer) fn.rep;
    return fn;
  }

  tree panic (TUPLE, "tex", "cmr", as_string (sz), as_string (dpi));
  fn                 = find_font (panic);
  font::instances (s)= (pointer) fn.rep;
  return fn;
}
