
/******************************************************************************
 * MODULE     : smart_font.cpp
 * DESCRIPTION: smart merging of several fonts for different unicode ranges
 * COPYRIGHT  : (C) 2013  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <tp://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "smart_font.hpp"
#include "Freetype/tt_tools.hpp"
#include "analyze.hpp"
#include "array.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "cork.hpp"
#include "font.hpp"
#include "iterator.hpp"
#include "lolly/data/unicode.hpp"
#include "scheme.hpp"
#include "tm_debug.hpp"
#include "translator.hpp"
#include "unicode.hpp"

#include <lolly/data/numeral.hpp>
#include <lolly/data/unicode.hpp>

using lolly::data::decode_from_utf8;
using lolly::data::to_Hex;

bool virtually_defined (string c, string name);
font smart_font_bis (string f, string v, string s, string sh, int sz, int hdpi,
                     int vdpi);

smart_map
get_smart_map (tree fn) {
  string name= recompose (tuple_as_array (fn), "-");
  if (smart_map::instances->contains (name)) return smart_map (name);
  return make (smart_map, name, tm_new<smart_map_rep> (name, fn));
}

/******************************************************************************
 * Virtual font handling
 ******************************************************************************/

static bool              virt_initialized= false;
static array<string>     std_virt;
static array<translator> std_trl;

static void
initialize_virtual () {
  if (virt_initialized) return;
  std_virt << string ("tradi-long") << string ("tradi-negate")
           << string ("tradi-misc");
  for (int i= 0; i < N (std_virt); i++)
    std_trl << load_translator (std_virt[i]);
  virt_initialized= true;
}

static string
find_in_virtual (string c) {
  initialize_virtual ();
  for (int i= 0; i < N (std_virt); i++)
    if (std_trl[i]->dict->contains (c)) return std_virt[i];
  return "";
}

static bool       gen_initialized= false;
static translator gen_trl;

static bool
find_in_emu_bracket (string c) {
  if (!gen_initialized) {
    gen_trl        = load_translator ("emu-bracket");
    gen_initialized= true;
  }
  return gen_trl->dict->contains (c);
}

static array<string>
emu_font_names () {
  array<string> a;
  a << string ("emu-fundamental") << string ("emu-greek")
    << string ("emu-operators") << string ("emu-relations")
    << string ("emu-orderings") << string ("emu-setrels")
    << string ("emu-arrows");
  return a;
}

/******************************************************************************
 * Special characters in mathematical fonts
 ******************************************************************************/

static string rewrite_math (string s);

static bool
is_math_family (string f) {
  return f == "roman" || f == "concrete" || f == "Euler" || f == "ENR";
}

static bool
is_greek (string c) {
  static hashmap<string, bool> t (false);
  if (N (t) == 0) {
    array<int> a;
    // for (int i= 0x391; i<=0x3a9; i++) if (i != 0x3a2) a << i;
    for (int i= 0x3b1; i <= 0x3c9; i++)
      a << i;
    a << 0x3d1 << 0x3d5 << 0x3d6 << 0x3f0 << 0x3f1 << 0x3f5;
    for (int i= 0; i < N (a); i++) {
      string s            = "<#" * to_Hex (a[i]) * ">";
      t (s)               = true;
      t (locase_all (s))  = true;
      t (rewrite_math (s))= true;
    }
  }
  return t[c];
}

static bool
is_rubber (string c) {
  return (starts (c, "<large-") || starts (c, "<left-") ||
          starts (c, "<right-") || starts (c, "<mid-")) &&
         ends (c, ">");
}

static hashmap<string, string> special_table ("");

static bool
unicode_provides (string s) {
  return strict_cork_to_utf8 (s) != s;
}

static bool
is_special (string s) {
  if (N (special_table) == 0) {
    special_table ("<noplus>")   = "";
    special_table ("<nocomma>")  = "";
    special_table ("<nospace>")  = "";
    special_table ("<nobracket>")= "";
    special_table ("<nosymbol>") = "";
    special_table ("*")          = "";
    special_table ("-")          = "<minus>";
    special_table ("|")          = "<mid>";
    special_table ("'")          = "<#2B9>";
    special_table ("`")          = "<backprime>";
    special_table ("<hat>")      = "<#2C6>";
    special_table ("<tilde>")    = "<#2DC>";
    special_table ("<comma>")    = ",";
  }
  if (starts (s, "<big-.")) special_table (s)= "";
  if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
    string ss= s (0, N (s) - 3) * ">";
    // cout << "Search " << ss << "\n";
    if (unicode_provides (ss)) special_table (s)= ss;
    ss= "<big" * s (5, N (s) - 3) * ">";
    // cout << "Search " << ss << "\n";
    if (unicode_provides (ss)) special_table (s)= ss;
    ss= "<" * s (5, N (s) - 3) * ">";
    if (ends (ss, "lim>")) ss= ss (0, N (ss) - 4) * ">";
    // cout << "Search " << ss << "\n";
    if (unicode_provides (ss)) special_table (s)= ss;
  }
  return special_table->contains (s);
}

/******************************************************************************
 * Mathematical letters in Unicode
 ******************************************************************************/

static hashmap<string, string> substitution_char ("");
static hashmap<string, string> substitution_font ("");

static void
unicode_subst (int src, int dest, int nr, string fn) {
  for (int i= 0; i < nr; i++) {
    string csrc = "<#" * to_Hex (src + i) * ">";
    string cdest= "<#" * to_Hex (dest + i) * ">";
    if (dest + i < 128) cdest= string ((char) (dest + i));
    substitution_char (csrc)= cdest;
    substitution_font (csrc)= fn;
    csrc                    = locase_all (csrc);
    substitution_char (csrc)= cdest;
    substitution_font (csrc)= fn;
    csrc                    = rewrite_math (csrc);
    substitution_char (csrc)= cdest;
    substitution_font (csrc)= fn;
  }
}

static void
unicode_letters (int start, string fn) {
  unicode_subst (start, 0x41, 26, fn);
  unicode_subst (start + 26, 0x61, 26, fn);
}

static void
unicode_greek (int start, string fn) {
  unicode_subst (start, 0x391, 25, fn); // FIXME: attention to 0x3a2
  unicode_subst (start + 25, 0x2207, 1, fn);
  unicode_subst (start + 26, 0x3b1, 25, fn);
  unicode_subst (start + 51, 0x2202, 1, fn);
  unicode_subst (start + 52, 0x3f5, 1, fn);
  unicode_subst (start + 53, 0x3d1, 1, fn);
  unicode_subst (start + 54, 0x3f0, 1, fn);
  unicode_subst (start + 55, 0x3d5, 1, fn);
  unicode_subst (start + 56, 0x3f1, 1, fn);
  unicode_subst (start + 57, 0x3d6, 1, fn);
}

static void
unicode_digits (int start, string fn) {
  unicode_subst (start, 0x30, 10, fn);
}

static void
init_unicode_substitution () {
  if (N (substitution_char) != 0) return;
  unicode_letters (0x1d400, "bold-math");
  unicode_letters (0x1d434, "italic-math");
  unicode_letters (0x1d468, "bold-italic-math");
  unicode_letters (0x1d49c, "cal");
  unicode_letters (0x1d4d0, "bold-cal");
  unicode_letters (0x1d504, "frak");
  unicode_letters (0x1d56c, "bold-frak");
  unicode_letters (0x1d538, "bbb");
  unicode_letters (0x1d5a0, "ss");
  unicode_letters (0x1d5d4, "bold-ss");
  unicode_letters (0x1d608, "italic-ss");
  unicode_letters (0x1d63c, "bold-italic-ss");
  unicode_letters (0x1d670, "tt");
  unicode_greek (0x1d6a8, "bold-math");
  unicode_greek (0x1d6e2, "italic-math");
  unicode_greek (0x1d71c, "bold-italic-math");
  unicode_greek (0x1d756, "bold-ss");
  unicode_greek (0x1d790, "bold-italic-ss");
  unicode_digits (0x1d7ce, "bold-math");
  unicode_digits (0x1d7d8, "bbb");
  unicode_digits (0x1d7e2, "ss");
  unicode_digits (0x1d7ec, "bold-ss");
  unicode_digits (0x1d7f6, "tt");
  unicode_subst (0x212c, 0x42, 1, "cal");
  unicode_subst (0x2130, 0x45, 1, "cal");
  unicode_subst (0x2131, 0x46, 1, "cal");
  unicode_subst (0x210b, 0x48, 1, "cal");
  unicode_subst (0x2110, 0x49, 1, "cal");
  unicode_subst (0x2112, 0x4c, 1, "cal");
  unicode_subst (0x2133, 0x4d, 1, "cal");
  unicode_subst (0x211b, 0x52, 1, "cal");
  unicode_subst (0x212f, 0x65, 1, "cal");
  unicode_subst (0x210a, 0x67, 1, "cal");
  unicode_subst (0x2134, 0x6f, 1, "cal");
  unicode_subst (0x212d, 0x43, 1, "frak");
  unicode_subst (0x210c, 0x49, 1, "frak");
  unicode_subst (0x2111, 0x4a, 1, "frak");
  unicode_subst (0x211c, 0x52, 1, "frak");
  unicode_subst (0x2128, 0x5a, 1, "frak");
  unicode_subst (0x2102, 0x43, 1, "bbb");
  unicode_subst (0x210d, 0x48, 1, "bbb");
  unicode_subst (0x2115, 0x4e, 1, "bbb");
  unicode_subst (0x2119, 0x50, 1, "bbb");
  unicode_subst (0x211a, 0x51, 1, "bbb");
  unicode_subst (0x211d, 0x52, 1, "bbb");
  unicode_subst (0x2124, 0x5a, 1, "bbb");
}

int
get_utf8_code (string c) {
  int c_N= N (c);
  if (c_N <= 2 || c_N > 6) {
    // the largest unicode is U+10FFFF
    return -1;
  }
  string uc  = strict_cork_to_utf8 (c);
  int    pos = 0;
  int    code= decode_from_utf8 (uc, pos);
  if (pos == c_N) return code;
  else return -1;
}

string
substitute_math_letter (string c, int math_kind) {
  if (math_kind == 0) return "";
  int code= get_utf8_code (c);
  if ((code >= 0x1d400 && code <= 0x1d7ff) ||
      (code >= 0x2100 && code <= 0x213f)) {
    init_unicode_substitution ();
    string nc= "<#" * to_Hex (code) * ">";
    string sc= substitution_char[nc];
    string sf= substitution_font[nc];
    // cout << c << " (" << nc << ") -> " << sc << ", " << sf << "\n";
    if (sc != "" && sc != c) {
      bool flag= ends (sf, "cal") || ends (sf, "frak") || ends (sf, "bbb");
      if (!flag || math_kind == 2) return sf;
    }
  }
  return "";
}

/******************************************************************************
 * Getting mathematical characters from unicode planes
 ******************************************************************************/

static hashmap<string, string> italic_greek ("");

static void
unicode_subst_back (int dest, int src, int nr, hashmap<string, string>& h) {
  for (int i= 0; i < nr; i++) {
    string csrc = "<#" * to_Hex (src + i) * ">";
    string cdest= "<#" * to_Hex (dest + i) * ">";
    if (src + i < 128) csrc= string ((char) (src + i));
    if (dest + i < 128) cdest= string ((char) (dest + i));
    h (csrc)= cdest;
    csrc    = locase_all (csrc);
    h (csrc)= cdest;
    csrc    = rewrite_math (csrc);
    h (csrc)= cdest;
  }
}

string
substitute_italic_greek (string c) {
  hashmap<string, string>& h (italic_greek);
  if (N (h) == 0) {
    int start= 0x1d6e2;
    unicode_subst_back (start, 0x391, 25, h); // FIXME: attention to 0x3a2
    unicode_subst_back (start + 25, 0x2207, 1, h);
    unicode_subst_back (start + 26, 0x3b1, 25, h);
    unicode_subst_back (start + 51, 0x2202, 1, h);
    unicode_subst_back (start + 52, 0x3f5, 1, h);
    unicode_subst_back (start + 53, 0x3d1, 1, h);
    unicode_subst_back (start + 54, 0x3f0, 1, h);
    unicode_subst_back (start + 55, 0x3d5, 1, h);
    unicode_subst_back (start + 56, 0x3f1, 1, h);
    unicode_subst_back (start + 57, 0x3d6, 1, h);
  }
  if (!italic_greek->contains (c)) return "";
  return italic_greek[c];
}

string
substitute_upright_greek (string c) {
  if (!starts (c, "<up")) return "";
  if (starts (c, "<up-")) c= "<" * c (4, N (c));
  else if (starts (c, "<up")) c= "<" * c (3, N (c));
  if (!is_greek (c)) return "";
  return c;
}

string
substitute_upright (string c) {
  if (!starts (c, "<up-") || !ends (c, ">")) return "";
  if (N (c) == 6) return c (4, 5);
  return "<" * c (4, N (c));
}

string
substitute_italic (string c) {
  if (!starts (c, "<it-") || !ends (c, ">")) return "";
  if (N (c) == 6) return c (4, 5);
  return "<" * c (4, N (c));
}

/******************************************************************************
 * Font sequences
 ******************************************************************************/

array<string>
trimmed_tokenize (string s, string sep) {
  return trim_spaces (tokenize (s, sep));
}

string
main_family (string f) {
  array<string> a= trimmed_tokenize (f, ",");
  for (int i= 0; i < N (a); i++)
    if (N (trimmed_tokenize (a[i], "=")) <= 1) return a[i];
  if (N (a) == 0) return f;
  a= trimmed_tokenize (a[0], "=");
  if (N (a) <= 1) return f;
  return a[1];
}

static bool
is_cjk_punct (string_u8 c) {
  static hashset<string_u8> set;
  if (N (set) == 0) {
    set->insert ("“");
    set->insert ("”");
    set->insert ("‘");
    set->insert ("’");
    set->insert ("·");
  }
  return set->contains (c);
}

static bool
in_unicode_range (string c, string range) {
  string uc= strict_cork_to_utf8 (c);
  if (N (uc) == 0) return false;
  int    pos = 0;
  int    code= decode_from_utf8 (uc, pos);
  string got = lolly::data::unicode_get_range (code);
  if (range == got) return range != "";
  if (range == "cjk") {
    if (got == "hangul" || got == "hiragana" ||
        got == "enclosed_alphanumerics" || got == "latin")
      return true;
    return is_cjk_punct (uc);
  }
  // There are actually two ranges (cjk/hangul) for Korean characters and
  // two ranges (cjk/hiragana) for Japanese characters
  // For example, on macOS, `sys-korean` is expanded to `cjk=Apple SD Gothic
  // Neo,roman`, assuming that `Apple SD Gothic Neo` is the default korean font
  // on macOS
  if (range == "mathlarge" || range == "mathbigop")
    if (starts (c, "<big-") || (code >= 0x220f && code <= 0x2211) ||
        (code >= 0x222b && code <= 0x2233) ||
        (code >= 0x22c0 && code <= 0x22c3) ||
        (code >= 0x2a00 && code <= 0x2a1c))
      return true;
  if (range == "mathlarge" || range == "mathrubber")
    if (starts (c, "<wide-") || starts (c, "<large-") || starts (c, "<left-") ||
        starts (c, "<mid-") || starts (c, "<right-"))
      return true;
  return false;
}

/******************************************************************************
 * Further character collections
 ******************************************************************************/

static hashmap<string, hashset<string>> char_collections;

static void
collection_insert (string name, string c) {
  if (c == "") return;
  if (!char_collections->contains (name))
    char_collections (name)= hashset<string> ();
  char_collections (name)->insert (c);
  int code= get_utf8_code (c);
  if (code >= 0) {
    string uc= "<#" * to_Hex (code) * ">";
    if (uc != c) char_collections (name)->insert (uc);
  }
}

static void
collection_inherit (string name, string base) {
  hashset<string>  h = char_collections[base];
  iterator<string> it= iterate (h);
  while (it->busy ())
    collection_insert (name, it->next ());
}

static void
init_collections () {
  if (N (char_collections) > 0) return;
  for (char c= '0'; c <= '9'; c++)
    collection_insert ("digit", string (c));
  for (char c= 'a'; c <= 'z'; c++) {
    collection_insert ("lowercase-latin", string (c));
    collection_insert ("lowercase-latin-bold", "<b-" * string (c) * ">");
  }
  for (char c= 'A'; c <= 'Z'; c++) {
    collection_insert ("uppercase-latin", string (c));
    collection_insert ("uppercase-latin-bold", "<b-" * string (c) * ">");
  }
  collection_inherit ("latin", "lowercase-latin");
  collection_inherit ("latin", "uppercase-latin");
  collection_inherit ("latin-bold", "lowercase-latin-bold");
  collection_inherit ("latin-bold", "uppercase-latin-bold");
  for (int code= 0x380; code <= 0x3ff; code++) {
    string uc= "<#" * to_Hex (code) * ">";
    string gc= rewrite_math (uc);
    if (gc != uc) {
      string bgc= "<b-" * gc (1, N (gc));
      if (is_locase (gc[1])) {
        collection_insert ("lowercase-greek", gc);
        collection_insert ("lowercase-greek", substitute_italic_greek (gc));
        collection_insert ("lowercase-greek-bold", bgc);
      }
      if (is_upcase (gc[1])) {
        collection_insert ("uppercase-greek", gc);
        collection_insert ("uppercase-greek", substitute_italic_greek (gc));
        collection_insert ("uppercase-greek-bold", bgc);
      }
      collection_insert ("greek", gc);
      collection_insert ("greek", substitute_italic_greek (gc));
      collection_insert ("greek-bold", bgc);
    }
  }
  collection_inherit ("basic-letters", "digit");
  collection_inherit ("basic-letters", "latin");
  collection_inherit ("basic-letters", "latin-bold");
  collection_inherit ("basic-letters", "greek");
  collection_inherit ("basic-letters", "greek-bold");
}

static bool
in_collection (string c, string name) {
  init_collections ();
  return char_collections->contains (name) &&
         char_collections[name]->contains (c);
}

/******************************************************************************
 * Font substitutions
 ******************************************************************************/

string
tex_gyre_fix (string family, string series, string shape) {
  for (int i= N (family) - 1; i >= 0; i--)
    if (family[i] == ',')
      return family (0, i + 1) *
             tex_gyre_fix (family (i + 1, N (family)), series, shape);
  if (family == "bonum") family= "TeX Gyre Bonum";
  if (family == "pagella") family= "TeX Gyre Pagella";
  if (family == "schola") family= "TeX Gyre Schola";
  if (family == "termes") family= "TeX Gyre Termes";
  if (starts (family, "TeX Gyre")) {
    if (starts (family, "TeX Gyre Bonum") ||
        starts (family, "TeX Gyre Pagella") ||
        starts (family, "TeX Gyre Schola") ||
        starts (family, "TeX Gyre Termes")) {
      if (starts (shape, "math") && series == "medium") {
        if (!ends (family, " Math")) family= family * " Math";
      }
      else if (ends (family, " Math")) family= family (0, N (family) - 5);
    }
  }
  return family;
}

string
kepler_fix (string family, string series, string shape) {
  for (int i= N (family) - 1; i >= 0; i--)
    if (family[i] == ',')
      return family (0, i + 1) *
             kepler_fix (family (i + 1, N (family)), series, shape);
  if (starts (family, "Kepler")) {
    if (starts (shape, "math") && series == "medium") {
      if (!ends (family, " Math")) family= family * " Math";
    }
    else if (ends (family, " Math")) family= family (0, N (family) - 5);
  }
  return family;
}

string
stix_fix (string family, string series, string shape) {
  if (family == "stix") family= "Stix";
  if (starts (family, "Stix")) {
    if (starts (shape, "math") && series == "medium") {
      if (!ends (family, " Math")) family= family * " Math";
    }
    else if (ends (family, " Math")) family= family (0, N (family) - 5);
  }
  return family;
}

string
math_fix (string family, string series, string shape) {
  if (starts (shape, "math")) {
    array<string> a= trimmed_tokenize (family, ","), r;
    for (int i= 0; i < N (a); i++) {
      array<string> b= trimmed_tokenize (a[i], "=");
      if (N (b) == 2) {
        array<string> c= trimmed_tokenize (b[0], " ");
        if (contains (string ("math"), c)) {
          for (int j= 0; j < N (c); j++)
            if (c[j] == "math") {
              c= append (range (c, 0, j), range (c, j + 1, N (c)));
              break;
            }
          string conds= recompose (c, " ");
          string base = tex_gyre_fix (b[1], series, shape);
          base        = kepler_fix (base, series, shape);
          // base= stix_fix (base, series, shape);
          string mathfn= (N (c) == 0 ? base : conds * "=" * base);
          r << mathfn;
        }
        else r << a[i];
      }
      else r << a[i];
    }
    family= recompose (r, ",");
  }
  return family;
}

/******************************************************************************
 * The smart font class
 ******************************************************************************/

typedef int                  int_vector[256];
typedef hashmap<string, int> int_table;

smart_font_rep::smart_font_rep (string name, font base_fn, font err_fn,
                                string family2, string variant2, string series2,
                                string shape2, int sz2, int hdpi2, int vdpi2)
    : font_rep (name, base_fn), mfam (main_family (family2)), family (family2),
      variant (variant2), series (series2), shape (shape2), rshape (shape2),
      sz (sz2), hdpi (hdpi2), dpi (vdpi2), math_kind (0), italic_nr (-1),
      fn (2), sm (get_smart_map (tuple (family2, variant2, series2, shape2))) {
  fn[SUBFONT_MAIN] = adjust_subfont (base_fn);
  fn[SUBFONT_ERROR]= adjust_subfont (err_fn);
  this->copy_math_pars (base_fn);
  if (shape == "mathitalic" || shape == "mathupright" || shape == "mathshape") {
    if (is_math_family (mfam)) {
      rshape= "right";
      if (shape == "mathupright") this->copy_math_pars (base_fn);
      else {
        tree key= tuple ("math", mfam, variant, series, rshape);
        int  nr = sm->add_font (key, REWRITE_MATH);
        initialize_font (nr);
        this->copy_math_pars (fn[nr]);
        fn[SUBFONT_MAIN]= fn[nr];
      }
    }
    else {
      math_kind= 1;
      if (shape == "mathupright") math_kind= 2;
      if (shape == "mathshape") math_kind= 3;
      rshape= "right";
      if (math_kind == 2) this->copy_math_pars (base_fn);
      else {
        italic_nr= sm->add_font (tuple ("fast-italic"), REWRITE_NONE);
        initialize_font (italic_nr);
        this->copy_math_pars (fn[italic_nr]);
      }
      (void) sm->add_font (tuple ("special"), REWRITE_SPECIAL);
      (void) sm->add_font (tuple ("emu-bracket"), REWRITE_EMULATE);
      (void) sm->add_font (tuple ("other"), REWRITE_NONE);
      (void) sm->add_font (tuple ("regular"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-math"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("italic-math"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-italic-math"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("cal"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-cal"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("frak"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-frak"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bbb"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("tt"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("italic-ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-italic-ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("italic-roman"), REWRITE_NONE);
    }
  }
}

font
smart_font_rep::adjust_subfont (font fn) {
  if (hdpi == dpi) return fn;
  double zoomx= ((double) hdpi) / ((double) dpi);
  return fn->magnify (zoomx, 1.0);
}

/******************************************************************************
 * Fonts for backward compatibility
 ******************************************************************************/

font
smart_font_rep::get_math_font (string fam, string var, string ser, string sh) {
  find_closest (fam, var, ser, sh);
  string mvar= "mr";
  if (var == "ss") mvar= "ms";
  if (var == "tt") mvar= "mt";
  return find_font (fam, mvar, ser, "", sz, dpi);
}

font
smart_font_rep::get_cyrillic_font (string fam, string var, string ser,
                                   string sh) {
  find_closest (fam, var, ser, sh);
  return find_font ("cyrillic", var, ser, sh, sz, dpi);
}

font
smart_font_rep::get_greek_font (string fam, string var, string ser, string sh) {
  find_closest (fam, var, ser, sh);
  return find_font ("greek", var, ser, sh, sz, dpi);
}

font
smart_font_rep::get_latin_font (string fam, string var, string ser, string sh) {
  find_closest (fam, var, ser, sh);
  return find_font ("CMU", var, ser, sh, sz, dpi);
}

static string
rewrite_math (string s) {
  string r;
  int    i= 0, n= N (s);
  while (i < n) {
    int start= i;
    tm_char_forwards (s, i);
    if (s[start] == '<' && start + 1 < n && s[start + 1] == '#' &&
        s[i - 1] == '>')
      r << utf8_to_cork (strict_cork_to_utf8 (s (start, i)));
    else r << s (start, i);
  }
  return r;
}

static string
rewrite_letters (string s) {
  init_unicode_substitution ();
  string r;
  int    i= 0, n= N (s);
  while (i < n) {
    int start= i;
    tm_char_forwards (s, i);
    string ss= s (start, i);
    if (substitution_char->contains (ss)) r << substitution_char[ss];
    else r << ss;
  }
  return r;
}

static string
rewrite (string s, int kind) {
  switch (kind) {
  case REWRITE_NONE:
    return s;
  case REWRITE_MATH:
    return rewrite_math (s);
  case REWRITE_CYRILLIC:
    return code_point_to_cyrillic_subset_in_t2a (s);
  case REWRITE_LETTERS:
    return rewrite_letters (s);
  case REWRITE_SPECIAL:
    return special_table[s];
  case REWRITE_EMULATE:
    return (N (s) <= 1 ? s : string ("<emu-") * s (1, N (s)));
  case REWRITE_POOR_BBB:
    return s (N (s) - 2, N (s) - 1);
  case REWRITE_ITALIC_GREEK:
    return substitute_italic_greek (s);
  case REWRITE_UPRIGHT_GREEK:
    return substitute_upright_greek (s);
  case REWRITE_UPRIGHT:
    return substitute_upright (s);
  case REWRITE_ITALIC:
    return substitute_italic (s);
  case REWRITE_IGNORE:
    return "";
  default:
    return s;
  }
}

/******************************************************************************
 * Smart font resolution
 ******************************************************************************/

void
smart_font_rep::advance (string s, int& pos, string& r, int& nr) {
  int*                  chv= sm->chv;
  hashmap<string, int>& cht (sm->cht);
  int                   count= 0;
  int                   start= pos;
  nr                         = -1;
  int s_N                    = N (s);
  if (s_N == 1 && pos == 0 && math_kind == 1 && ends (mfam, "Math") &&
      is_alpha (s[0])) {
    nr = 0;
    pos= pos + 1;
    if (is_locase (s[0])) {
      r= "<#" * to_Hex (0x1d44e + (int) (s[0] - 'a')) * ">";
    }
    else if (is_upcase (s[0])) {
      r= "<#" * to_Hex (0x1d434 + (int) (s[0] - 'A')) * ">";
    }
    else {
      TM_FAILED ("invalid branch for smart_fon_rep::advance");
    }
  }
  else {
    while (pos < s_N) {
      if (s[pos] != '<') {
        int c= (int) (unsigned char) s[pos];

        int fn_index= chv[c];
        if (math_kind != 0 && math_kind != 2 && is_alpha (c) &&
            (pos == 0 || !is_alpha (s[pos - 1])) &&
            (pos + 1 == s_N || !is_alpha (s[pos + 1]))) {
          fn_index= italic_nr;
        }
        else if (fn_index == -1) {
          fn_index= resolve (s (pos, pos + 1));
        }

        if (count == 1 && nr != -1 && fn_index == nr) {
          // NOTE: rewrite strings like --- character by character if necessary
          if (sm->fn_rewr[nr] == REWRITE_SPECIAL) break;
        }
        if (fn_index == nr) pos++;
        else if (nr == -1) {
          pos++;
          nr= fn_index;
        }
        else break;
      }
      else {
        int end= pos;
        tm_char_forwards (s, end);
        string current_c= s (pos, end);

        int fn_index= cht[current_c];
        if (fn_index == -1) {
          fn_index= resolve (current_c);
        }

        if (count == 1 && nr != -1 && fn_index == nr) {
          if (N (fn) <= nr || is_nil (fn[nr])) initialize_font (nr);
          if (!fn[nr]->supports (s (start, end))) break;
          pos= end;
        }
        else if (fn_index == nr) pos= end;
        else if (nr == -1) {
          pos= end;
          nr = fn_index;
        }
        else break;
      }
      count++;
    }
    r= s (start, pos);
    if (nr < 0) return;
    if (N (fn) <= nr || is_nil (fn[nr])) initialize_font (nr);
    if (sm->fn_rewr[nr] != REWRITE_NONE) r= rewrite (r, sm->fn_rewr[nr]);
  }
  if (DEBUG_VERBOSE) {
    debug_fonts << "Advance for font of [" << s << "] " << this->res_name
                << " math_kind: " << math_kind << LF;
    debug_fonts << "Physical font of [" << r << "]"
                << "[" << herk_to_utf8 (r) << "][" << cork_to_utf8 (r) << "]"
                << " is " << fn[nr]->res_name << LF;
  }
}

bool
is_italic_font (string master) {
  return contains (string ("italic"), master_features (master));
}

static bool
is_wanted (string c, string family, array<string> rules, array<string> given) {
  int rules_N= N (rules);
  for (int i= 0; i < rules_N; i++) {
    if (is_empty (rules[i])) continue;
    bool          ok= false;
    array<string> v = tokenize (rules[i], "|");
    for (int j= 0; j < N (v); j++) {
      string wanted= locase_all (v[j]);
      if (is_empty (wanted)) ok= true;
      else if (contains (wanted, given)) ok= true;
      else if (in_unicode_range (c, wanted)) ok= true;
      else if (wanted == substitute_math_letter (c, 2)) ok= true;
      else if (wanted == c) ok= true;
      else if (in_collection (c, wanted)) ok= true;
      else if (N (wanted) > 0 && wanted[0] == '!' && !in_collection (c, wanted))
        ok= true;
      else {
        array<string> w= tokenize (v[j], ":");
        if (N (w) == 1) w << w[0];
        if (N (w) == 2) {
          int code = get_utf8_code (c);
          int start= get_utf8_code (w[0]);
          int end  = get_utf8_code (w[1]);
          if (code != -1 && code >= start && code <= end) ok= true;
        }
      }
    }
    if (!ok) return false;
  }
  return true;
}

int
smart_font_rep::resolve (string c, string fam, int attempt) {
  string range= get_unicode_range (c);
  if (DEBUG_VERBOSE) {
    debug_fonts << "Resolve " << c << " in math_kind " << math_kind
                << " in unicode range " << range << " in fam " << fam
                << " mfam " << mfam << ", attempt " << attempt << LF;
  }
  array<string> a= trimmed_tokenize (fam, "=");
  if (N (a) >= 2) {
    array<string> given= logical_font (family, variant, series, rshape);
    fam                = a[1];
    array<string> b    = tokenize (a[0], " ");
    bool          ok   = is_wanted (c, fam, b, given);
    if (!ok) {
      return -1;
    }

    fam= tex_gyre_fix (fam, series, shape);
    fam= kepler_fix (fam, series, shape);
    // fam= stix_fix (fam, series, shape);

    if (math_kind != 0 && shape == "mathitalic" &&
        (range == "greek" || (starts (c, "<b-") && ends (c, ">")) ||
         c == "<imath>" || c == "<jmath>" || c == "<ell>")) {
      font cfn= smart_font_bis (fam, variant, series, shape, sz, hdpi, dpi);
      if (cfn->supports (c)) {
        tree key= tuple ("subfont", fam);
        int  nr = sm->add_font (key, REWRITE_NONE);
        initialize_font (nr);
        return sm->add_char (key, c);
      }
    }
  }

  if (N (c) == 1 && is_alpha (c[0]) && ends (fam, " Math") && shape == "italic")
    if (starts (fam, "TeX Gyre ") || starts (fam, "Stix "))
      fam= fam (0, N (fam) - 5);

  if (attempt == 1) {
    bool ok= true;
    if (fam == "cal" || fam == "cal*" || fam == "Bbb" || fam == "Bbb****")
      ok= ok && is_alpha (c) && upcase_all (c) == c;
    if (fam == "cal**" || fam == "Bbb*") ok= ok && is_alpha (c);
    if (!ok) return -1;

    if (fam == mfam) {
      if (fn[SUBFONT_MAIN]->supports (c)) {
        return sm->add_char (tuple ("main"), c);
      }
    }
    else {
      font cfn= closest_font (fam, variant, series, rshape, sz, dpi, 1);
      if (cfn->supports (c)) {
        tree key= tuple (fam, variant, series, rshape, "1");
        int  nr = sm->add_font (key, REWRITE_NONE);
        initialize_font (nr);
        return sm->add_char (key, c);
      }
    }

    if (fam == "roman" && range == "greek") {
      tree key= tuple ("greek", fam, variant, series, rshape);
      int  nr = sm->add_font (key, REWRITE_NONE);
      initialize_font (nr);
      return sm->add_char (key, c);
    }
    if (fam == "roman" && range == "latin") {
      tree key= tuple ("latin", fam, variant, series, rshape);
      int  nr = sm->add_font (key, REWRITE_NONE);
      initialize_font (nr);
      return sm->add_char (key, c);
    }
    if (is_math_family (fam)) {
      tree key= tuple ("math", fam, variant, series, rshape);
      int  nr = sm->add_font (key, REWRITE_MATH);
      initialize_font (nr);
      if (fn[nr]->supports (rewrite (c, REWRITE_MATH)))
        return sm->add_char (key, c);
    }
    if ((fam == "roman" || fam == "cyrillic") && N (c) > 1) {
      tree key= tuple ("cyrillic", fam, variant, series, rshape);
      int  nr = sm->add_font (key, REWRITE_CYRILLIC);
      initialize_font (nr);
      if (fn[nr]->supports (rewrite (c, REWRITE_CYRILLIC)))
        return sm->add_char (key, c);
    }
    if (c == "<#3000>") {
      tree key= tuple ("ignore");
      int  nr = sm->add_font (key, REWRITE_IGNORE);
      initialize_font (nr);
      return sm->add_char (key, c);
    }
    if (N (c) == 7 && starts (c, "<bbb-") && !occurs ("TeX Gyre", mfam)) {
      font cfn= closest_font (fam, variant, series, rshape, sz, dpi, 1);
      if (cfn->supports (c (N (c) - 2, N (c) - 1))) {
        array<string> lfn= logical_font (fam, variant, series, rshape);
        lfn              = apply_substitutions (lfn);
        array<string> pfn= search_font (lfn, 1);
        array<string> ch = font_database_characteristics (pfn[0], pfn[1]);
        double        rat= ((double) cfn->yx) / ((double) cfn->wfn);
        double        hw = rat * get_up_pen_width (ch);
        double        vw = rat * get_up_pen_height (ch);
        double        lw = ((double) cfn->wline) / ((double) cfn->wfn);
        hw               = max (hw, 0.25 * lw);
        vw               = max (vw, 0.25 * lw);
        tree key         = tuple ("poor-bbb", as_string (hw), as_string (vw));
        int  nr          = sm->add_font (key, REWRITE_POOR_BBB);
        initialize_font (nr);
        return sm->add_char (key, c);
      }
    }
    if (starts (c, "<it-") && ends (c, ">")) {
      tree key= tuple ("it");
      int  nr = sm->add_font (key, REWRITE_ITALIC);
      initialize_font (nr);
      return sm->add_char (key, c);
    }
    if (fam == mfam && !is_italic_font (mfam)) {
      array<string> emu_names= emu_font_names ();
      for (int i= 0; i < N (emu_names); i++)
        if (virtually_defined (c, emu_names[i])) {
          tree key= tuple ("emulate", emu_names[i]);
          int  nr = sm->add_font (key, REWRITE_NONE);
          initialize_font (nr);
          if (fn[nr]->supports (c)) return sm->add_char (key, c);
        }
    }
  }

  if (attempt > 1) {
    if (in_unicode_range (c, "cjk")) {
      range= "cjk";
    }
    int    a= attempt - 1;
    string v;
    if (range == "") v= variant;
    else if (v == "rm") v= range;
    else v= variant * "-" * range;
    font cfn= closest_font (fam, v, series, rshape, sz, dpi, a);
    // cout << "Trying " << c << " in " << cfn->res_name << "\n";
    if (cfn->supports (c)) {
      tree key= tuple (fam, v, series, rshape, as_string (a));
      int  nr = sm->add_font (key, REWRITE_NONE);
      initialize_font (nr);
      return sm->add_char (key, c);
    }
  }

  return -1;
}

bool
smart_font_rep::is_italic_prime (string c) {
  if (c != "'" && c != "`") return false;
  array<string> a= trimmed_tokenize (family, ",");
  string        s= "<#2B9>";
  if (c == "`") s= "<backprime>";
  for (int i= 0; i < N (a); i++)
    if (resolve (s, a[i], 1) >= 0) return false;
  return true;
}

extern bool has_poor_rubber;

int
smart_font_rep::resolve_rubber (string c, string fam, int attempt) {
  // cout << "Resolve rubber " << c << ", " << fam << ", " << attempt << LF;
  if (is_italic_font (mfam)) return -1;
  int l= search_forwards ("-", 0, c) + 1;
  int r= search_forwards ("-", l, c);
  if (r == -1) r= N (c) - 1;
  string ss  = c (l, r);
  string goal= ss;
  if (N (goal) != 1) goal= "<" * goal * ">";
  if (goal == "." || goal == "<nobracket>") {
    tree key= tuple ("ignore");
    int  nr = sm->add_font (key, REWRITE_IGNORE);
    initialize_font (nr);
    return sm->add_char (key, c);
  }
  if (has_poor_rubber) {
    if (goal == "<sqrt>") goal= "|"; // FIXME: better goal?
    if (goal == "<||>" || goal == "<interleave>") goal= "|";
    if (goal == "<langle>" || goal == "<rangle>" || goal == "<llangle>" ||
        goal == "<rrangle>")
      goal= "/";
    if (goal == "<lfloor>" || goal == "<lceil>" || goal == "<llbracket>" ||
        goal == "<dlfloor>" || goal == "<dlceil>" || goal == "<tlbracket>" ||
        goal == "<tlfloor>" || goal == "<tlceil>")
      goal= "[";
    if (goal == "<rfloor>" || goal == "<rceil>" || goal == "<rrbracket>" ||
        goal == "<drfloor>" || goal == "<drceil>" || goal == "<trbracket>" ||
        goal == "<trfloor>" || goal == "<trceil>")
      goal= "]";
  }
  int bnr= resolve (goal, main_family (fam), attempt);
  if (bnr >= 0 && bnr < N (fn) && !is_nil (fn[bnr])) {
    tree key= tuple ("rubber", as_string (bnr));
    int  nr = sm->add_font (key, REWRITE_NONE);
    initialize_font (nr);
    // cout << fn[nr]->res_name << " supports " << c << "? "
    //      << fn[nr]->supports (c) << LF;
    if (fn[nr]->supports (c)) return sm->add_char (key, c);
  }
  return -1;
}

font
smart_font_rep::make_rubber_font (font base) {
  if (contains (res_name, "mathlarge=") || contains (res_name, "mathrubber="))
    return this;
  else if (fn[SUBFONT_MAIN]->math_type == MATH_TYPE_OPENTYPE)
    return fn[SUBFONT_MAIN]->make_rubber_font (base);
  return font_rep::make_rubber_font (base);
}

static bool
use_italic_greek (array<string> a) {
  // FIXME: this is a very hacky fix for fonts such as
  // 'math basic-letters=Linux Libertine,math=TeX Gyre Termes,Linux Libertine'
  // which get rewritten as follows in math mode:
  // 'math basic-letters=Linux Libertine,TeX Gyre Termes,Linux Libertine'
  int count= 0;
  for (int i= 0; i < N (a); i++)
    if (!occurs ("=", a[i])) count++;
  return count <= 1;
}

int
smart_font_rep::resolve (string c) {
  if (DEBUG_VERBOSE) {
    debug_fonts << "Logical font of " << cork_to_utf8 (c) << " is "
                << this->res_name << LF;
    debug_fonts << "Main subfont of " << cork_to_utf8 (c) << " is "
                << fn[SUBFONT_MAIN]->res_name << LF;
  }
  array<string> a= trimmed_tokenize (family, ",");

  if (math_kind != 0) {
    string upc= substitute_upright (c);
    if (upc != "" && fn[SUBFONT_MAIN]->supports (upc)) {
      tree key= tuple ("up");
      int  nr = sm->add_font (key, REWRITE_UPRIGHT);
      initialize_font (nr);
      return sm->add_char (key, c);
    }
    string ugc= substitute_upright_greek (c);
    if (ugc != "" && fn[SUBFONT_MAIN]->supports (ugc)) {
      tree key= tuple ("upright-greek");
      int  nr = sm->add_font (key, REWRITE_UPRIGHT_GREEK);
      initialize_font (nr);
      return sm->add_char (key, c);
    }
    if (is_greek (c) && use_italic_greek (a) && shape != "mathupright") {
      string gc= substitute_italic_greek (c);
      if (gc != "" && fn[SUBFONT_MAIN]->supports (gc)) {
        tree key= tuple ("italic-greek");
        int  nr = sm->add_font (key, REWRITE_ITALIC_GREEK);
        initialize_font (nr);
        return sm->add_char (key, c);
      }
      // cout << "Found " << c << " in greek\n";
      return sm->add_char (tuple ("italic-math"), c);
    }
    if (c == "<imath>" || c == "<jmath>" || c == "<ell>") {
      // cout << "Found " << c << " in dotless\n";
      return sm->add_char (tuple ("italic-math"), c);
    }
    if (is_italic_prime (c)) {
      // cout << "Found " << c << " in italic prime\n";
      return sm->add_char (tuple ("italic-math"), c);
    }
    if (is_special (c) && (N (c) != 1 || !ends (variant, "-tt")) &&
        (!starts (c, "<big") || !starts (mfam, "TeX Gyre") ||
         !ends (mfam, " Math")) &&
        (!starts (c, "<big") ||
         (!occurs ("mathlarge=", family) && !occurs ("mathbigop=", family)))) {
      // cout << "Found " << c << " in special\n";
      return sm->add_char (tuple ("special"), c);
    }
    if (find_in_emu_bracket (c) && !is_italic_font (mfam)) {
      // cout << "Found " << c << " in virtual emu-bracket\n";
      return sm->add_char (tuple ("virtual", "emu-bracket"), c);
    }
    if (c == "<langle>" || c == "<rangle>")
      if (!is_italic_font (mfam) && fn[SUBFONT_MAIN]->supports ("/")) {
        // cout << "Found " << c << " in emu-bracket\n";
        return sm->add_char (tuple ("emu-bracket"), c);
      }
  }

  if (mfam == "roman" && shape == "mathupright" &&
      (variant == "rm" || variant == "ss" || variant == "tt") && N (c) == 1 &&
      (c[0] < 'A' || c[0] > 'Z') && (c[0] < 'a' || c[0] > 'z'))
    return sm->add_char (tuple ("italic-roman"), c);

  for (int attempt= 1; attempt <= FONT_ATTEMPTS; attempt++) {
    if (attempt > 1 && substitute_math_letter (c, math_kind) != "") break;
    for (int i= 0; i < N (a); i++) {
      int nr= resolve (c, a[i], attempt);
      if (nr >= 0) {
        // initialize_font (nr);
        // cout << "Found " << c << " in " << fn[nr]->res_name << "\n";
        return nr;
      }
      if (is_rubber (c)) {
        nr= resolve_rubber (c, a[i], attempt);
        if (nr >= 0) {
          // cout << "Found " << c << " in poor-rubber\n";
          return nr;
        }
      }
      if (starts (c, "<wide-")) {
        if (fn[SUBFONT_MAIN]->supports (c)) {
          // cout << "Found " << c << " in main\n";
          return sm->add_char (tuple ("main"), c);
        }
        if (series == "bold") {
          // cout << "Found " << c << " in poor-bold\n";
          return sm->add_char (tuple ("poor-bold"), c);
        }
      }
    }
  }

  string sf= substitute_math_letter (c, math_kind);
  if (sf != "") {
    // cout << "Found " << c << " in " << sf << " (math-letter)\n";
    return sm->add_char (tuple (sf), c);
  }

  string virt= find_in_virtual (c);
  if (math_kind != 0 && !unicode_provides (c) && virt == "")
    if (!starts (c, "<left-") && !starts (c, "<right-") &&
        !starts (c, "<mid-")) {
      // cout << "Found " << c << " in other\n";
      return sm->add_char (tuple ("other"), c);
    }

  if (virt != "") {
    // cout << "Found " << c << " in " << virt << "\n";
    return sm->add_char (tuple ("virtual", virt), c);
  }

  // cout << "Error " << c << "\n";
  return sm->add_char (tuple ("error"), c);
}

void
smart_font_rep::initialize_font (int nr) {
  if (N (fn) <= nr) fn->resize (nr + 1);
  if (!is_nil (fn[nr])) return;
  array<string> a= tuple_as_array (sm->fn_spec[nr]);
  if (a[0] == "math")
    fn[nr]= adjust_subfont (get_math_font (a[1], a[2], a[3], a[4]));
  else if (a[0] == "cyrillic")
    fn[nr]= adjust_subfont (get_cyrillic_font (a[1], a[2], a[3], a[4]));
  else if (a[0] == "greek")
    fn[nr]= adjust_subfont (get_greek_font (a[1], a[2], a[3], a[4]));
  else if (a[0] == "latin")
    fn[nr]= adjust_subfont (get_latin_font (a[1], a[2], a[3], a[4]));
  else if (a[0] == "subfont")
    fn[nr]= smart_font_bis (a[1], variant, series, shape, sz, hdpi, dpi);
  else if (a[0] == "special")
    fn[nr]= smart_font_bis (family, variant, series, "right", sz, hdpi, dpi);
  else if (a[0] == "emu-bracket")
    fn[nr]= virtual_font (this, "emu-bracket", sz, hdpi, dpi, false);
  else if (a[0] == "other") {
    int nvdpi= adjusted_dpi ("roman", variant, series, "mathitalic", 1);
    int nhdpi= (hdpi * nvdpi + (dpi >> 1)) / dpi;
    fn[nr]= smart_font_bis ("roman", variant, series, "mathitalic", sz, nhdpi,
                            nvdpi);
  }
  else if (a[0] == "bold-math")
    fn[nr]= smart_font_bis (family, variant, "bold", "right", sz, hdpi, dpi);
  else if (a[0] == "fast-italic")
    fn[nr]= smart_font_bis (family, variant, series, "italic", sz, hdpi, dpi);
  else if (a[0] == "italic-math")
    fn[nr]= smart_font_bis (family, variant, series, "italic", sz, hdpi, dpi);
  else if (a[0] == "italic-roman")
    fn[nr]=
        smart_font_bis (family, variant, series, "mathitalic", sz, hdpi, dpi);
  else if (a[0] == "bold-italic-math")
    fn[nr]= smart_font_bis (family, variant, "bold", "italic", sz, hdpi, dpi);
  else if (a[0] == "italic-greek") fn[nr]= fn[SUBFONT_MAIN];
  else if (a[0] == "upright-greek") fn[nr]= fn[SUBFONT_MAIN];
  else if (a[0] == "up") fn[nr]= fn[SUBFONT_MAIN];
  else if (a[0] == "it")
    fn[nr]= smart_font_bis (family, variant, series, "italic", sz, hdpi, dpi);
  else if (a[0] == "tt")
    fn[nr]= smart_font_bis (family, "tt", series, "right", sz, hdpi, dpi);
  else if (a[0] == "ss")
    fn[nr]= smart_font_bis (family, "ss", series, "right", sz, hdpi, dpi);
  else if (a[0] == "bold-ss")
    fn[nr]= smart_font_bis (family, "ss", "bold", "right", sz, hdpi, dpi);
  else if (a[0] == "italic-ss")
    fn[nr]= smart_font_bis (family, "ss", series, "italic", sz, hdpi, dpi);
  else if (a[0] == "bold-italic-ss")
    fn[nr]= smart_font_bis (family, "ss", "bold", "italic", sz, hdpi, dpi);
  else if (a[0] == "cal" && N (a) == 1)
    fn[nr]= smart_font_bis (family, "calligraphic", series, "italic", sz, hdpi,
                            dpi);
  else if (a[0] == "bold-cal")
    fn[nr]= smart_font_bis (family, "calligraphic", "bold", "italic", sz, hdpi,
                            dpi);
  else if (a[0] == "frak")
    fn[nr]= smart_font_bis (family, "gothic", series, "right", sz, hdpi, dpi);
  else if (a[0] == "bold-frak")
    fn[nr]= smart_font_bis (family, "gothic", "bold", "right", sz, hdpi, dpi);
  else if (a[0] == "bbb" && N (a) == 1)
    fn[nr]= smart_font_bis (family, "outline", series, "right", sz, hdpi, dpi);
  else if (a[0] == "virtual")
    fn[nr]= virtual_font (this, a[1], sz, hdpi, dpi, false);
  else if (a[0] == "emulate") {
    font vfn= fn[SUBFONT_MAIN];
    if (a[1] != "emu-fundamental")
      vfn= virtual_font (vfn, "emu-fundamental", sz, hdpi, dpi, true);
    fn[nr]= virtual_font (vfn, a[1], sz, hdpi, dpi, true);
  }
  else if (a[0] == "poor-bold" && N (a) == 1) {
    font sfn= smart_font_bis (family, variant, "medium", shape, sz, hdpi, dpi);
    double emb= 5.0 / 3.0;
    double fat= ((emb - 1.0) * sfn->wline) / sfn->wfn;
    fn[nr]    = poor_bold_font (sfn, fat, fat);
  }
  else if (a[0] == "poor-bbb" && N (a) == 3) {
    double pw= as_double (a[1]);
    double ph= as_double (a[2]);
    font sfn = smart_font_bis (family, variant, series, "right", sz, hdpi, dpi);
    fn[nr]   = poor_bbb_font (sfn, pw, ph, 1.5 * pw);
  }
  else if (a[0] == "rubber" && N (a) == 2 && is_int (a[1])) {
    initialize_font (as_int (a[1]));
    fn[nr]= adjust_subfont (rubber_font (fn[as_int (a[1])]));
    // fn[nr]= adjust_subfont (rubber_unicode_font (fn[as_int (a[1])]));
  }
  else if (a[0] == "ignore") fn[nr]= fn[SUBFONT_MAIN];
  else {
    font cfn= closest_font (a[0], a[1], a[2], a[3], sz, dpi, as_int (a[4]));
    fn[nr]  = adjust_subfont (cfn);
  }
  // cout << "Font " << nr << ", " << a << " -> " << fn[nr]->res_name << "\n";
  if (fn[nr]->res_name == res_name) {
    failed_error << "Font " << nr << ", " << a << " -> " << fn[nr]->res_name
                 << "\n";
    TM_FAILED ("substitution font loop detected");
  }
}

static int
get_ex (string family, string variant, string series, string shape,
        int attempt) {
  array<string> lfn= logical_font (family, variant, series, shape);
  array<string> pfn= search_font (lfn, attempt);
  array<string> chs= font_database_characteristics (pfn[0], pfn[1]);
  string        ex = find_attribute_value (chs, "ex");
  if (ex == "") return 0;
  else return as_int (ex);
}

int
smart_font_rep::adjusted_dpi (string fam, string var, string ser, string sh,
                              int attempt) {
  int    ex1 = get_ex (mfam, variant, series, rshape, 1);
  int    ex2 = get_ex (fam, var, ser, sh, attempt);
  double zoom= 1.0;
  if (ex1 != 0 && ex2 != 0) zoom= ((double) ex1) / ((double) ex2);
  if (zoom > 0.975 && zoom < 1.025) zoom= 1;
  if (starts (fam, "TeX Gyre Cursor") && starts (mfam, "TeX Gyre Pagella"))
    zoom*= 0.9; // FIXME: temporary hack for new manual
  // cout << mfam << ", " << fam << " -> "
  //      << ex1 << ", " << ex2 << ", " << zoom << "\n";
  return (int) tm_round (dpi * zoom);
}

/******************************************************************************
 * Getting extents and drawing strings
 ******************************************************************************/

static string empty_string ("");

bool
smart_font_rep::supports (string c) {
  (void) c;
  return true;
}

void
smart_font_rep::get_extents (string s, metric& ex) {
  // cout << "Extents of " << s << " for " << res_name << "\n";
  int i= 0, n= N (s);
  if (n == 0) fn[0]->get_extents (empty_string, ex);
  else {
    int    nr;
    string r= s;
    metric ey;
    while (true) {
      advance (s, i, r, nr);
      if (nr >= 0) {
        // cout << "From " << nr << " -> " << sm->fn_spec[nr] << "\n";
        fn[nr]->get_extents (r, ex);
        break;
      }
      if (i >= n) {
        fn[0]->get_extents (empty_string, ex);
        break;
      }
    }
    while (i < n) {
      advance (s, i, r, nr);
      if (nr >= 0) {
        // cout << "From " << nr << " -> " << sm->fn_spec[nr] << "\n";
        fn[nr]->get_extents (r, ey);
        ex->y1= min (ex->y1, ey->y1);
        ex->y2= max (ex->y2, ey->y2);
        ex->x3= min (ex->x3, ex->x2 + ey->x3);
        ex->y3= min (ex->y3, ey->y3);
        ex->x4= max (ex->x4, ex->x2 + ey->x4);
        ex->y4= max (ex->y4, ey->y4);
        ex->x2+= ey->x2;
      }
    }
  }
}

void
smart_font_rep::get_xpositions (string s, SI* xpos) {
  SI  x= 0;
  int i= 0, n= N (s);
  xpos[0]= x;
  while (i < n) {
    int    nr;
    string r    = s;
    int    start= i;
    advance (s, i, r, nr);
    if (nr >= 0) {
      if (r == s (start, i)) {
        fn[nr]->get_xpositions (r, xpos + start);
        for (int j= 0; j <= N (r); j++)
          xpos[start + j]+= x;
      }
      else {
        STACK_NEW_ARRAY (tmp, SI, N (r) + 1);
        fn[nr]->get_xpositions (r, tmp);
        for (int j= start; j < i; j++)
          xpos[j]= x;
        xpos[i]= x + tmp[N (r)];
        STACK_DELETE_ARRAY (tmp);
      }
      x= xpos[i];
    }
    else
      for (int j= start; j <= i; j++)
        xpos[j]= x;
  }
}

void
smart_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  SI  x= 0;
  int i= 0, n= N (s);
  xpos[0]= x;
  while (i < n) {
    int    nr;
    string r    = s;
    int    start= i;
    advance (s, i, r, nr);
    if (nr >= 0) {
      if (r == s (start, i)) {
        fn[nr]->get_xpositions (r, xpos + start, xk);
        for (int j= 0; j <= N (r); j++)
          xpos[start + j]+= x;
      }
      else {
        STACK_NEW_ARRAY (tmp, SI, N (r) + 1);
        fn[nr]->get_xpositions (r, tmp, xk);
        for (int j= start; j < i; j++)
          xpos[j]= x;
        xpos[i]= x + tmp[N (r)];
        STACK_DELETE_ARRAY (tmp);
      }
      x= xpos[i];
    }
    else
      for (int j= start; j <= i; j++)
        xpos[j]= x;
  }
}

void
smart_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  int i= 0, n= N (s);
  while (i < n) {
    int    nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->draw_fixed (ren, r, x, y);
      if (i < n) {
        fn[nr]->get_extents (r, ey);
        x+= ey->x2;
      }
    }
  }
}

void
smart_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  int i= 0, n= N (s);
  while (i < n) {
    int    nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->draw_fixed (ren, r, x, y, xk);
      if (i < n) {
        fn[nr]->get_extents (r, ey, xk);
        x+= ey->x2;
      }
    }
  }
}

font
smart_font_rep::magnify (double zoomx, double zoomy) {
  // if (zoomx != zoomy) return poor_magnify (zoomx, zoomy);
  return smart_font_bis (family, variant, series, shape, sz,
                         (int) tm_round (hdpi * zoomx),
                         (int) tm_round (dpi * zoomy));
}

/******************************************************************************
 * Other routines for fonts
 ******************************************************************************/

void
smart_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  if (pos >= N (s)) return;
  int    i= pos, nr;
  string r= s;
  advance (s, i, r, nr);
  if (nr < 0) {
    tm_char_forwards (s, pos);
    return;
  }
  int pos2= 0;
  fn[nr]->advance_glyph (r, pos2, ligf);
  if (pos + pos2 <= N (s) && r (0, pos2) == s (pos, pos + pos2) && pos2 > 0)
    pos+= pos2;
  else tm_char_forwards (s, pos);
}

glyph
smart_font_rep::get_glyph (string s) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_glyph (s);
  string r= s;
  advance (s, i, r, nr);
  if (nr < 0) return glyph ();
  return fn[nr]->get_glyph (r);
}

int
smart_font_rep::index_glyph (string s, font_metric& fnm, font_glyphs& fng) {
  int i= 0, n= N (s), nr;
  if (n == 0) return -1;
  string r= s;
  advance (s, i, r, nr);
  if (nr < 0 || N (r) == 0) return -1;
  return fn[nr]->index_glyph (r, fnm, fng);
}

double
smart_font_rep::get_left_slope (string s) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_left_slope (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_slope (r);
}

double
smart_font_rep::get_right_slope (string s) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_right_slope (s);
  string r= s;
  while (i < n)
    advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_slope (r);
}

SI
smart_font_rep::get_left_correction (string s) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_left_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_correction (r);
}

SI
smart_font_rep::get_right_correction (string s) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_right_correction (s);
  string r= s;
  while (i < n)
    advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_correction (r);
}

SI
smart_font_rep::get_lsub_correction (string s) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_lsub_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_lsub_correction (r);
}

SI
smart_font_rep::get_lsup_correction (string s) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_lsup_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_lsup_correction (r);
}

SI
smart_font_rep::get_rsub_correction (string s) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_rsub_correction (s);
  string r= s;
  while (i < n)
    advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_rsub_correction (r);
}

SI
smart_font_rep::get_rsup_correction (string s) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_rsup_correction (s);
  string r= s;
  while (i < n)
    advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_rsup_correction (r);
}

SI
smart_font_rep::get_wide_correction (string s, int mode) {
  int i= 0, n= N (s), nr;
  if (n == 0) return fn[0]->get_wide_correction (s, mode);
  string r= s;
  while (i < n)
    advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_wide_correction (r, mode);
}

/******************************************************************************
 * User interface
 ******************************************************************************/

font
smart_font_bis (string family, string variant, string series, string shape,
                int sz, int hdpi, int vdpi) {
  string name= family * "-" * variant * "-" * series * "-" * shape * "-" *
               as_string (sz) * "-" * as_string (vdpi) * "-smart";
  if (hdpi != vdpi)
    name= family * "-" * variant * "-" * series * "-" * shape * "-" *
          as_string (sz) * "-" * as_string (hdpi) * "-" * as_string (vdpi) *
          "-smart";
  if (font::instances->contains (name)) return font (name);
  if (starts (family, "tc")) {
    // FIXME: temporary hack for symbols from std-symbol.ts
    font fn= find_font (family, variant, series, shape, sz, vdpi);
    if (hdpi == vdpi) return fn;
    return fn->magnify (((double) hdpi) / ((double) vdpi), 1.0);
  }
  if (starts (family, "sys-")) {
    if (family == "sys-chinese") {
      string name= default_chinese_font_name ();
      family     = "cjk=" * name * ",roman";
    }
    if (family == "sys-japanese") {
      string name= default_japanese_font_name ();
      family     = "cjk=" * name * ",roman";
    }
    if (family == "sys-korean") {
      string name= default_korean_font_name ();
      family     = "cjk=" * name * ",roman";
    }
  }
  family= tex_gyre_fix (family, series, shape);
  family= kepler_fix (family, series, shape);
  // family= stix_fix (family, series, shape);
  family   = math_fix (family, series, shape);
  string sh= shape;
  if (shape == "mathitalic" || shape == "mathshape") sh= "right";
  string mfam   = main_family (family);
  font   base_fn= closest_font (mfam, variant, series, sh, sz, vdpi);
  if (is_nil (base_fn)) return font ();
  font sec_fn= closest_font ("roman", "ss", "medium", "right", sz, vdpi);
  font err_fn= error_font (sec_fn);
  return make (font, name,
               tm_new<smart_font_rep> (name, base_fn, err_fn, family, variant,
                                       series, shape, sz, hdpi, vdpi));
}

font
smart_font (string family, string variant, string series, string shape, int sz,
            int dpi) {
  if (variant == "rm")
    return smart_font_bis (family, variant, series, shape, sz, dpi, dpi);
  array<string> lfn1= logical_font (family, "rm", series, shape);
  array<string> lfn2= logical_font (family, variant, series, shape);
  array<string> pfn1= search_font (lfn1, 1);
  array<string> pfn2= search_font (lfn2, 1);
  if (N (pfn1) > 0 && N (pfn2) > 0 && pfn1[0] == pfn2[0])
    return smart_font_bis (family, variant, series, shape, sz, dpi, dpi);
  font   fn1 = smart_font_bis (family, "rm", series, shape, sz, dpi, dpi);
  font   fn2 = smart_font_bis (family, variant, series, shape, sz, dpi, dpi);
  double zoom= ((double) fn1->yx) / max (((double) fn2->yx), 1.0);
  if (fn1->yx < PIXEL || fn2->yx < PIXEL) zoom= 1.0;
  if (zoom > 0.975 && zoom < 1.025) return fn2;
  return fn2->magnify (zoom);
}

font
math_smart_font (string family, string variant, string series, string shape,
                 string tfam, string tvar, string tser, string tsh, int sz,
                 int dpi) {
  if (tfam == "roman" || starts (tfam, "sys-")) {
    tfam= family;
  }
  if (variant != "mr") {
    if (variant == "ms") tvar= "ss";
    if (variant == "mt") tvar= "tt";
  }
  if (shape == "right") tsh= "mathupright";
  return smart_font (tfam, tvar, tser, tsh, sz, dpi);
}

font
prog_smart_font (string family, string variant, string series, string shape,
                 string tfam, string tvar, string tser, string tsh, int sz,
                 int dpi) {
  if (tfam == "roman") {
    tfam= family;
  }
  return smart_font (tfam, tvar, tser, tsh, sz, dpi);
}

static string
get_string_parameter (string val, int i, string def) {
  array<string> a= trimmed_tokenize (val, ";");
  if (i < N (a)) return a[i];
  return def;
}

static double
get_double_parameter (string val, int i, double def) {
  array<string> a= trimmed_tokenize (val, ";");
  if (i < N (a) && is_double (a[i])) return as_double (a[i]);
  return def;
}

static void
get_length_parameter (string val, int i, double& quan, string& unit) {
  array<string> a= trimmed_tokenize (val, ";");
  if (i < N (a)) {
    string s= a[i];
    int    k= N (s);
    while (k > 0 && is_alpha (s[k - 1]))
      k--;
    unit= s (k, N (s));
    quan= 0.0;
    if (is_double (s (0, k))) quan= as_double (s (0, k));
  }
}

font
apply_effects (font fn, string effects) {
  if (N (effects) == 0) return fn;
  array<string> a= trimmed_tokenize (effects, ",");
  for (int i= 0; i < N (a); i++) {
    array<string> b= trimmed_tokenize (a[i], "=");
    if (N (b) == 2) {
      if (b[0] == "bold" && is_double (b[1])) {
        double emb= as_double (b[1]);
        if (emb < 1.0) emb= 1.0;
        if (emb > 5.0) emb= 5.0;
        double fat= ((emb - 1.0) * fn->wline) / fn->wfn;
        fn        = poor_bold_font (fn, fat, fat);
      }
      else if (b[0] == "bbb" && is_double (b[1])) {
        double emb= as_double (b[1]);
        if (emb < 1.0) emb= 1.0;
        if (emb > 5.0) emb= 5.0;
        double penw= ((double) fn->wline) / ((double) fn->wfn);
        double penh= ((double) fn->wline) / ((double) fn->wfn);
        double fat = ((emb - 1.0) * fn->wline) / fn->wfn;
        fn         = poor_bbb_font (fn, penw, penh, fat);
      }
      else if (b[0] == "slant" && is_double (b[1])) {
        double slant= as_double (b[1]);
        if (slant < -2.0) slant= -2.0;
        if (slant > 2.0) slant= 2.0;
        fn= poor_italic_font (fn, slant);
      }
      else if (b[0] == "hmagnify" && is_double (b[1])) {
        double xmag= as_double (b[1]);
        if (xmag < 0.1) xmag= 0.1;
        if (xmag > 10.0) xmag= 10.0;
        fn= poor_stretched_font (fn, xmag, 1.0);
      }
      else if (b[0] == "vmagnify" && is_double (b[1])) {
        double ymag= as_double (b[1]);
        if (ymag < 0.1) ymag= 0.1;
        if (ymag > 10.0) ymag= 10.0;
        fn= poor_stretched_font (fn, 1.0, ymag);
      }
      else if (b[0] == "hextended" && is_double (b[1])) {
        double xf= as_double (b[1]);
        if (xf < 0.1) xf= 0.1;
        if (xf > 10.0) xf= 10.0;
        fn= poor_extended_font (fn, xf);
      }
      /*
      else if (b[0] == "vextended" && is_double (b[1])) {
        double yf= as_double (b[1]);
        if (yf < 0.1) yf= 0.1;
        if (yf > 10.0) yf= 10.0;
        fn= poor_vextended_font (fn, yf);
      }
      */
      else if (b[0] == "mono" && is_double (b[1])) {
        double Mmag= as_double (b[1]);
        if (Mmag < 0.1) Mmag= 0.1;
        if (Mmag > 10.0) Mmag= 10.0;
        fn= poor_mono_font (fn, Mmag, Mmag);
      }
      else if (b[0] == "degraded") {
        double threshold= get_double_parameter (b[1], 0, 0.666);
        double freq     = get_double_parameter (b[1], 1, 1.0);
        if (threshold < 0.01) threshold= 0.01;
        if (threshold > 0.99) threshold= 0.99;
        if (freq < 0.10) freq= 0.10;
        if (freq > 10.0) freq= 10.0;
        tree kind= tuple ("degraded", as_string (threshold), as_string (freq));
        fn       = poor_distorted_font (fn, kind);
      }
      else if (b[0] == "distorted") {
        double strength= get_double_parameter (b[1], 0, 1.0);
        double freq    = get_double_parameter (b[1], 1, 1.0);
        if (strength < 0.1) strength= 0.1;
        if (strength > 9.9) strength= 9.9;
        if (freq < 0.10) freq= 0.10;
        if (freq > 10.0) freq= 10.0;
        tree kind= tuple ("distorted", as_string (strength), as_string (freq));
        fn       = poor_distorted_font (fn, kind);
      }
      else if (b[0] == "gnawed") {
        double strength= get_double_parameter (b[1], 0, 1.0);
        double freq    = get_double_parameter (b[1], 1, 1.0);
        if (strength < 0.1) strength= 0.1;
        if (strength > 9.9) strength= 9.9;
        if (freq < 0.10) freq= 0.10;
        if (freq > 10.0) freq= 10.0;
        tree kind= tuple ("gnawed", as_string (strength), as_string (freq));
        fn       = poor_distorted_font (fn, kind);
      }
      else if (b[0] == "blurred") {
        double rad_val = 1.0;
        string rad_unit= "pt";
        get_length_parameter (b[1], 0, rad_val, rad_unit);
        if (rad_unit == "pt") rad_val= rad_val / fn->size;
        if (rad_val < 0.01) rad_val= 0.01;
        if (rad_val > 1.00) rad_val= 1.00;
        tree kind= tuple ("blurred", as_string (rad_val));
        fn       = poor_effected_font (fn, kind);
      }
      else if (b[0] == "enhanced") {
        double rad_val = 1.0;
        string rad_unit= "pt";
        get_length_parameter (b[1], 0, rad_val, rad_unit);
        string shadow= get_string_parameter (b[1], 1, "black");
        string sunny = get_string_parameter (b[1], 2, "white");
        if (rad_unit == "pt") rad_val= rad_val / fn->size;
        if (rad_val < 0.01) rad_val= 0.01;
        if (rad_val > 1.00) rad_val= 1.00;
        double      m       = sqrt (0.5);
        tree        kind1   = tuple ("blurred", as_string (rad_val),
                                     as_string (m * rad_val), as_string (-m * rad_val));
        font        blurred1= poor_effected_font (fn, kind1);
        font        fn1     = recolored_font (blurred1, shadow);
        tree        kind2   = tuple ("blurred", as_string (rad_val),
                                     as_string (-m * rad_val), as_string (m * rad_val));
        font        blurred2= poor_effected_font (fn, kind2);
        font        fn2     = recolored_font (blurred2, sunny);
        array<font> a;
        a << fn1 << fn2 << fn;
        fn= superposed_font (a, 2);
      }
    }
  }
  return fn;
}
