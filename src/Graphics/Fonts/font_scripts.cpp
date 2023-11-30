
/******************************************************************************
 * MODULE     : font_scripts.cpp
 * DESCRIPTION: microtypographic script positioning
 * COPYRIGHT  : (C) 2017  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "analyze.hpp"
#include "font.hpp"

/******************************************************************************
 * Helper routines for script correction tables
 ******************************************************************************/

int get_utf8_code (string c);

void
adjust_char (hashmap<string, double>& t, string c, double delta) {
  t (c)+= delta;
  int code= get_utf8_code (c);
  if (code >= 0x3b1 && code <= 0x3f5) {
    if (code <= 0x3c9) code+= 0x1d6fc - 0x3b1;
    else if (code == 0x3f5) code= 0x1d716; // varepsilon
    else if (code == 0x3d1) code= 0x1d717; // vartheta
    else if (code == 0x3f0) code= 0x1d718; // varkappa
    else if (code == 0x3d5) code= 0x1d719; // phi
    else if (code == 0x3f1) code= 0x1d71a; // varrho
    else if (code == 0x3d6) code= 0x1d71b; // varpi
    else code= 0;
    if (code != 0) {
      string nc= "<#" * upcase_all (as_hexadecimal (code)) * ">";
      if (nc != c) t (nc)+= delta;
    }
  }
}

void
adjust_pair (hashmap<string, double>& t, string c, double delta) {
  adjust_char (t, c, delta);
  if (N (c) == 1) c= "<b-" * c * ">";
  else c= "<b-" * c (1, N (c));
  adjust_char (t, c, delta);
}

void
adjust_integral_sub (hashmap<string, double>& t, string c, string suf,
                     double delta) {
  adjust_char (t, "<big-" * c * "-" * suf * ">", delta);
  adjust_char (t, "<big-up" * c * "-" * suf * ">", delta);
  adjust_char (t, "<big-" * c * "lim-" * suf * ">", delta);
  adjust_char (t, "<big-up" * c * "lim-" * suf * ">", delta);
  if (suf == "1") {
    adjust_char (t, "<" * c * ">", delta);
    adjust_char (t, "<up" * c * ">", delta);
    adjust_char (t, "<" * c * "lim>", delta);
    adjust_char (t, "<up" * c * "lim>", delta);
  }
  if (suf == "2") {
    adjust_char (t, "<big-" * c * ">", delta);
    adjust_char (t, "<big-up" * c * ">", delta);
    adjust_char (t, "<big-" * c * "lim>", delta);
    adjust_char (t, "<big-up" * c * "lim>", delta);
  }
}

void
adjust_integral (hashmap<string, double>& t, string suf, double delta) {
  adjust_integral_sub (t, "int", suf, delta);
  adjust_integral_sub (t, "iint", suf, delta);
  adjust_integral_sub (t, "iiint", suf, delta);
  adjust_integral_sub (t, "iiiint", suf, delta);
  adjust_integral_sub (t, "idotsint", suf, delta);
}

void
adjust_contour_integral (hashmap<string, double>& t, string suf, double delta) {
  adjust_integral_sub (t, "oint", suf, delta);
  adjust_integral_sub (t, "oiint", suf, delta);
  adjust_integral_sub (t, "oiiint", suf, delta);
}

/******************************************************************************
 * Standard corrections
 ******************************************************************************/

void
lsub_adjust_std (hashmap<string, double>& t) {
  adjust_pair (t, "V", 0.05);
  adjust_pair (t, "W", 0.05);
  adjust_pair (t, "Y", 0.05);
  adjust_pair (t, "<Upsilon>", 0.05);
  adjust_pair (t, "<Phi>", 0.05);
  adjust_pair (t, "<Psi>", 0.05);
  adjust_pair (t, "<nabla>", 0.05);
  adjust_pair (t, "<vee>", 0.1);
  adjust_pair (t, "<curlyvee>", 0.15);
}

void
lsup_adjust_std (hashmap<string, double>& t) {
  adjust_pair (t, "/", 0.07);
  adjust_pair (t, "a", 0.05);
  adjust_pair (t, "c", 0.05);
  adjust_pair (t, "d", 0.05);
  adjust_pair (t, "e", 0.05);
  adjust_pair (t, "g", 0.05);
  adjust_pair (t, "o", 0.05);
  adjust_pair (t, "q", 0.05);
  adjust_pair (t, "A", 0.05);
  adjust_pair (t, "<alpha>", 0.05);
  adjust_pair (t, "<beta>", 0.05);
  adjust_pair (t, "<delta>", 0.05);
  adjust_pair (t, "<iota>", 0.05);
  adjust_pair (t, "<lambda>", 0.05);
  adjust_pair (t, "<omicron>", 0.05);
  adjust_pair (t, "<rho>", 0.05);
  adjust_pair (t, "<sigma>", 0.05);
  adjust_pair (t, "<phi>", 0.05);
  adjust_pair (t, "<epsilon>", 0.05);
  adjust_pair (t, "<varpi>", 0.05);
  adjust_pair (t, "<varsigma>", 0.05);
  adjust_pair (t, "<varphi>", 0.05);
  adjust_pair (t, "<varrho>", 0.05);
  adjust_pair (t, "<Alpha>", 0.05);
  adjust_pair (t, "<Delta>", 0.05);
  adjust_pair (t, "<Lambda>", 0.05);
  adjust_pair (t, "<wedge>", 0.05);
  adjust_pair (t, "<curlywedge>", 0.1);
}

void
rsub_adjust_std (hashmap<string, double>& t) {
  adjust_pair (t, "1", -0.02);
  adjust_pair (t, "J", -0.01);
  adjust_pair (t, "P", -0.02);
  adjust_pair (t, "T", -0.03);
  adjust_pair (t, "V", -0.05);
  adjust_pair (t, "W", -0.05);
  adjust_pair (t, "Y", -0.07);
  adjust_pair (t, "<up-T>", -0.05);
  adjust_pair (t, "<up-V>", -0.05);
  adjust_pair (t, "<up-W>", -0.05);
  adjust_pair (t, "<up-Y>", -0.05);
  adjust_pair (t, "<Gamma>", -0.1);
  adjust_pair (t, "<Tau>", -0.05);
  adjust_pair (t, "<Upsilon>", -0.05);
  adjust_pair (t, "<Psi>", -0.03);
  adjust_pair (t, "<gamma>", -0.02);
  adjust_pair (t, "<nabla>", -0.1);
}

void
rsup_adjust_std (hashmap<string, double>& t) {
  adjust_pair (t, "1", -0.05);
  adjust_pair (t, "A", -0.05);
  adjust_pair (t, "L", -0.05);
  adjust_pair (t, "<up-A>", -0.05);
  adjust_pair (t, "<up-L>", -0.05);
  adjust_pair (t, "<Alpha>", -0.1);
  adjust_pair (t, "<Delta>", -0.1);
  adjust_pair (t, "<Lambda>", -0.1);
  adjust_pair (t, "<bbb-A>", -0.05);
  adjust_pair (t, "<bbb-L>", -0.05);
}

/******************************************************************************
 * Adjusting arrow subscripts and superscripts
 ******************************************************************************/

void
adjust_arrow (hashmap<string, double>& t, string c, double delta) {
  adjust_char (t, c, delta);
  if (N (c) <= 3 || !starts (c, "<") || !ends (c, ">")) return;
  if (is_locase (c[1])) c= "<long" * c (1, N (c));
  else c= "<Long" * locase_all (c (1, 2)) * c (2, N (c));
  adjust_char (t, c, delta);
}

void
right_adjust_arrow (hashmap<string, double>& t, double delta) {
  adjust_arrow (t, "<rightarrow>", delta);
  adjust_arrow (t, "<leftrightarrow>", delta);
  adjust_arrow (t, "<mapsto>", delta);
  adjust_arrow (t, "<twoheadrightarrow>", delta);
  adjust_arrow (t, "<rightarrowtail>", delta);
  adjust_arrow (t, "<hookrightarrow>", delta);
  adjust_arrow (t, "<looparrowright>", delta);
  adjust_arrow (t, "<rightharpoondown>", delta);
  adjust_arrow (t, "<rightharpoonup>", delta);
  adjust_arrow (t, "<Rightarrow>", delta);
  adjust_arrow (t, "<Leftrightarrow>", delta);
}

/******************************************************************************
 * Guessing further adjustments
 ******************************************************************************/

void
lsub_adjust_guessed (hashmap<string, double>& t) {
  (void) t;
}

void
lsup_adjust_guessed (hashmap<string, double>& t) {
  adjust_integral (t, "1", 0.1);
  adjust_integral (t, "2", 0.1);
}

void
rsub_adjust_guessed (hashmap<string, double>& t) {
  adjust_pair (t, "/", -0.05);
  adjust_pair (t, "7", -0.1);
  adjust_pair (t, "T", -0.05);
  adjust_pair (t, "V", -0.05);
  adjust_pair (t, "W", -0.05);
  adjust_pair (t, "Y", -0.05);
  adjust_pair (t, "<Gamma>", -0.03);
  adjust_pair (t, "<Rho>", -0.05);
  adjust_pair (t, "<Tau>", -0.03);
  adjust_pair (t, "<Upsilon>", -0.03);
  adjust_pair (t, "<Psi>", -0.03);
  adjust_pair (t, "<bbb-F>", -0.05);
  adjust_pair (t, "<bbb-P>", -0.05);
  adjust_pair (t, "<bbb-T>", -0.05);
  adjust_pair (t, "<bbb-V>", -0.05);
  adjust_pair (t, "<bbb-W>", -0.05);
  adjust_pair (t, "<bbb-Y>", -0.05);
  adjust_pair (t, "<bbb-v>", -0.05);
  adjust_pair (t, "<bbb-w>", -0.05);
  adjust_pair (t, "<bbb-y>", -0.05);
  adjust_integral (t, "1", -0.1);
  adjust_integral (t, "2", -0.1);
}

void
rsup_adjust_guessed (hashmap<string, double>& t) {
  adjust_pair (t, "\\", -0.05);
  adjust_pair (t, "L", -0.05);
  adjust_pair (t, "<bbb-A>", -0.1);
  adjust_pair (t, "<bbb-L>", -0.1);
}

/******************************************************************************
 * Interface
 ******************************************************************************/

static hashmap<string, double> lsub_guessed (0.0);
static hashmap<string, double> lsup_guessed (0.0);
static hashmap<string, double> rsub_guessed (0.0);
static hashmap<string, double> rsup_guessed (0.0);

hashmap<string, double>
lsub_guessed_table () {
  if (N (lsub_guessed) == 0) {
    lsub_adjust_std (lsub_guessed);
    lsub_adjust_guessed (lsub_guessed);
  }
  return lsub_guessed;
}

hashmap<string, double>
lsup_guessed_table () {
  if (N (lsup_guessed) == 0) {
    lsup_adjust_std (lsup_guessed);
    lsup_adjust_guessed (lsup_guessed);
  }
  return lsup_guessed;
}

hashmap<string, double>
rsub_guessed_table () {
  if (N (rsub_guessed) == 0) {
    rsub_adjust_std (rsub_guessed);
    rsub_adjust_guessed (rsub_guessed);
  }
  return rsub_guessed;
}

hashmap<string, double>
rsup_guessed_table () {
  if (N (rsup_guessed) == 0) {
    rsup_adjust_std (rsup_guessed);
    rsup_adjust_guessed (rsup_guessed);
  }
  return rsup_guessed;
}
