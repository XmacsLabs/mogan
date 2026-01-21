
/******************************************************************************
 * MODULE     : verb_language.cpp
 * DESCRIPTION: the "verbatim" language
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "observers.hpp"
#include "packrat.hpp"
#include "scheme.hpp"

verb_language_rep::verb_language_rep (string name) : language_rep (name) {
  hl_lan= packrat_abbreviation (res_name, "Main");
}

inline static bool
is_sep_char (char c) {
  return c == '-' || c == '/' || c == '\\' || c == ',' || c == '?';
}

// Protect TeXmacs internal escape sequences like "<#4E2D>" (CJK, etc.)
// from being split during automatic line wrapping in code/prog environments.
static inline int
tm_atom_end_for_code_wrap (string s, int i) {
  int n= N (s);
  if (i < 0 || i >= n) return i;
  if (s[i] != '<') return i + 1;

  // Only treat "<#...>" as an indivisible atom to avoid affecting normal code
  // like "<tag>"
  if (i + 1 >= n || s[i + 1] != '#') return i + 1;

  int j= i + 2;
  while (j < n && s[j] != '>')
    j++;
  if (j < n && s[j] == '>') return j + 1;

  // malformed sequence: degrade gracefully
  return i + 1;
}

// Snap "after" to the greatest atom boundary <= after, so we never split inside
// "<#...>".
static inline int
tm_snap_after_boundary_for_code_wrap (string s, int after) {
  int n= N (s);
  if (after <= 0) return 0;
  if (after >= n) return n;

  int i   = 0;
  int last= 0;
  while (i < n) {
    int j= tm_atom_end_for_code_wrap (s, i);
    if (j > after) break;
    last= j;
    i   = j;
  }
  return last;
}

text_property
verb_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos == N (s)) return &tp_normal_rep;
  if (s[pos] == ' ') {
    pos++;
    return &tp_space_rep;
  }
  if (is_sep_char (s[pos])) {
    pos++;
    while (pos < N (s) && is_sep_char (s[pos]) && s[pos] != '-')
      pos++;
    return &tp_hyph_rep;
  }

  array<int> cols= obtain_highlight (t, hl_lan);
  if (N (cols) == 0)
    while ((pos < N (s)) && (s[pos] != ' ') && !is_sep_char (s[pos]))
      pos++;
  else if ((pos < N (s)) && (s[pos] != ' ') && !is_sep_char (s[pos])) {
    pos++;
    while ((pos < N (s)) && (s[pos] != ' ') && !is_sep_char (s[pos]) &&
           cols[pos] == cols[pos - 1])
      pos++;
  }
  return &tp_normal_rep;
}

array<int>
verb_language_rep::get_hyphens (string s) {
  int        i;
  array<int> penalty (N (s) + 1);
  for (i= 0; i < N (penalty); i++)
    penalty[i]= HYPH_PANIC;
  return penalty;
}

void
verb_language_rep::hyphenate (string s, int after, string& left,
                              string& right) {
  int a= tm_snap_after_boundary_for_code_wrap (s, after);
  left = s (0, a);
  right= s (a, N (s));
}

string
verb_language_rep::get_color (tree t, int start, int end) {
  if (start >= end) return "";
  array<int> cols= obtain_highlight (t, hl_lan);
  if (start < N (cols) && cols[start] != 0)
    return decode_color (res_name, cols[start]);
  return "";
}
