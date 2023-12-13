
/******************************************************************************
 * MODULE     : tt_tools.hpp
 * DESCRIPTION: Direct access of True Type font (independent from FreeType)
 * COPYRIGHT  : (C) 2012  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TT_TOOLS_H
#define TT_TOOLS_H

#include "url.hpp"

void        tt_dump (url u);
scheme_tree tt_font_name (url u);
url         tt_unpack (string name);

#ifdef USE_FREETYPE

array<string> tt_analyze (string family);
double        characteristic_distance (array<string> a1, array<string> a2);
double        trace_distance (string v1, string v2, double m);
string        find_attribute_value (array<string> a, string s);

// quantities with respect to ex height
double get_M_width (array<string> a);
double get_lo_pen_width (array<string> a);
double get_lo_pen_height (array<string> a);
double get_up_pen_width (array<string> a);
double get_up_pen_height (array<string> a);

#else

inline array<string>
tt_analyze (string family) {
  (void) family;
  array<string> r;
  return r;
}

inline double
trace_distance (string v1, string v2, double m) {
  return 0;
}
inline string
find_attribute_value (array<string> a, string s) {
  return "";
}
inline int
characteristic_distance (array<string> a, array<string> s) {
  return 0;
}

inline double
get_M_width (array<string> a) {
  return 0.0;
}
inline double
get_lo_pen_width (array<string> a) {
  return 0.0;
}
inline double
get_lo_pen_height (array<string> a) {
  return 0.0;
}
inline double
get_up_pen_width (array<string> a) {
  return 0.0;
}
inline double
get_up_pen_height (array<string> a) {
  return 0.0;
}

#endif

#endif // TT_TOOLS_H
