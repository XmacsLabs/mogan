
/******************************************************************************
 * MODULE     : unicode.hpp
 * DESCRIPTION: Unicode related routines
 * COPYRIGHT  : (C) 2013  Joris van der Hoeven
 *                  2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "unicode.hpp"
#include "converter.hpp"

#include <lolly/data/unicode.hpp>

string
get_unicode_range (string c) {
  string uc= strict_cork_to_utf8 (c);
  if (N (uc) == 0) return "";
  int      pos  = 0;
  uint32_t code = lolly::data::decode_from_utf8 (uc, pos);
  string   range= lolly::data::unicode_get_range (code);
  if (pos == N (uc)) return range;
  return "";
}

bool
is_emoji_character (int uc) {
  return lolly::data::unicode_get_range (uc) == "emoji";
}
