
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
is_emoji_character (unsigned int uc) {
  return (uc >= 0x1F600 && uc <= 0x1F64F) || // Emoticons
         (uc >= 0x1F300 && uc <= 0x1F5FF) || // Misc Symbols and Pictographs
         (uc >= 0x1F680 && uc <= 0x1F6FF) || // Transport and Map Symbols
         (uc >= 0x1F700 && uc <= 0x1F77F) || // Alchemical Symbols
         (uc >= 0x1F780 && uc <= 0x1F7FF) || // Geometric Shapes Extended
         (uc >= 0x1F800 && uc <= 0x1F8FF) || // Supplemental Arrows-C
         (uc >= 0x1F900 &&
          uc <= 0x1F9FF) || // Supplemental Symbols and Pictographs
         (uc >= 0x1FA00 && uc <= 0x1FA6F) || // Chess Symbols
         (uc >= 0x1FA70 &&
          uc <= 0x1FAFF) ||                // Symbols and Pictographs Extended-A
         (uc >= 0x2600 && uc <= 0x26FF) || // Miscellaneous Symbols
         (uc >= 0x2700 && uc <= 0x27BF) || // Dingbats
         (uc >= 0xFE00 && uc <= 0xFE0F) || // Variation Selectors
         (uc >= 0x1F1E6 && uc <= 0x1F1FF) || // Regional Indicator Symbols
         uc == 0x200D;                       // Zero Width Joiner
}
