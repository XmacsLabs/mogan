
/******************************************************************************
 * MODULE     : json.cpp
 * DESCRIPTION: Json Data Type
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "unicode.hpp"

namespace lolly {
namespace data {
string
unicode_get_range (int code) {
  if (code <= 0x7f) return "ascii";
  else if (code >= 0x80 && code <= 0x37f) return "latin";
  else if (code >= 0x380 && code <= 0x3ff) return "greek";
  else if (code >= 0x400 && code <= 0x4ff) return "cyrillic";
  else if (code >= 0x2460 && code <= 0x24ff) return "enclosed_alphanumerics";
  else if (code >= 0x3000 && code <= 0x303f) return "cjk";
  else if (code >= 0x4e00 && code <= 0x9fcc) return "cjk";
  else if (code >= 0xff00 && code <= 0xffef) return "cjk";
  else if (code >= 0x3040 && code <= 0x309F) return "hiragana";
  else if (code >= 0xac00 && code <= 0xd7af) return "hangul";
  else if (code >= 0x2000 && code <= 0x23ff) return "mathsymbols";
  else if (code >= 0x2900 && code <= 0x2e7f) return "mathextra";
  else if (code >= 0x1d400 && code <= 0x1d7ff) return "mathletters";
  else return "";
}
} // namespace data
} // namespace lolly
