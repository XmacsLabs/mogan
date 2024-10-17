
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
#include "tbox/tbox.h"

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

bool
is_cjk_unified_ideographs (string s) {
  int n= N (s);
  for (int i= 0; i < n; i++)
    if (s[i] == '<' && i + 1 < n && s[i + 1] == '#') {
      int start= i + 2;
      i        = i + 2;
      while (i < n && s[i] != '>')
        i++;
      string r= s (start, i);
      if ("4E00" <= r && r <= "9FBF") continue;
      else return false;
    }
    else {
      return false;
    }
  return true;
}

bool
has_cjk_unified_ideographs (string s) {
  int n= N (s);
  for (int i= 0; i < n; i++)
    if (s[i] == '<' && i + 1 < n && s[i + 1] == '#') {
      int start= i + 2;
      i        = i + 2;
      while (i < n && s[i] != '>')
        i++;
      string r= s (start, i);
      if ("4E00" <= r && r <= "9FBF") return true;
      else continue;
    }
    else {
      continue;
    }
  return false;
}

string
utf16_to_utf8 (string s_u16) {
  tb_size_t  isize= N (s_u16);
  tb_byte_t* idata= tb_malloc_bytes (isize);
  tb_long_t  osize= (tb_long_t) (isize << 2);
  tb_byte_t* odata= tb_malloc_bytes ((tb_size_t) osize);

  for (tb_size_t i= 0; i < isize; i++) {
    idata[i]= (tb_byte_t) s_u16[i];
  }

  osize= tb_charset_conv_data (TB_CHARSET_TYPE_UTF16, TB_CHARSET_TYPE_UTF8,
                               idata, isize, odata, osize);

  string ret ((int) osize);
  for (tb_size_t i= 0; i < osize; i++) {
    ret[i]= (char) odata[i];
  }
  if (idata) tb_free (idata);
  if (odata) tb_free (odata);
  return ret;
}

#if defined(OS_MINGW) || defined(OS_WIN)
string
wchar_to_utf8 (const wchar_t* s_u16) {
  tb_size_t  wchar_size= tb_wcslen (s_u16);
  tb_size_t  isize     = wchar_size * 2;
  tb_byte_t* idata     = tb_malloc_bytes (isize);
  tb_long_t  osize     = (tb_long_t) (isize << 2);
  tb_byte_t* odata     = tb_malloc_bytes ((tb_size_t) osize);

  for (tb_size_t i= 0; i < wchar_size; i++) {
    uint16_t  bytes = (uint16_t) s_u16[i];
    tb_byte_t high  = (tb_byte_t) (bytes >> 8);
    tb_byte_t low   = (tb_byte_t) (bytes & 0x00FF);
    idata[2 * i]    = high;
    idata[2 * i + 1]= low;
  }

  osize= tb_charset_conv_data (TB_CHARSET_TYPE_UTF16, TB_CHARSET_TYPE_UTF8,
                               idata, isize, odata, osize);

  string ret ((int) osize);
  for (tb_size_t i= 0; i < osize; i++) {
    ret[i]= (char) odata[i];
  }
  if (idata) tb_free (idata);
  if (odata) tb_free (odata);
  return ret;
}
#endif

string
utf8_to_utf16 (string s_u8) {
  tb_long_t  osize= (tb_long_t) (N (s_u8) << 2);
  tb_byte_t* odata= tb_malloc_bytes ((tb_size_t) osize);

  osize= tb_charset_conv_cstr (TB_CHARSET_TYPE_UTF8, TB_CHARSET_TYPE_UTF16,
                               c_string (s_u8), odata, osize);

  string ret ((int) osize);
  for (tb_size_t i= 0; i < osize; i++) {
    ret[i]= odata[i];
  }
  if (odata) tb_free (odata);
  return ret;
}

} // namespace data
} // namespace lolly
