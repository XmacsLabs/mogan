
/******************************************************************************
* MODULE     : locale.hpp
* DESCRIPTION: Locale related routines
* COPYRIGHT  : (C) 1999-2019  Joris van der Hoeven, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LOCALE_HPP
#define LOCALE_HPP

#include "string.hpp"

string locale_to_language (string s);
string language_to_locale (string s);
string language_to_local_ISO_charset (string s);
string get_locale_language ();
string get_locale_charset ();
#if defined(OS_MINGW) || defined(OS_WIN)
namespace win32 {
  string get_date (string lan, string fm);
}
#endif
string get_date (string lan, string fm);
string pretty_time (int t);

#endif // LOCALE_HPP

