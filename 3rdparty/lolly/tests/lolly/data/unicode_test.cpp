
/******************************************************************************
 * MODULE     : unicode_test.cpp
 * DESCRIPTION: tests on unicode
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "a_tbox_main.cpp"
#include "lolly/data/unicode.hpp"

using lolly::data::unicode_get_range;

TEST_CASE ("unicode_get_range") {
  string_eq (unicode_get_range ((int) 'a'), "ascii");
  string_eq (unicode_get_range (0x2460), "enclosed_alphanumerics"); // ①
  string_eq (unicode_get_range (0x24ff), "enclosed_alphanumerics"); // ⓿
}
