/** \file lolly_string_test.cpp
 *  \copyright GPLv3
 *  \details Test for string
 *  \author jingkaimori
 *  \date   2024
 */

#include "doctest/doctest.h"
#include "lolly/data/lolly_string.hpp"

using namespace lolly::data;

TEST_CASE ("equality of string") {
  CHECK_EQ (string_u16 (u"abc") == u"abc", true);
  CHECK_EQ (string_u16 (u"abc") == u"", false);

  CHECK_EQ (string_u16 (u"abc") != u"abc", false);
  CHECK_EQ (string_u16 (u"abc") != u"", true);

  CHECK_EQ (string_u16 (u"abc") == string_u16 (u"abc"), true);
  CHECK_EQ (string_u16 (u"abc") == string_u16 (), false);
  CHECK_EQ (string_u16 (u"abc") != string_u16 (u"abc"), false);
  CHECK_EQ (string_u16 (u"abc") != string_u16 (), true);

  CHECK_EQ (string_u16 () == string_u16 (), true);
}

TEST_CASE ("compare string") {
  CHECK (string_u16 (u"ab") < string_u16 (u"b"));
  CHECK (!(string_u16 (u"b") < string_u16 (u"ab")));
  CHECK (string_u16 () < string_u16 (u"0"));
  CHECK (!(string_u16 (u"0") < string_u16 ()));
  CHECK (string_u16 (u"a") <= string_u16 (u"a"));
  CHECK (!(string_u16 (u"ab") <= string_u16 (u"a")));
  CHECK (string_u16 (u"ab") <= string_u16 (u"b"));
  CHECK (!(string_u16 (u"b") <= string_u16 (u"ab")));
  CHECK (string_u16 () <= string_u16 ());
  CHECK (string_u16 () <= string_u16 (u"0"));
  CHECK (!(string_u16 (u"0") <= string_u16 ()));
}

TEST_CASE ("test slice") {
  CHECK_EQ (string_u16 (u"abcde") (0, 0) == string_u16 (), true);
  CHECK_EQ (string_u16 (u"abcde") (0, 1) == string_u16 (u"a"), true);
  CHECK_EQ (string_u16 (u"abcde") (1, 3) (0, 1) == string_u16 (u"b"), true);
  CHECK_EQ (string_u16 (u"abcde") (0, 10) == string_u16 (u"abcde"), true);
  CHECK_EQ (string_u16 (u"abcde") (-1, 1) == string_u16 (u"a"), true);
  CHECK_EQ (string_u16 (u"abcde") (3, 2) == string_u16 (), true);
  CHECK_EQ (string_u16 (u"abcde") (3, -2) == string_u16 (), true);
  CHECK_EQ (string_u16 (u"abcde") (10, 11) == string_u16 (), true);
  CHECK_EQ (string_u16 (u"abcde") (-3, -2) == string_u16 (), true);
}

TEST_CASE ("test concat") {
  CHECK_EQ (string_u16 (u"abc") * u"de" == string_u16 (u"abcde"), true);
  CHECK_EQ (string_u16 (u"abc") * string_u16 (u"de") == string_u16 (u"abcde"),
            true);
  CHECK_EQ (u"abc" * string_u16 (u"de") == string_u16 (u"abcde"), true);
}

/******************************************************************************
 * Modifications
 ******************************************************************************/

TEST_CASE ("test append") {
  auto str= string_u16 ();
  str << u'x';
  CHECK_EQ (str == string_u16 (u"x"), true);
  str << string_u16 (u"yz");
  CHECK_EQ (str == string_u16 (u"xyz"), true);
}
