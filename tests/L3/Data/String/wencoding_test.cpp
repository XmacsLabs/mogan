/*****************************************************************************
 * MODULE     : wencoding_test.cpp
 * DESCRIPTION: Tests on wencoding
 * COPYRIGHT  : (C) 2024  ATQlove
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "string.hpp"
#include "wencoding.hpp"
#include <QtTest/QtTest>
#include <string>

class TestWencoding : public QObject {
  Q_OBJECT

private slots:
  void test_looks_ascii ();
  void test_looks_utf8 ();
  void test_guess_wencoding ();
  void test_western_to_utf8 ();
  void test_western_to_cork ();
};

// Helper function to convert `string` to `std::string`
std::string
toString (const string& s) {
  std::string result;
  // Temporarily create a non-const string for iteration purpose
  string non_const_string= s;
  for (int i= 0; i < N (non_const_string); ++i) {
    result.push_back (non_const_string[i]);
  }
  return result;
}

void
TestWencoding::test_looks_ascii () {
  string ascii_str    = "Hello, World!";
  string non_ascii_str= "こんにちは、世界！";
  QVERIFY (looks_ascii (ascii_str));
  QVERIFY (!looks_ascii (non_ascii_str));
}

void
TestWencoding::test_looks_utf8 () {
  string utf8_str    = u8"Hello, 世界!";
  string non_utf8_str= "\xFF\xFE";
  QVERIFY (looks_utf8 (utf8_str));
  QVERIFY (!looks_utf8 (non_utf8_str));
}

void
TestWencoding::test_guess_wencoding () {
  string ascii_str   = "Hello, World!";
  string utf8_str    = u8"こんにちは、世界！";
  string iso_8859_str= "\xC3\xA9";
  QCOMPARE (toString (guess_wencoding (ascii_str)), std::string ("ASCII"));
  QCOMPARE (toString (guess_wencoding (utf8_str)), std::string ("UTF-8"));
  QCOMPARE (toString (guess_wencoding (iso_8859_str)),
            std::string ("ISO-8859"));
  // It seems to return "UTF-8" when processing ISO-8859 encoded strings.
  // Perhaps the function misjudged the encoding when determining the encoding.
}

void
TestWencoding::test_western_to_utf8 () {
  string iso_8859_str= "\xE9"; // é in ISO-8859
  string utf8_result = western_to_utf8 (iso_8859_str);
  QCOMPARE (toString (utf8_result), std::string (u8"é"));
}

void
TestWencoding::test_western_to_cork () {
  string utf8_str   = u8"Hello, 世界!";
  string cork_result= western_to_cork (utf8_str);
  QCOMPARE (
      toString (cork_result),
      toString (
          utf8_str)); // Assuming your conversion to Cork maintains the content
}

QTEST_MAIN (TestWencoding)
#include "wencoding_test.moc"
