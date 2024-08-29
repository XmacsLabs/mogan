#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest/doctest.h"
#include "string.hpp"

TEST_CASE ("equality of string") {
  CHECK_EQ (string ("abc") == "abc", true);
  CHECK_EQ (string ("abc") == "", false);

  CHECK_EQ (string ("abc") != "abc", false);
  CHECK_EQ (string ("abc") != "", true);

  CHECK_EQ (string ("abc") == string ("abc"), true);
  CHECK_EQ (string ("abc") == string (), false);
  CHECK_EQ (string ("abc") != string ("abc"), false);
  CHECK_EQ (string ("abc") != string (), true);

  CHECK_EQ (string () == string (), true);
}

TEST_CASE ("compare string") {
  CHECK (string ("ab") < string ("b"));
  CHECK (string () < string ("0"));
  CHECK (string ("a") <= string ("a"));
  CHECK (string ("ab") <= string ("b"));
  CHECK (string () <= string ());
  CHECK (string () <= string ("0"));
}

TEST_CASE ("test slice") {
  CHECK_EQ (string ("abcde") (0, 0) == string (), true);
  CHECK_EQ (string ("abcde") (0, 1) == string ("a"), true);
  CHECK_EQ (string ("abcde") (0, 10) == string ("abcde"), true);
  CHECK_EQ (string ("abcde") (-1, 1) == string ("a"), true);
  CHECK_EQ (string ("abcde") (3, 2) == string (), true);
  CHECK_EQ (string ("abcde") (3, -2) == string (), true);
  CHECK_EQ (string ("abcde") (10, 11) == string (), true);
  CHECK_EQ (string ("abcde") (-3, -2) == string (), true);
}

TEST_CASE ("test concat") {
  CHECK_EQ (string ("abc") * "de" == string ("abcde"), true);
  CHECK_EQ (string ("abc") * string ("de") == string ("abcde"), true);
  CHECK_EQ ("abc" * string ("de") == string ("abcde"), true);
}

/******************************************************************************
 * Modifications
 ******************************************************************************/

TEST_CASE ("test append") {
  auto str= string ();
  str << 'x';
  CHECK_EQ (str == string ("x"), true);
  str << string ("yz");
  CHECK_EQ (str == string ("xyz"), true);
}

/******************************************************************************
 * Conversions
 ******************************************************************************/

TEST_CASE ("test as bool") {
  CHECK_EQ (as_bool (string ("true")), true);
  CHECK_EQ (as_bool (string ("#t")), true);
  CHECK_EQ (as_bool (string ("false")), false);

  CHECK_EQ (as_bool ("true"), true);
  CHECK_EQ (as_bool ("#t"), true);
  CHECK_EQ (!as_bool ("false"), true);
  // implicit conversion from char*
}

TEST_CASE ("test as strig bool") {
  CHECK_EQ (as_string_bool (true) == string ("true"), true);
  CHECK_EQ (as_string_bool (false) == string ("false"), true);
}

/******************************************************************************
 * Predicates
 ******************************************************************************/

TEST_CASE ("test is empty") {
  CHECK_EQ (is_empty (""), true);
  CHECK_EQ (!is_empty (" "), true);
  CHECK_EQ (!is_empty ("nonempty"), true);
}

TEST_CASE ("test is bool") {
  CHECK_EQ (is_bool ("true"), true);
  CHECK_EQ (is_bool ("false"), true);
  CHECK_EQ (is_bool (string ("true")), true);
  CHECK_EQ (is_bool (string ("false")), true);

  CHECK_EQ (!is_bool ("100"), true);
  CHECK_EQ (!is_bool ("nil"), true);
}

TEST_CASE ("test is int") {
  // Empty string is not an int
  CHECK_EQ (!is_int (""), true);

  // Only 0-9 in chars are int
  for (auto i= 0; i < 256; i++) {
    char iter= (char) i;
    if (iter >= '0' && iter <= '9') CHECK_EQ (is_int (iter), true);
    else CHECK_EQ (!is_int (iter), true);
  }

  // Random tests
  CHECK_EQ (is_int ("-100"), true);
  CHECK_EQ (is_int ("+100"), true);
  CHECK_EQ (is_int ("100"), true);
  CHECK_EQ (!is_int (".0"), true);
  CHECK_EQ (!is_int ("0x09"), true);
}

TEST_CASE ("test is quoted") {
  CHECK_EQ (is_quoted ("\"\""), true);
  CHECK_EQ (is_quoted ("\"Hello TeXmacs\""), true);
  CHECK_EQ (is_quoted ("\"Hello\"TeXmacs\""), true);

  CHECK_EQ (!is_quoted ("\""), true);
  CHECK_EQ (!is_quoted ("A"), true);
  CHECK_EQ (!is_quoted ("9"), true);
  CHECK_EQ (!is_quoted ("Hello TeXmacs"), true);
  CHECK_EQ (!is_quoted ("\"Hello TeXmac\"s"), true);
  CHECK_EQ (!is_quoted ("H\"ello TeXmacs\""), true);
  // is_quoted only checks if a string starts with a double quote
  // and ends with another double quote, regardless the validity
  // of the raw string
}
