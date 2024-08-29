#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest/doctest.h"

#include "analyze.hpp"

TEST_CASE ("test is alpha") {
  for (unsigned char c= 0; c < 255; c++) {
    if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122)) {
      CHECK (is_alpha (c));
    }
    else {
      CHECK (!is_alpha (c));
    }
  }
}

TEST_CASE ("cjk_unified_ideographs") {
  CHECK (is_cjk_unified_ideographs ("<#4E2D>"));
  CHECK (has_cjk_unified_ideographs ("<#4E2D>"));
  CHECK (has_cjk_unified_ideographs ("bib-<#4E2D>"));
  CHECK (!is_cjk_unified_ideographs ("bib-<#4E2D>"));
}

TEST_CASE ("test locase all") {
  CHECK_EQ (locase_all (string ("true")) == string ("true"), true);
  CHECK_EQ (locase_all (string ("TRue")) == string ("true"), true);
  CHECK_EQ (locase_all (string ("TRUE")) == string ("true"), true);
  CHECK_EQ (locase_all (string ("123TRUE")) == string ("123true"), true);
}

TEST_CASE ("test upcase all") {
  CHECK_EQ (upcase_all (string ("true")) == string ("TRUE"), true);
  CHECK_EQ (upcase_all (string ("TRue")) == string ("TRUE"), true);
  CHECK_EQ (upcase_all (string ("TRUE")) == string ("TRUE"), true);
  CHECK_EQ (upcase_all (string ("123true")) == string ("123TRUE"), true);
}

TEST_CASE ("test string minus") {
  CHECK_EQ (string_minus ("Hello World", "eo") == string ("Hll Wrld"), true);
  CHECK_EQ (string_minus ("", "abc") == string (""), true);
  CHECK_EQ (string_minus ("abc", "") == string ("abc"), true);
}

TEST_CASE ("test string union") {
  CHECK_EQ (string_union ("abc", "") == string ("abc"), true);
  CHECK_EQ (string_union ("", "abc") == string ("abc"), true);
  CHECK_EQ (string_union ("Hello World", "eo") == string ("Hll Wrldeo"), true);
}

TEST_CASE ("test scm quote") {
  CHECK_EQ (scm_quote ("a") == "\"a\"", true);
  CHECK_EQ (scm_quote ("") == "\"\"", true);
  CHECK_EQ (scm_quote ("\\") == "\"\\\\\"", true);
}

TEST_CASE ("test_scm_unquote") {
  CHECK_EQ (scm_unquote ("\"\"") == "", true);
  CHECK_EQ (scm_unquote ("\"abc\"") == "abc", true);
  CHECK_EQ (scm_unquote ("abc") == "abc", true);
  CHECK_EQ (scm_unquote ("") == "", true);
  CHECK_EQ (scm_unquote ("\"\\\\\"") == "\\", true);
}

TEST_CASE ("test_raw_quote") {
  CHECK_EQ (raw_quote ("a") == "\"a\"", true);
  CHECK_EQ (raw_quote ("") == "\"\"", true);
}

TEST_CASE ("test_raw_unquote") {
  CHECK_EQ (raw_unquote ("\"a\"") == "a", true);
  CHECK_EQ (raw_unquote ("\"a") == "\"a", true);
  CHECK_EQ (raw_unquote ("a\"") == "a\"", true);
  CHECK_EQ (raw_unquote ("") == "", true);
  CHECK_EQ (raw_unquote ("a") == "a", true);
}

TEST_CASE ("test_unescape_guile") {
  CHECK_EQ (unescape_guile ("\\\\") == "\\\\\\\\", true);
}

TEST_CASE ("test_starts") {
  CHECK (starts ("abc_def", "abc"));
  CHECK (!starts ("abc_def", "def"));
  CHECK (starts ("abc", ""));
  CHECK (starts ("", ""));
}

TEST_CASE ("test_ends") {
  CHECK (ends ("abc_def", "def"));
  CHECK (ends ("abc_def", ""));
  CHECK (!ends ("abc_def", "de"));
}

TEST_CASE ("test_read_word") {
  string word;
  int    i= 0;
  CHECK (read_word ("hello123", i, word));
  CHECK_EQ (word == "hello", true);
  CHECK_EQ (i, 5);

  i   = 0;
  word= "";
  CHECK (!read_word ("123", i, word));
  CHECK (is_empty (word));
  CHECK_EQ (i, 0);
}

TEST_CASE ("test_is_digit") {
  for (unsigned char c= 0; c < 255; c++) {
    if (c >= 48 && c <= 57) {
      CHECK (is_digit (c));
    }
    else {
      CHECK (!is_digit (c));
    }
  }
}

TEST_CASE ("test_is_space") {
  for (unsigned char c= 0; c < 255; c++) {
    if ((c == 9) || (c == 10) || (c == 13) || (c == 32)) {
      CHECK (is_space (c));
    }
    else {
      CHECK (!is_space (c));
    }
  }
}

TEST_CASE ("test_is_binary_digit") {
  for (unsigned char c= 0; c < 255; c++) {
    if ((c == '0') || (c == '1')) {
      CHECK (is_binary_digit (c));
    }
    else {
      CHECK (!is_binary_digit (c));
    }
  }
}