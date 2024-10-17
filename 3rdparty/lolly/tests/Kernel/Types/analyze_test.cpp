#include "a_lolly_test.hpp"
#include "analyze.hpp"

TEST_CASE ("is_alpha") {
  for (unsigned char c= 0; c < 255; c++) {
    if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122)) {
      CHECK (is_alpha (c));
    }
    else {
      CHECK (!is_alpha (c));
    }
  }
}

TEST_CASE ("is_digit") {
  for (unsigned char c= 0; c < 255; c++) {
    if (c >= 48 && c <= 57) {
      CHECK (is_digit (c));
    }
    else {
      CHECK (!is_digit (c));
    }
  }
}

TEST_CASE ("is_space") {
  for (unsigned char c= 0; c < 255; c++) {
    if ((c == 9) || (c == 10) || (c == 13) || (c == 32)) {
      CHECK (is_space (c));
    }
    else {
      CHECK (!is_space (c));
    }
  }
}

TEST_CASE ("is_binary_digit") {
  for (unsigned char c= 0; c < 255; c++) {
    if ((c == '0') || (c == '1')) {
      CHECK (is_binary_digit (c));
    }
    else {
      CHECK (!is_binary_digit (c));
    }
  }
}

TEST_CASE ("is_alpha") {
  CHECK (is_alpha ("a"));
  CHECK (is_alpha ("abc"));
  CHECK (is_alpha ("Hello"));
  CHECK (!is_alpha ("!"));
  CHECK (!is_alpha ("abc123"));
  CHECK (!is_alpha (""));
}

TEST_CASE ("is_alphanum") {
  CHECK (is_alphanum ("s3"));
  CHECK (is_alphanum ("ipv6"));
  CHECK (is_alphanum ("abc"));
  CHECK (is_alphanum ("123"));
  CHECK (!is_alphanum (""));
  CHECK (!is_alphanum ("!"));
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

TEST_CASE ("remove_prefix") {
  string_eq (remove_prefix ("abc", "a"), "bc");
  string_eq (remove_prefix ("abc", ""), "abc");
  string_eq (remove_prefix ("", ""), "");
  string_eq (remove_prefix ("abc", ""), "abc");
  string_eq (remove_prefix ("a1a", "a"), "1a");
}

TEST_CASE ("remove_suffix") {
  string_eq (remove_suffix ("abc", "c"), "ab");
  string_eq (remove_suffix ("abc", ""), "abc");
  string_eq (remove_suffix ("", ""), "");
  string_eq (remove_suffix ("abc", ""), "abc");
  string_eq (remove_suffix ("a1a", "a"), "a1");
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

TEST_CASE ("contains/occurs") {
  CHECK (contains ("abc", "a"));
  CHECK (contains ("abc", "ab"));
  CHECK (contains ("abc", "bc"));
  CHECK (!contains ("abc", "B"));
  CHECK (contains ("", ""));
  CHECK (contains ("abc", ""));
  CHECK (!contains ("abc", " "));
  CHECK (contains ("hello world", " "));
}

TEST_CASE ("replace") {
  CHECK_EQ (replace ("a-b", "-", "_") == "a_b", true);
  CHECK_EQ (replace ("a-b-c", "-", "_") == "a_b_c", true);
}

TEST_CASE ("tokenize") {
  CHECK_EQ (tokenize ("hello world", " "), array<string> ("hello", "world"));
  CHECK_EQ (tokenize ("zotero://select/library/items/2AIFJFS7", "://"),
            array<string> ("zotero", "select/library/items/2AIFJFS7"));
}

TEST_CASE ("recompose") {
  string_eq (recompose (array<string> ("hello", "world"), " "), "hello world");
  string_eq (
      recompose (array<string> ("zotero", "select/library/items/2AIFJFS7"),
                 "://"),
      "zotero://select/library/items/2AIFJFS7");
}
