
/******************************************************************************
 * MODULE     : sha_test.cpp
 * DESCRIPTION: tests on sha
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "a_tbox_main.cpp"
#include "file.hpp"
#include "lolly/hash/sha.hpp"

using lolly::hash::sha224_hexdigest;
using lolly::hash::sha256_hexdigest;

TEST_CASE ("sha224_hexdigest") {
  SUBCASE ("normal file") {
    string_eq (sha224_hexdigest (url_pwd () * "LICENSE"),
               "2f051189f6ddb2d2fd11faeb8cf3e3b38b4216f2798ce1d92421e129");
  }
  SUBCASE ("empty file") {
    url temp= url_temp ();
    string_save ("", temp);
    CHECK_EQ (file_size (temp), 0);
    string_eq (sha224_hexdigest (temp),
               "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f");
  }
  SUBCASE ("invalid file") {
    string_eq (sha224_hexdigest (url_system ("https://mogan.app")), "");
    string_eq (sha224_hexdigest (url_system ("/path/to/not_exist")), "");
  }
}

TEST_CASE ("sha256_hexdigest") {
  SUBCASE ("normal file") {
    string_eq (
        sha256_hexdigest (url_pwd () * "LICENSE"),
        "8ceb4b9ee5adedde47b31e975c1d90c73ad27b6b165a1dcd80c7c545eb65b903");
  }
  SUBCASE ("empty file") {
    url temp= url_temp ();
    string_save ("", temp);
    CHECK_EQ (file_size (temp), 0);
    string_eq (
        sha256_hexdigest (temp),
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
  }
  SUBCASE ("invalid file") {
    string_eq (sha256_hexdigest (url_system ("https://mogan.app")), "");
    string_eq (sha256_hexdigest (url_system ("/path/to/not_exist")), "");
  }
}
