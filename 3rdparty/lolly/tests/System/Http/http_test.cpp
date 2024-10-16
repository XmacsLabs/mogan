/** \file http_test.cpp
 *  \copyright GPLv3
 *  \details Unitests for http.
 *  \author Darcy Shen
 *  \date   2023
 */

#include "a_tbox_main.cpp"
#include "file.hpp"
#include "generic_tree.hpp"
#include "hashmap.hpp"
#include "lolly/io/http.hpp"

using namespace lolly::io;

TEST_CASE ("http::get") {
#ifndef OS_WASM
  tree r          = http_get ("https://httpbin.org/get");
  long status_code= as<tree, long> (http_response_ref (r, STATUS_CODE));
  CHECK_EQ (status_code, 200);
  auto hmap= as<tree, hashmap<string, string> > (http_response_ref (r, HEADER));
  string content_type= hmap ("content-type");
  string_eq (content_type, "application/json");
#endif
}

TEST_CASE ("http::download") {
#ifndef OS_WASM
  url  from= url ("http://mirrors.ustc.edu.cn/gnu/GNUinfo/README");
  url  to  = url_temp_dir () * url ("README");
  tree r   = download (from, to);
  CHECK (file_size (to) > 0);
#endif
}
