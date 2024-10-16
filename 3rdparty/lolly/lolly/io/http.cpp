
/******************************************************************************
 * MODULE     : http.cpp
 * DESCRIPTION: HTTP related routines
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "lolly/io/http.hpp"
#include "analyze.hpp"
#include "generic_tree.hpp"
#include "hashmap.hpp"
#include "lolly/data/uri.hpp"
#include "lolly/io/http_response.hpp"
#include "tree.hpp"

#ifndef OS_WASM
#include <cpr/cpr.h>
#endif

namespace lolly {
namespace io {

#ifdef OS_WASM

tree
http_get (url u) {
  return http_response_init ();
}

tree
download (url from, url to) {
  return http_response_init ();
}

#else

static tree
response_to_tree (cpr::Response r, string url) {
  tree ret= http_response_init ();
  http_response_set (ret, STATUS_CODE, as<long, tree> (r.status_code));
  http_response_set (ret, TEXT, tree (r.text.c_str ()));
  http_response_set (ret, URL, tree (url));
  http_response_set (ret, ELAPSED, as<double, tree> (r.elapsed));
  auto hmap= hashmap<string, string> ();
  for (auto i= r.header.begin (); i != r.header.end (); i++) {
    string key  = locase_all (string (i->first.c_str ()));
    string value= string (i->second.c_str ());
    hmap (key)  = value;
  }
  http_response_set (ret, HEADER, as<hashmap<string, string>, tree> (hmap));
  return ret;
}

tree
http_get (url u) {
  string        u_str = as_string (u);
  c_string      u_cstr= c_string (u_str);
  cpr::Response r     = cpr::Get (cpr::Url{u_cstr});
  return response_to_tree (r, u_str);
}

tree
download (url from, url to) {
  string   from_str = as_string (from);
  c_string from_cstr= c_string (from_str);
  string   to_str   = as_string (to);
  c_string to_cstr  = c_string (to_str);

  std::ofstream to_stream (to_cstr, std::ios::binary);
  cpr::Response r= cpr::Download (to_stream, cpr::Url{from_cstr});
  return response_to_tree (r, from_str);
}

#endif

} // namespace io
} // namespace lolly
