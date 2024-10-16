
/******************************************************************************
 * MODULE     : http_response.hpp
 * DESCRIPTION: Tree repr of HTTP Response
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#pragma once

#include "generic_tree.hpp"
#include "hashmap.hpp"
#include "tree.hpp"

namespace lolly {
namespace io {

enum http_response_label : int {
  STATUS_CODE= 1,
  TEXT,
  URL,
  ELAPSED,
  HEADER,
  ROOT,
  TUPLE
};

inline tree
http_response_init () {
  tree ret= tree (http_response_label::ROOT, 0);
  ret << tree (http_response_label::STATUS_CODE, as<long, tree> (404));
  ret << tree (http_response_label::TEXT, tree (""));
  ret << tree (http_response_label::URL, tree (""));
  ret << tree (http_response_label::ELAPSED, as<double, tree> (0.0));
  auto hmap  = hashmap<string, string> ();
  tree header= tree (http_response_label::HEADER,
                     as<hashmap<string, string>, tree> (hmap));
  ret << header;
  return ret;
}

inline tree
http_response_ref (tree r, http_response_label op) {
  return r[op - 1][0];
}

inline void
http_response_set (tree r, http_response_label op, tree t) {
  r[op - 1][0]= t;
}
} // namespace io
} // namespace lolly
