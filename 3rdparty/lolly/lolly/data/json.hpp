
/******************************************************************************
 * MODULE     : json.hpp
 * DESCRIPTION: JSON Data Type
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#pragma once

#include "string.hpp"
#include "tree.hpp"
#include <stdint.h>

namespace lolly {
namespace data {

enum json_label : int {
  STRING_TYPE= 0,
  NULL_TYPE,
  BOOL_TYPE,
  INT64_TYPE,
  UINT64_TYPE,
  DOUBLE_TYPE,
  JSON_OBJECT,
  JSON_ARRAY,
  JSON_PAIR
};

struct json_rep : concrete_struct {
  tree                 t;
  hashmap<string, int> index;
  inline json_rep (tree p_t) : t (p_t) {}
};

class json {
  CONCRETE (json);

private:
  json (tree t) : rep (tm_new<json_rep> (t)) {}

public:
  // empty json
  json () { rep= tm_new<json_rep> (tree (JSON_OBJECT)); };

  // primitives constructors
  json (string value) { rep= tm_new<json_rep> (tree (value)); }
  json (const char* value) { rep= tm_new<json_rep> (tree (string (value))); }
  json (bool value) {
    rep= tm_new<json_rep> (tree (BOOL_TYPE, tree (as_string_bool (value))));
  }
  json (int64_t value) {
    rep= tm_new<json_rep> (tree (INT64_TYPE, tree (as_string (value))));
  }
  json (int32_t value) {
    rep= tm_new<json_rep> (tree (INT64_TYPE, tree (as_string (value))));
  }
  json (int16_t value) {
    rep= tm_new<json_rep> (tree (INT64_TYPE, tree (as_string (value))));
  }
  json (double value) {
    rep= tm_new<json_rep> (tree (DOUBLE_TYPE, tree (as_string (value))));
  }
  json (array<json> value) {
    array<tree> arr= array<tree> ();
    for (int i= 0; i < N (value); i++) {
      arr << value[i]->t;
    }
    rep= tm_new<json_rep> (tree (JSON_ARRAY, arr));
  }
  static inline json json_null () { return json (tree (NULL_TYPE)); };

  bool is_null () { return rep->t->op == NULL_TYPE; }
  bool is_string () { return rep->t->op == STRING_TYPE; }
  bool is_number () {
    return rep->t->op == DOUBLE_TYPE || rep->t->op == INT64_TYPE ||
           rep->t->op == UINT64_TYPE;
  }
  bool is_bool () { return rep->t->op == BOOL_TYPE; }
  bool is_object () { return rep->t->op == JSON_OBJECT; }
  bool is_array () { return rep->t->op == JSON_ARRAY; }

  bool   contains (string key);
  json   get (string key);
  json   operator() (string key);
  void   set (string key, json value);
  string dump ();
};
CONCRETE_CODE (json);

string as_string (json j);

} // namespace data
} // namespace lolly
