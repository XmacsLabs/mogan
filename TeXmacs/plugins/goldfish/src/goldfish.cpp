//
// Copyright (C) 2024 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "goldfish.hpp"
#include "s7.h"
#include <string>
#include <iostream>
#include <cpr/cpr.h>

using namespace goldfish;
using namespace std;

static s7_pointer
response2hashtable (s7_scheme* sc, cpr::Response r) {
  s7_pointer ht= s7_make_hash_table (sc, 8);
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "status-code"), s7_make_integer (sc, r.status_code));
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "url"), s7_make_string (sc, r.url.c_str()));
  s7_hash_table_set (sc, ht, s7_make_symbol(sc, "elapsed"), s7_make_real (sc, r.elapsed));
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "text"), s7_make_string (sc, r.text.c_str ()));
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "reason"), s7_make_string (sc, r.reason.c_str ()));
  s7_pointer headers= s7_make_hash_table(sc, 8);
  for (const auto &header : r.header) {
    const auto key= header.first.c_str ();
    const auto value= header.second.c_str ();
    s7_hash_table_set (sc, headers, s7_make_string (sc, key), s7_make_string (sc, value));
  }
  s7_hash_table_set (sc, ht, s7_make_symbol(sc, "headers"), headers);

  return ht;
}

inline cpr::Parameters
to_cpr_parameters (s7_scheme* sc, s7_pointer args) {
  cpr::Parameters params= cpr::Parameters{};
  if (s7_is_list(sc, args)) {
    s7_pointer iter= args;
    while (!s7_is_null (sc, iter)) {
      s7_pointer pair= s7_car (iter);
      if (s7_is_pair (pair)) {
        const char* key= s7_string (s7_car (pair));
        const char* value= s7_string (s7_cdr (pair));
        params.Add (cpr::Parameter (string (key), string (value)));
      }
      iter= s7_cdr (iter);
    }
  }
  return params;
}

inline cpr::Header
to_cpr_headers (s7_scheme* sc, s7_pointer args) {
  cpr::Header headers= cpr::Header{};
  if (s7_is_list(sc, args)) {
    s7_pointer iter= args;
    while (!s7_is_null (sc, iter)) {
      s7_pointer pair= s7_car (iter);
      if (s7_is_pair (pair)) {
        const char* key= s7_string (s7_car (pair));
        const char* value= s7_string (s7_cdr (pair));
        headers.insert (std::make_pair (key, value));
      }
      iter= s7_cdr (iter);
    }
  }
  return headers;
}

static s7_pointer
f_http_head (s7_scheme* sc, s7_pointer args) {
  const char* url= s7_string (s7_car (args));
  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  cpr::Response r= session.Head ();
  return response2hashtable (sc, r);
}

inline void
glue_http_head (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);
  const char* s_http_head = "g_http-head";
  const char* d_http_head = "(g_http-head url ...) => hash-table?";
  auto func_http_head= s7_make_typed_function (sc, s_http_head, f_http_head, 1, 0, false, d_http_head, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, s_http_head), func_http_head);
}

static s7_pointer
f_http_get (s7_scheme* sc, s7_pointer args) {
  const char* url= s7_string (s7_car (args));
  s7_pointer params= s7_cadr (args);
  cpr::Parameters cpr_params= to_cpr_parameters(sc, params);

  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  session.SetParameters (cpr_params);

  cpr::Response r= session.Get ();
  return response2hashtable (sc, r);
}

inline void
glue_http_get (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);
  const char* s_http_get= "g_http-get";
  const char* d_http_get= "(g_http-get url params) => hash-table?";
  auto func_http_get= s7_make_typed_function (sc, s_http_get, f_http_get, 2, 0, false, d_http_get, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, s_http_get), func_http_get);
}

static s7_pointer
f_http_post (s7_scheme* sc, s7_pointer args) {
  const char* url= s7_string (s7_car (args));
  s7_pointer params= s7_cadr (args);
  cpr::Parameters cpr_params= to_cpr_parameters(sc, params);
  const char* body= s7_string (s7_caddr (args));
  cpr::Body cpr_body= cpr::Body (body);
  s7_pointer headers= s7_cadddr (args);
  cpr::Header cpr_headers= to_cpr_headers (sc, headers);

  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  session.SetParameters (cpr_params);
  session.SetBody (cpr_body);
  session.SetHeader (cpr_headers);

  cpr::Response r= session.Post ();
  return response2hashtable (sc, r);
}

inline void
glue_http_post (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);
  const char* name= "g_http-post";
  const char* doc= "(g_http-post url params body) => hash-table?";
  auto func_http_post= s7_make_typed_function (sc, name, f_http_post, 4, 0, false, doc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func_http_post);
}

inline void
glue_http (s7_scheme* sc) {
  glue_http_head (sc);
  glue_http_get (sc);
  glue_http_post (sc);
}

int
main (int argc, char** argv) {
  string      gf_lib_dir  = find_goldfish_library ();
  const char* gf_lib      = gf_lib_dir.c_str ();
  s7_scheme* sc= init_goldfish_scheme (gf_lib);
  glue_http (sc);
  return repl_for_community_edition (sc, argc, argv);
}
