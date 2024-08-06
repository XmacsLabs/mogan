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

#include <filesystem>
#include <iostream>
#include <sstream>
#include <vector>

using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::vector;
using std::filesystem::exists;
using std::filesystem::path;

using goldfish::glue_goldfish;
using goldfish::glue_liii_os;
using goldfish::glue_liii_uuid;
using goldfish::glue_scheme_process_context;
using goldfish::glue_scheme_time;

void
display_help () {
  cout << "Goldfish Scheme " << goldfish_version << " by LiiiLabs" << endl;
  cout << "--version\t"
       << "display version" << endl;
  cout << "-e       \t"
       << "-e '(+ 1 2)'" << endl;
  cout << "-l FILE  \t"
       << "Load the scheme code on path" << endl;
  cout << "FILE     \t"
       << "Load the scheme code on path and print the evaluated result" << endl;
}

void
display_version () {
  cout << "Goldfish Scheme " << goldfish_version << " by LiiiLabs" << endl;
  cout << "based on S7 Scheme " << S7_VERSION << " (" << S7_DATE << ")" << endl;
}

void
display_for_invalid_options () {
  cerr << "Invalid command line options!" << endl << endl;
  display_help ();
}

void
goldfish_eval_file (s7_scheme* sc, string path, bool quiet) {
  s7_pointer result= s7_load (sc, path.c_str ());
  if (!result) {
    cerr << "Failed to load " << path << endl;
    exit (-1);
  }
  if (!quiet) {
    cout << path << " => " << s7_object_to_c_string (sc, result) << endl;
  }
}

void
goldfish_eval_code (s7_scheme* sc, string code) {
  s7_pointer x= s7_eval_c_string (sc, code.c_str ());
  cout << s7_object_to_c_string (sc, x) << endl;
}

int
main (int argc, char** argv) {
  // Check if the standard library and boot.scm exists
  const path gf_root= path (argv[0]).parent_path ().parent_path ();
  const path gf_lib = gf_root / "goldfish";
  const path gf_boot= gf_lib / "scheme" / "boot.scm";
  if (!exists (gf_lib)) {
    cerr << "The load path for Goldfish Scheme Standard Library does not exist"
         << endl;
    exit (-1);
  }
  if (!exists (gf_boot)) {
    cerr << "The boot.scm for Goldfish Scheme does not exist" << endl;
    exit (-1);
  }
  vector<string> all_args (argv, argv + argc);
  for (string arg : all_args) {
    command_args.push_back (arg);
  }

  // Init the underlying S7 Scheme and add the load_path
  s7_scheme* sc;
  sc= s7_init ();
  s7_load (sc, gf_boot.string ().c_str ());
  s7_add_to_load_path (sc, gf_lib.string ().c_str ());

  // Init tbox
  if (!tb_init (tb_null, tb_null)) exit (-1);

  // Glues
  glue_goldfish (sc);
  glue_scheme_time (sc);
  glue_scheme_process_context (sc);
  glue_liii_os (sc);
  glue_liii_uuid (sc);

  // Command options
  vector<string> args (argv + 1, argv + argc);
  if (args.size () == 0) {
    display_help ();
  }
  else if (args.size () == 1 && args[0].size () > 0 && args[0][0] == '-') {
    if (args[0] == "--version") {
      display_version ();
    }
    else {
      display_for_invalid_options ();
    }
  }
  else if (args.size () == 2 && args[0] == "-e") {
    goldfish_eval_code (sc, args[1]);
  }
  else if (args.size () == 2 && args[0] == "-l") {
    goldfish_eval_file (sc, args[1], true);
  }
  else if (args.size () == 1 && args[0].size () > 0 && args[0][0] != '-') {
    goldfish_eval_file (sc, args[0], false);
  }
  else {
    display_for_invalid_options ();
  }
  return 0;
}
