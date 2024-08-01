/* 0-clause BSD */

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <chrono>
#include <filesystem>
#include <iostream>
#include <sstream>

#ifndef _MSC_VER
#include <errno.h>
#include <unistd.h>
#endif

#include <s7.h>

using std::filesystem::exists;
using std::filesystem::path;

static s7_pointer
g_current_second (s7_scheme* sc, s7_pointer args) {
  auto now= std::chrono::system_clock::now ();
  // TODO: use std::chrono::tai_clock::now() when using C++ 20
  auto      now_duration= now.time_since_epoch ();
  double    ts          = std::chrono::duration<double> (now_duration).count ();
  s7_double res         = ts;
  return s7_make_real (sc, res);
}

static void
glue_scheme_time (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_define (sc, cur_env, s7_make_symbol (sc, "current-second"),
             s7_make_typed_function (sc, "current-second", g_current_second, 0,
                                     0, false, "current-second", NULL));
}

int
main (int argc, char** argv) {
  const path gf_root=
      std::filesystem::path (argv[0]).parent_path ().parent_path ();
  const path gf_lib = gf_root / "goldfish";
  const path gf_boot= gf_lib / "scheme" / "boot.scm";

  if (!exists (gf_lib)) {
    std::cerr
        << "The load path for Goldfish Scheme Standard Library does not exist"
        << std::endl;
  }
  if (!exists (gf_root)) {
    std::cerr << "The boot.scm for Goldfish Scheme does not exist" << std::endl;
  }

  s7_scheme* sc;
  sc= s7_init ();
  s7_load (sc, gf_boot.string ().c_str ());
  s7_add_to_load_path (sc, gf_lib.string ().c_str ());

  glue_scheme_time (sc);

  if (argc >= 2) {
    if (strcmp (argv[1], "-e") == 0) /* repl -e '(+ 1 2)' */
    {
      s7_pointer x;
      x= s7_eval_c_string (sc, argv[2]);
      fprintf (stdout, "%s\n", s7_object_to_c_string (sc, x));
      return (0);
    }
    if (strcmp (argv[1], "--version") == 0) {
      fprintf (stdout, "s7: %s, %s\n", S7_VERSION, S7_DATE);
      return (0);
    }
    errno= 0;
    if (!s7_load (sc, argv[1])) {
      fprintf (stderr, "%s: %s\n", strerror (errno), argv[1]);
      return (2);
    }
  }
  return 0;
}
