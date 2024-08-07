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

#include <chrono>
#include <filesystem>
#include <iostream>
#include <s7.h>
#include <string>
#include <tbox/tbox.h>
#include <vector>

#if !defined(_MSC_VER)
#include <errno.h>
#include <wordexp.h>
#endif

const int patch_version= 1;                // Goldfish Patch Version
const int minor_version= S7_MAJOR_VERSION; // S7 Major Version
const int major_version= 17;               // C++ Standard version

static auto command_args= std::vector<std::string> ();

const std::string goldfish_version=
    std::to_string (major_version)
        .append (".")
        .append (std::to_string (minor_version))
        .append (".")
        .append (std::to_string (patch_version));

namespace goldfish {
using std::filesystem::create_directory;
using std::filesystem::exists;
using std::filesystem::filesystem_error;
using std::filesystem::path;
using std::filesystem::remove;
using std::filesystem::temp_directory_path;

// Glues for Goldfish
static s7_pointer
f_version (s7_scheme* sc, s7_pointer args) {
  return s7_make_string (sc, goldfish_version.c_str ());
}

static s7_pointer
f_file_exists (s7_scheme* sc, s7_pointer args) {
  const char* path_c= s7_string (s7_car (args));
  auto        p     = path (path_c);
  bool        ret   = false;
  try {
    ret= exists (p);
  } catch (filesystem_error const& ex) {
    return s7_error (sc, s7_make_symbol (sc, "read-error"),
                     s7_make_string (sc, ex.what ()));
  }
  return s7_make_boolean (sc, ret);
}

static s7_pointer
f_delete_file (s7_scheme* sc, s7_pointer args) {
  const char* path_c= s7_string (s7_car (args));
  auto        p     = path (path_c);
  try {
    remove (p);
  } catch (filesystem_error const& ex) {
    return s7_error (sc, s7_make_symbol (sc, "io-error"),
                     s7_make_string (sc, ex.what ()));
  }

  return s7_make_boolean (sc, s7_make_symbol (sc, "<#unspecified>"));
}

inline void
glue_goldfish (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_version    = "version";
  const char* d_version    = "(version) => string";
  const char* s_file_exists= "g_file-exists?";
  const char* d_file_exists= "(g_file-exists? string) => boolean";
  const char* s_delete_file= "g_delete-file";
  const char* d_delete_file= "(g_delete-file string) => <#unspecified>";

  s7_define (sc, cur_env, s7_make_symbol (sc, s_version),
             s7_make_typed_function (sc, s_version, f_version, 0, 0, false,
                                     d_version, NULL));

  s7_define (sc, cur_env, s7_make_symbol (sc, s_file_exists),
             s7_make_typed_function (sc, s_file_exists, f_file_exists, 1, 0,
                                     false, d_file_exists, NULL));

  s7_define (sc, cur_env, s7_make_symbol (sc, s_delete_file),
             s7_make_typed_function (sc, s_delete_file, f_delete_file, 1, 0,
                                     false, d_delete_file, NULL));
}

// Glues for (scheme time)
static s7_pointer
f_current_second (s7_scheme* sc, s7_pointer args) {
  auto now= std::chrono::system_clock::now ();
  // TODO: use std::chrono::tai_clock::now() when using C++ 20
  auto      now_duration= now.time_since_epoch ();
  double    ts          = std::chrono::duration<double> (now_duration).count ();
  s7_double res         = ts;
  return s7_make_real (sc, res);
}

inline void
glue_scheme_time (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_current_second= "g_current-second";
  const char* d_current_second= "(g_current-second) => double, return the "
                                "current unix timestamp in double";
  s7_define (sc, cur_env, s7_make_symbol (sc, s_current_second),
             s7_make_typed_function (sc, s_current_second, f_current_second, 0,
                                     0, false, d_current_second, NULL));
}

// Glues for (scheme process-context)
static s7_pointer
f_get_environment_variable (s7_scheme* sc, s7_pointer args) {
#ifdef _MSC_VER
  std::string path_sep= ";";
#else
  std::string path_sep= ":";
#endif
  std::string          ret;
  tb_size_t            size       = 0;
  const char*          key        = s7_string (s7_car (args));
  tb_environment_ref_t environment= tb_environment_init ();
  if (environment) {
    size= tb_environment_load (environment, key);
    if (size >= 1) {
      tb_for_all_if (tb_char_t const*, value, environment, value) {
        ret.append (value).append (path_sep);
      }
    }
  }
  tb_environment_exit (environment);
  if (size == 0) { // env key not found
    return s7_make_boolean (sc, false);
  }
  else {
    return s7_make_string (sc, ret.substr (0, ret.size () - 1).c_str ());
  }
}

static s7_pointer
f_command_line (s7_scheme* sc, s7_pointer args) {
  s7_pointer ret = s7_nil (sc);
  int        size= command_args.size ();
  for (int i= size - 1; i >= 0; i--) {
    ret= s7_cons (sc, s7_make_string (sc, command_args[i].c_str ()), ret);
  }
  return ret;
}

inline void
glue_scheme_process_context (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_get_environment_variable= "g_get-environment-variable";
  const char* d_get_environment_variable=
      "(g_get-environemt-variable string) => string";
  const char* s_command_line= "g_command-line";
  const char* d_command_line= "(g_command-line) => string";

  s7_define (sc, cur_env, s7_make_symbol (sc, s_get_environment_variable),
             s7_make_typed_function (sc, s_get_environment_variable,
                                     f_get_environment_variable, 1, 0, false,
                                     d_get_environment_variable, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_command_line),
             s7_make_typed_function (sc, s_command_line, f_command_line, 0, 0,
                                     false, d_command_line, NULL));
}

// Glue for (liii os)
static s7_pointer
f_os_type (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_LINUX
  return s7_make_string (sc, "Linux");
#endif
#ifdef TB_CONFIG_OS_MACOSX
  return s7_make_string (sc, "Darwin");
#endif
#ifdef TB_CONFIG_OS_WINDOWS
  return s7_make_string (sc, "Windows");
#endif
  return s7_make_boolean (sc, false);
}

static s7_pointer
f_os_arch (s7_scheme* sc, s7_pointer args) {
  return s7_make_string (sc, TB_ARCH_STRING);
}

static s7_pointer
f_os_call (s7_scheme* sc, s7_pointer args) {
  const char*       cmd_c= s7_string (s7_car (args));
  tb_process_attr_t attr = {tb_null};
  attr.flags             = TB_PROCESS_FLAG_NO_WINDOW;
  int ret;

#if _MSC_VER
  ret= (int) std::system (cmd_c);
#else
  wordexp_t   p;
  ret= wordexp (cmd_c, &p, 0);
  if (ret != 0) {
    // failed after calling wordexp
  }
  else if (p.we_wordc == 0) {
    wordfree (&p);
    ret= EINVAL;
  }
  else {
    ret= (int) tb_process_run (p.we_wordv[0], (tb_char_t const**) p.we_wordv,
                               &attr);
    wordfree (&p);
  }
#endif
  return s7_make_integer (sc, ret);
}

static s7_pointer
f_os_temp_dir (s7_scheme* sc, s7_pointer args) {
  auto        temp_dir_path= temp_directory_path ().string ();
  const char* temp_dir     = temp_dir_path.c_str ();
  return s7_make_string (sc, temp_dir);
}

static s7_pointer
f_mkdir (s7_scheme* sc, s7_pointer args) {
  const char* dir_c= s7_string (s7_car (args));
  path        dir  = path (dir_c);
  bool        ret  = false;
  try {
    ret= create_directory (dir);
  } catch (filesystem_error const& ex) {
    return s7_error (sc, s7_make_symbol (sc, "io-error"),
                     s7_make_string (sc, ex.what ()));
  }
  return s7_make_boolean (sc, ret);
}

inline void
glue_liii_os (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_os_type    = "g_os-type";
  const char* d_os_type    = "(g_os-type) => string";
  const char* s_os_arch    = "g_os-arch";
  const char* d_os_arch    = "(g_os-arch) => string";
  const char* s_os_call    = "g_os-call";
  const char* d_os_call    = "(g_os-call string) => int";
  const char* s_os_temp_dir= "g_os-temp-dir";
  const char* d_os_temp_dir= "(g_os-temp-dir) => string";
  const char* s_mkdir      = "g_mkdir";
  const char* d_mkdir      = "(g_mkdir string) => boolean";

  s7_define (sc, cur_env, s7_make_symbol (sc, s_os_type),
             s7_make_typed_function (sc, s_os_type, f_os_type, 0, 0, false,
                                     d_os_type, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_os_arch),
             s7_make_typed_function (sc, s_os_arch, f_os_arch, 0, 0, false,
                                     d_os_arch, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_os_call),
             s7_make_typed_function (sc, s_os_call, f_os_call, 1, 0, false,
                                     d_os_call, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_os_temp_dir),
             s7_make_typed_function (sc, s_os_temp_dir, f_os_temp_dir, 0, 0,
                                     false, d_os_call, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_mkdir),
             s7_make_typed_function (sc, s_mkdir, f_mkdir, 1, 0, false, d_mkdir,
                                     NULL));
}

static s7_pointer
f_uuid4 (s7_scheme* sc, s7_pointer args) {
  tb_char_t        uuid[37];
  const tb_char_t* ret= tb_uuid4_make_cstr (uuid, tb_null);
  return s7_make_string (sc, ret);
}

inline void
glue_liii_uuid (s7_scheme* sc) {
  s7_pointer  cur_env= s7_curlet (sc);
  const char* s_uuid4= "g_uuid4";
  const char* d_uuid4= "(g_uuid4) => string";
  s7_define (sc, cur_env, s7_make_symbol (sc, s_uuid4),
             s7_make_typed_function (sc, s_uuid4, f_uuid4, 0, 0, false, d_uuid4,
                                     NULL));
}

} // namespace goldfish
