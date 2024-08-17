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

#include <cstdlib>
#include <iostream>
#include <s7.h>
#include <string>
#include <vector>

#include <tbox/platform/file.h>
#include <tbox/platform/path.h>
#include <tbox/tbox.h>

#ifdef TB_CONFIG_OS_WINDOWS
#include <io.h>
#include <windows.h>
#else
#include <pwd.h>
#include <unistd.h>
#endif

#if !defined(TB_CONFIG_OS_WINDOWS)
#include <errno.h>
#include <wordexp.h>
#endif

#define GOLDFISH_VERSION "17.10.3"
#define GOLDFISH_PATH_MAXN TB_PATH_MAXN

static std::vector<std::string> command_args= std::vector<std::string> ();

namespace goldfish {
using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::vector;

inline s7_pointer
string_vector_to_s7_vector (s7_scheme* sc, vector<string> v) {
  int        N  = v.size ();
  s7_pointer ret= s7_make_vector (sc, N);
  for (int i= 0; i < N; i++) {
    s7_vector_set (sc, ret, i, s7_make_string (sc, v[i].c_str ()));
  }
  return ret;
}

// Glues for Goldfish
static s7_pointer
f_version (s7_scheme* sc, s7_pointer args) {
  return s7_make_string (sc, GOLDFISH_VERSION);
}

static s7_pointer
f_delete_file (s7_scheme* sc, s7_pointer args) {
  const char* path_c= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_file_remove (path_c));
}

inline void
glue_goldfish (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_version    = "version";
  const char* d_version    = "(version) => string";
  const char* s_delete_file= "g_delete-file";
  const char* d_delete_file= "(g_delete-file string) => boolean";

  s7_define (sc, cur_env, s7_make_symbol (sc, s_version),
             s7_make_typed_function (sc, s_version, f_version, 0, 0, false,
                                     d_version, NULL));

  s7_define (sc, cur_env, s7_make_symbol (sc, s_delete_file),
             s7_make_typed_function (sc, s_delete_file, f_delete_file, 1, 0,
                                     false, d_delete_file, NULL));
}

// Glues for (scheme time)
static s7_pointer
f_current_second (s7_scheme* sc, s7_pointer args) {
  // TODO: use std::chrono::tai_clock::now() when using C++ 20
  tb_timeval_t tp= {0};
  tb_gettimeofday (&tp, tb_null);
  s7_double res= (time_t) tp.tv_sec + (tp.tv_usec / 1000000.0);
  return s7_make_real (sc, res);
}

inline void
glue_scheme_time (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_current_second= "g_current-second";
  const char* d_current_second= "(g_current-second): () => double, return the "
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
f_unset_environment_variable (s7_scheme* sc, s7_pointer args) {
  const char* env_name= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_environment_remove (env_name));
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
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_temporary (path, GOLDFISH_PATH_MAXN);
  return s7_make_string (sc, path);
}

static s7_pointer
f_isdir (s7_scheme* sc, s7_pointer args) {
  const char*    dir_c= s7_string (s7_car (args));
  tb_file_info_t info;
  bool           ret= false;
  if (tb_file_info (dir_c, &info)) {
    switch (info.type) {
    case TB_FILE_TYPE_DIRECTORY:
    case TB_FILE_TYPE_DOT:
    case TB_FILE_TYPE_DOT2:
      ret= true;
    }
  }
  return s7_make_boolean (sc, ret);
}

static s7_pointer
f_isfile (s7_scheme* sc, s7_pointer args) {
  const char*    dir_c= s7_string (s7_car (args));
  tb_file_info_t info;
  bool           ret= false;
  if (tb_file_info (dir_c, &info)) {
    switch (info.type) {
    case TB_FILE_TYPE_FILE:
      ret= true;
    }
  }
  return s7_make_boolean (sc, ret);
}

static s7_pointer
f_mkdir (s7_scheme* sc, s7_pointer args) {
  const char* dir_c= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_directory_create (dir_c));
}

static s7_pointer
f_chdir (s7_scheme* sc, s7_pointer args) {
  const char* dir_c= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_directory_current_set (dir_c));
}

static s7_pointer
f_getcwd (s7_scheme* sc, s7_pointer args) {
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_current (path, GOLDFISH_PATH_MAXN);
  return s7_make_string (sc, path);
}

static tb_long_t
tb_directory_walk_func (tb_char_t const* path, tb_file_info_t const* info,
                        tb_cpointer_t priv) {
  // check
  tb_assert_and_check_return_val (path && info, TB_DIRECTORY_WALK_CODE_END);

  vector<string>* p_v_result= (vector<string>*) priv;
  p_v_result->push_back (string (path));
  return TB_DIRECTORY_WALK_CODE_CONTINUE;
}

static s7_pointer
f_listdir (s7_scheme* sc, s7_pointer args) {
  const char*    path_c= s7_string (s7_car (args));
  vector<string> entries;
  s7_pointer     ret= s7_make_vector (sc, 0);
  tb_directory_walk (path_c, 0, tb_false, tb_directory_walk_func, &entries);

  int entries_N= entries.size ();
  int path_N   = string (path_c).size ();
  for (int i= 0; i < entries_N; i++) {
    entries[i]= entries[i].substr (path_N + 1);
  }
  return string_vector_to_s7_vector (sc, entries);
}

static s7_pointer
f_access (s7_scheme* sc, s7_pointer args) {
  const char* path_c= s7_string (s7_car (args));
  int         mode  = s7_integer ((s7_cadr (args)));
#ifdef TB_CONFIG_OS_WINDOWS
  bool ret= (_access (path_c, mode) == 0);
#else
  bool           ret= (access (path_c, mode) == 0);
#endif
  return s7_make_boolean (sc, ret);
}

static s7_pointer
f_getlogin (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_WINDOWS
  return s7_make_boolean (sc, false);
#else
  uid_t          uid= getuid ();
  struct passwd* pwd= getpwuid (uid);
  return s7_make_string (sc, pwd->pw_name);
#endif
}

static s7_pointer
f_getpid (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_WINDOWS
  return s7_make_integer (sc, (int) GetCurrentProcessId ());
#else
  return s7_make_integer (sc, getpid ());
#endif
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
  const char* s_isdir      = "g_isdir";
  const char* d_isdir      = "(g_isdir string) => boolean";
  const char* s_isfile     = "g_isfile";
  const char* d_isfile     = "(g_isfile string) => boolean";
  const char* s_mkdir      = "g_mkdir";
  const char* d_mkdir      = "(g_mkdir string) => boolean";
  const char* s_chdir      = "g_chdir";
  const char* d_chdir      = "(g_chdir string) => boolean";
  const char* s_listdir    = "g_listdir";
  const char* d_listdir    = "(g_listdir) => vector";
  const char* s_getcwd     = "g_getcwd";
  const char* d_getcwd     = "(g_getcwd) => string";
  const char* s_access     = "g_access";
  const char* d_access     = "(g_access string integer) => boolean";
  const char* s_getlogin   = "g_getlogin";
  const char* d_getlogin   = "(g_getlogin) => string";
  const char* s_getpid     = "g_getpid";
  const char* d_getpid     = "(g_getpid) => integer";
  const char* s_unsetenv   = "g_unsetenv" const char* d_unsetenv=
      "(g_unsetenv string): string => boolean";

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
  s7_define (sc, cur_env, s7_make_symbol (sc, s_isdir),
             s7_make_typed_function (sc, s_isdir, f_isdir, 1, 0, false, d_isdir,
                                     NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_isfile),
             s7_make_typed_function (sc, s_isfile, f_isfile, 1, 0, false,
                                     d_isfile, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_mkdir),
             s7_make_typed_function (sc, s_mkdir, f_mkdir, 1, 0, false, d_mkdir,
                                     NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_chdir),
             s7_make_typed_function (sc, s_chdir, f_chdir, 1, 0, false, d_chdir,
                                     NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_listdir),
             s7_make_typed_function (sc, s_listdir, f_listdir, 1, 0, false,
                                     d_listdir, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_getcwd),
             s7_make_typed_function (sc, s_getcwd, f_getcwd, 0, 0, false,
                                     d_getcwd, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_access),
             s7_make_typed_function (sc, s_access, f_access, 2, 0, false,
                                     d_access, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_getlogin),
             s7_make_typed_function (sc, s_getlogin, f_getlogin, 0, 0, false,
                                     d_access, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_getpid),
             s7_make_typed_function (sc, s_getpid, f_getpid, 0, 0, false,
                                     d_getpid, NULL));
  s7_define (sc, cur_env,
             s7_make_symbol (sc, s_unsetenv, f_unset_environment_variable, 1, 0,
                             false, d_unsetenv, NULL));
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

inline void
glue_for_community_edition (s7_scheme* sc) {
  glue_goldfish (sc);
  glue_scheme_time (sc);
  glue_scheme_process_context (sc);
  glue_liii_os (sc);
  glue_liii_uuid (sc);
}

static void
display_help () {
  cout << "Goldfish Scheme " << GOLDFISH_VERSION << " by LiiiLabs" << endl;
  cout << "--version\t"
       << "display version" << endl;
  cout << "-e       \t"
       << "-e '(+ 1 2)'" << endl;
  cout << "-l FILE  \t"
       << "Load the scheme code on path" << endl;
  cout << "FILE     \t"
       << "Load the scheme code on path and print the evaluated result" << endl;
}

static void
display_version () {
  cout << "Goldfish Scheme " << GOLDFISH_VERSION << " by LiiiLabs" << endl;
  cout << "based on S7 Scheme " << S7_VERSION << " (" << S7_DATE << ")" << endl;
}

static void
display_for_invalid_options () {
  cerr << "Invalid command line options!" << endl << endl;
  display_help ();
}

static void
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

static void
goldfish_eval_code (s7_scheme* sc, string code) {
  s7_pointer x= s7_eval_c_string (sc, code.c_str ());
  cout << s7_object_to_c_string (sc, x) << endl;
}

int
repl_for_community_edition (int argc, char** argv) {
  // Check if the standard library and boot.scm exists
  tb_char_t        data_goldfish[TB_PATH_MAXN]= {0};
  tb_char_t const* goldfish=
      tb_path_absolute (argv[0], data_goldfish, sizeof (data_goldfish));

  tb_char_t        data_bin[TB_PATH_MAXN]= {0};
  tb_char_t const* ret_bin=
      tb_path_directory (goldfish, data_bin, sizeof (data_bin));

  tb_char_t        data_root[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_root=
      tb_path_directory (ret_bin, data_root, sizeof (data_root));

  tb_char_t        data_lib[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_lib=
      tb_path_absolute_to (gf_root, "goldfish", data_lib, sizeof (data_lib));

  tb_char_t        data_boot[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_boot= tb_path_absolute_to (gf_lib, "scheme/boot.scm",
                                                 data_boot, sizeof (data_boot));

  if (!tb_file_access (gf_lib, TB_FILE_MODE_RO)) {
    cerr << "The load path for Goldfish Scheme Standard Library does not exist"
         << endl;
    exit (-1);
  }
  if (!tb_file_access (gf_boot, TB_FILE_MODE_RO)) {
    cerr << "The boot.scm for Goldfish Scheme does not exist" << endl;
    exit (-1);
  }
  vector<string> all_args (argv, argv + argc);
  int            all_args_N= all_args.size ();
  for (int i= 0; i < all_args_N; i++) {
    command_args.push_back (all_args[i]);
  }

  // Init the underlying S7 Scheme and add the load_path
  s7_scheme* sc;
  sc= s7_init ();
  s7_load (sc, gf_boot);
  s7_add_to_load_path (sc, gf_lib);
  const char* errmsg= NULL;
  s7_pointer  old_port=
      s7_set_current_error_port (sc, s7_open_output_string (sc));
  int gc_loc= -1;
  if (old_port != s7_nil (sc)) gc_loc= s7_gc_protect (sc, old_port);

  // Init tbox
  if (!tb_init (tb_null, tb_null)) exit (-1);

  // Glues
  glue_for_community_edition (sc);

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
  else if (args.size () >= 2 && args[0] == "-e") {
    goldfish_eval_code (sc, args[1]);
  }
  else if (args.size () >= 2 && args[0] == "-l") {
    goldfish_eval_file (sc, args[1], true);
  }
  else if (args.size () >= 1 && args[0].size () > 0 && args[0][0] != '-') {
    goldfish_eval_file (sc, args[0], false);
  }
  else {
    display_for_invalid_options ();
  }

  errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
  if ((errmsg) && (*errmsg)) cout << errmsg;
  s7_close_output_port (sc, s7_current_error_port (sc));
  s7_set_current_error_port (sc, old_port);
  if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);

  if ((errmsg) && (*errmsg)) return -1;
  else return 0;
}

} // namespace goldfish
