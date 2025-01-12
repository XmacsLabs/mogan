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

#include <algorithm>
#include <cstdlib>
#include <cstring>
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
#elif TB_CONFIG_OS_MACOSX
#include <limits.h>
#include <mach-o/dyld.h>
#else
#include <linux/limits.h>
#endif

#if !defined(TB_CONFIG_OS_WINDOWS)
#include <errno.h>
#include <pwd.h>
#include <unistd.h>
#include <wordexp.h>
#endif

#define GOLDFISH_VERSION "17.11.2"

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

inline void
glue_define (s7_scheme *sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func= s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

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

static s7_pointer
f_unset_environment_variable (s7_scheme* sc, s7_pointer args) {
  const char* env_name= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_environment_remove (env_name));
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

string
goldfish_exe () {
#ifdef TB_CONFIG_OS_WINDOWS
  char buffer[GOLDFISH_PATH_MAXN];
  GetModuleFileName (NULL, buffer, GOLDFISH_PATH_MAXN);
  return string (buffer);
#elif TB_CONFIG_OS_MACOSX
  char        buffer[PATH_MAX];
  uint32_t    size= sizeof (buffer);
  if (_NSGetExecutablePath (buffer, &size) == 0) {
    char real_path[GOLDFISH_PATH_MAXN];
    if (realpath (buffer, real_path) != NULL) {
      return string (real_path);
    }
  }
  return "";
#elif TB_CONFIG_OS_LINUX
  char    buffer[GOLDFISH_PATH_MAXN];
  ssize_t len= readlink ("/proc/self/exe", buffer, sizeof (buffer) - 1);
  if (len != -1) {
    buffer[len]= '\0';
    return std::string (buffer);
  }
  return "";
#endif
}

static s7_pointer
f_executable (s7_scheme* sc, s7_pointer args) {
  string exe_path= goldfish_exe ();
  return s7_make_string (sc, exe_path.c_str ());
}

inline void
glue_executable (s7_scheme* sc) {
  const char* name= "g_executable";
  const char* desc= "(g_executable) => string";
  glue_define (sc, name, desc, f_executable, 0, 0);
}

inline void
glue_liii_sys (s7_scheme* sc) {
  glue_executable (sc);
}

static s7_pointer
f_os_arch (s7_scheme* sc, s7_pointer args) {
  return s7_make_string (sc, TB_ARCH_STRING);
}

inline void
glue_os_arch (s7_scheme* sc) {
  const char* name= "g_os-arch";
  const char* desc= "(g_os-arch) => string";
  glue_define (sc, name, desc, f_os_arch, 0, 0);
}

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

inline void
glue_os_type (s7_scheme* sc) {
  const char* name= "g_os-type";
  const char* desc= "(g_os-type) => string";
  glue_define (sc, name, desc, f_os_type, 0, 0);
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
  wordexp_t p;
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

inline void glue_os_call(s7_scheme* sc) {
  const char* name = "g_os-call";
  const char* desc = "(g_os-call string) => int, execute a shell command and return the exit code";
  glue_define(sc, name, desc, f_os_call, 1, 0);
}

static s7_pointer
f_system (s7_scheme* sc, s7_pointer args) {
  const char* cmd_c= s7_string (s7_car (args));
  int         ret  = (int) std::system (cmd_c);
  return s7_make_integer (sc, ret);
}

inline void glue_system(s7_scheme* sc) {
  const char* name = "g_system";
  const char* desc = "(g_system string) => int, execute a shell command and return the exit code";
  glue_define(sc, name, desc, f_system, 1, 0);
}

static s7_pointer
f_access (s7_scheme* sc, s7_pointer args) {
  const char* path_c= s7_string (s7_car (args));
  int         mode  = s7_integer ((s7_cadr (args)));
  bool ret= false;
  if (mode == 0) {
    tb_file_info_t info;
    ret= tb_file_info (path_c, &info);
  } else {
    ret= tb_file_access (path_c, mode);
  }
  
  return s7_make_boolean (sc, ret);
}

inline void glue_access(s7_scheme* sc) {
  const char* name = "g_access";
  const char* desc = "(g_access string integer) => boolean, check file access permissions";
  glue_define(sc, name, desc, f_access, 2, 0);
}

inline void
glue_unsetenv (s7_scheme* sc) {
  const char* name= "g_unsetenv";
  const char* desc= "(g_unsetenv string): string => boolean";
  glue_define (sc, name, desc, f_unset_environment_variable, 1, 0);
}

static s7_pointer
f_os_temp_dir (s7_scheme* sc, s7_pointer args) {
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_temporary (path, GOLDFISH_PATH_MAXN);
  return s7_make_string (sc, path);
}

inline void
glue_os_temp_dir (s7_scheme* sc) {
  const char* name= "g_os-temp-dir";
  const char* desc= "(g_os-temp-dir) => string, get the temporary directory path";
  glue_define (sc, name, desc, f_os_temp_dir, 0, 0);
}

static s7_pointer
f_mkdir (s7_scheme* sc, s7_pointer args) {
  const char* dir_c= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_directory_create (dir_c));
}

inline void glue_mkdir(s7_scheme* sc) {
  const char* name = "g_mkdir";
  const char* desc = "(g_mkdir string) => boolean, create a directory";
  glue_define(sc, name, desc, f_mkdir, 1, 0);
}

static s7_pointer
f_chdir (s7_scheme* sc, s7_pointer args) {
  const char* dir_c= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_directory_current_set (dir_c));
}

inline void glue_chdir(s7_scheme* sc) {
  const char* name = "g_chdir";
  const char* desc = "(g_chdir string) => boolean, change the current working directory";
  glue_define(sc, name, desc, f_chdir, 1, 0);
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

  int    entries_N   = entries.size ();
  string path_s      = string (path_c);
  int    path_N      = path_s.size ();
  int    path_slash_N= path_N;
  char   last_ch     = path_s[path_N - 1];
#if defined(TB_CONFIG_OS_WINDOWS)
  if (last_ch != '/' && last_ch != '\\') {
    path_slash_N= path_slash_N + 1;
  }
#else
  if (last_ch != '/') {
    path_slash_N= path_slash_N + 1;
  }
#endif
  for (int i= 0; i < entries_N; i++) {
    entries[i]= entries[i].substr (path_slash_N);
  }
  return string_vector_to_s7_vector (sc, entries);
}

inline void
glue_listdir (s7_scheme* sc) {
  const char* name= "g_listdir";
  const char* desc= "(g_listdir string) => vector, list the contents of a directory";
  glue_define (sc, name, desc, f_listdir, 1, 0);
}

static s7_pointer
f_getcwd (s7_scheme* sc, s7_pointer args) {
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_current (path, GOLDFISH_PATH_MAXN);
  return s7_make_string (sc, path);
}

inline void
glue_getcwd (s7_scheme* sc) {
  const char* name= "g_getcwd";
  const char* desc= "(g_getcwd) => string, get the current working directory";
  glue_define (sc, name, desc, f_getcwd, 0, 0);
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

inline void
glue_getlogin (s7_scheme* sc) {
  const char* name= "g_getlogin";
  const char* desc= "(g_getlogin) => string, get the current user's login name";
  glue_define (sc, name, desc, f_getlogin, 0, 0);
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
glue_getpid (s7_scheme* sc) {
  const char* name= "g_getpid";
  const char* desc= "(g_getpid) => integer";
  glue_define (sc, name, desc, f_getpid, 0, 0);
}

inline void
glue_liii_os (s7_scheme* sc) {
  glue_os_arch (sc);
  glue_os_type (sc);
  glue_os_call (sc);
  glue_system (sc);
  glue_access (sc);
  glue_unsetenv (sc);
  glue_getcwd (sc);
  glue_os_temp_dir (sc);
  glue_mkdir (sc);
  glue_chdir (sc);
  glue_listdir (sc);
  glue_getlogin (sc);
  glue_getpid (sc);
}

static s7_pointer
f_uuid4 (s7_scheme* sc, s7_pointer args) {
  tb_char_t        uuid[37];
  const tb_char_t* ret= tb_uuid4_make_cstr (uuid, tb_null);
  return s7_make_string (sc, ret);
}

inline void
glue_uuid4 (s7_scheme* sc) {
  const char* name= "g_uuid4";
  const char* desc= "(g_uuid4) => string";
  glue_define (sc, name, desc, f_uuid4, 0, 0);
}

inline void
glue_liii_uuid (s7_scheme* sc) {
  glue_uuid4 (sc);
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

inline void
glue_isdir (s7_scheme* sc) {
  const char* name= "g_isdir";
  const char* desc= "(g_isdir string) => boolean";
  glue_define (sc, name, desc, f_isdir, 1, 0);
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

inline void
glue_isfile (s7_scheme* sc) {
  const char* name= "g_isfile";
  const char* desc= "(g_isfile string) => boolean";
  glue_define (sc, name, desc, f_isfile, 1, 0);
}

static s7_pointer
f_path_getsize (s7_scheme* sc, s7_pointer args) {
  const char*    path_c= s7_string (s7_car (args));
  tb_file_info_t info;
  if (tb_file_info (path_c, &info)) {
    return s7_make_integer (sc, (int) info.size);
  }
  else {
    return s7_make_integer (sc, (int) -1);
  }
}

inline void
glue_path_getsize (s7_scheme* sc) {
  const char* name= "g_path-getsize";
  const char* desc= "(g_path_getsize string): string => integer";
  glue_define (sc, name, desc, f_path_getsize, 1, 0);
}

static s7_pointer f_path_read_text(s7_scheme* sc, s7_pointer args) {
  const char* path = s7_string (s7_car (args));
  if (!path) {
    return s7_make_boolean(sc, false);
  }

  tb_file_ref_t file = tb_file_init(path, TB_FILE_MODE_RO);
  if (file == tb_null) {
    // TODO: warning on the tb_file_init failure
    return s7_make_boolean(sc, false);
  }

  tb_file_sync (file);

  tb_size_t size = tb_file_size(file);
  if (size == 0) {
    tb_file_exit (file);
    return s7_make_string (sc, "");
  }

  tb_byte_t* buffer = new tb_byte_t[size + 1];
  tb_size_t real_size = tb_file_read (file, buffer, size);
  buffer[real_size] = '\0';

  tb_file_exit(file);
  std::string content (reinterpret_cast<char*>(buffer), real_size);
  delete[] buffer;

  return s7_make_string(sc, content.c_str());
}

inline void
glue_path_read_text(s7_scheme* sc) {
  const char* name = "g_path-read-text";
  const char* desc = "(g_path-read-text path) => string, read the content of the file at the given path";
  s7_define_function(sc, name, f_path_read_text, 1, 0, false, desc);
}

static s7_pointer
f_path_write_text (s7_scheme* sc, s7_pointer args) {
  const char* path = s7_string (s7_car (args));
  if (!path) {
    return s7_make_integer(sc, -1);
  }

  const char* content= s7_string (s7_cadr (args));
  if (!content) {
    return s7_make_integer(sc, -1);
  }

  tb_file_ref_t file = tb_file_init(path, TB_FILE_MODE_WO | TB_FILE_MODE_CREAT | TB_FILE_MODE_TRUNC);
  if (file == tb_null) {
    return s7_make_integer(sc, -1);
  }

  tb_filelock_ref_t lock = tb_filelock_init(file);
  if (tb_filelock_enter(lock, TB_FILELOCK_MODE_EX) == tb_false) {
    tb_filelock_exit(lock);
    tb_file_exit(file);
    return s7_make_integer(sc, -1);
  }

  tb_size_t content_size= strlen(content);
  tb_size_t written_size= tb_file_writ(file, reinterpret_cast<const tb_byte_t*>(content), content_size);

  bool release_success= tb_filelock_leave (lock);
  tb_filelock_exit (lock);
  bool exit_success= tb_file_exit(file);

  if (written_size == content_size && release_success && exit_success) {
    return s7_make_integer(sc, written_size);
  } else {
    return s7_make_integer(sc, -1);
  }
}

inline void glue_path_write_text(s7_scheme* sc) {
  const char* name = "g_path-write-text";
  const char* desc = "(g_path-write-text path content) => integer,\
write content to the file at the given path and return the number of bytes written, or -1 on failure";
  s7_define_function(sc, name, f_path_write_text, 2, 0, false, desc);
}

inline void
glue_liii_path (s7_scheme* sc) {
  glue_isfile (sc);
  glue_isdir (sc);
  glue_path_getsize (sc);
  glue_path_read_text (sc);
  glue_path_write_text (sc);
}

void
glue_for_community_edition (s7_scheme* sc) {
  glue_goldfish (sc);
  glue_scheme_time (sc);
  glue_scheme_process_context (sc);
  glue_liii_sys (sc);
  glue_liii_os (sc);
  glue_liii_path (sc);
  glue_liii_uuid (sc);
}

static void
display_help () {
  cout << "Goldfish Scheme " << GOLDFISH_VERSION << " by LiiiLabs" << endl;
  cout << "--version\t"
       << "Display version" << endl;
  cout << "-m default\t"
       << "Allowed mode: default, liii, sicp, r7rs, s7" << endl;
  cout << "-e       \t"
       << "Load the scheme code on the command line" << endl
       << "\t\teg. -e '(begin (display `Hello) (+ 1 2))'" << endl;
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

s7_scheme*
init_goldfish_scheme (const char* gf_lib) {
  s7_scheme* sc= s7_init ();
  s7_add_to_load_path (sc, gf_lib);

  if (!tb_init (tb_null, tb_null)) exit (-1);

  glue_for_community_edition (sc);
  return sc;
}

void
customize_goldfish_by_mode (s7_scheme* sc, string mode,
                            const char* boot_file_path) {
  if (mode != "s7") {
    s7_load (sc, boot_file_path);
  }

  if (mode == "default" || mode == "liii") {
    s7_eval_c_string (sc, "(import (liii base) (liii error) (liii lang))");
  }
  else if (mode == "scheme") {
    s7_eval_c_string (sc, "(import (liii base) (liii error))");
  }
  else if (mode == "sicp") {
    s7_eval_c_string (sc, "(import (scheme base) (srfi sicp))");
  }
  else if (mode == "r7rs") {
    s7_eval_c_string (sc, "(import (scheme base))");
  }
  else if (mode == "s7") {
  }
  else {
    cerr << "No such mode: " << mode << endl;
    exit (-1);
  }
}

string
find_goldfish_library () {
  string exe_path= goldfish_exe ();

  tb_char_t        data_bin[TB_PATH_MAXN]= {0};
  tb_char_t const* ret_bin=
      tb_path_directory (exe_path.c_str (), data_bin, sizeof (data_bin));

  tb_char_t        data_root[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_root=
      tb_path_directory (ret_bin, data_root, sizeof (data_root));

  tb_char_t        data_lib[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_lib= tb_path_absolute_to (gf_root, "share/goldfish",
                                                data_lib, sizeof (data_lib));
#ifdef TB_CONFIG_OS_LINUX
  if (strcmp (gf_root, "/") == 0) {
    gf_lib= "/usr/share/goldfish";
  }
#endif

  if (!tb_file_access (gf_lib, TB_FILE_MODE_RO)) {
    gf_lib=
        tb_path_absolute_to (gf_root, "goldfish", data_lib, sizeof (data_lib));
    if (!tb_file_access (gf_lib, TB_FILE_MODE_RO)) {
      cerr << "The load path for Goldfish standard library does not exist"
           << endl;
      exit (-1);
    }
  }

  return string (gf_lib);
}

string
find_goldfish_boot (const char* gf_lib) {
  tb_char_t        data_boot[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_boot= tb_path_absolute_to (gf_lib, "scheme/boot.scm",
                                                 data_boot, sizeof (data_boot));

  if (!tb_file_access (gf_boot, TB_FILE_MODE_RO)) {
    cerr << "The boot.scm for Goldfish Scheme does not exist" << endl;
    exit (-1);
  }
  return string (gf_boot);
}

int
repl_for_community_edition (s7_scheme* sc, int argc, char** argv) {
  string      gf_lib_dir  = find_goldfish_library ();
  const char* gf_lib      = gf_lib_dir.c_str ();
  string      gf_boot_path= find_goldfish_boot (gf_lib);
  const char* gf_boot     = gf_boot_path.c_str ();

  vector<string> all_args (argv, argv + argc);
  int            all_args_N= all_args.size ();
  for (int i= 0; i < all_args_N; i++) {
    command_args.push_back (all_args[i]);
  }

  // zero args
  vector<string> args (argv + 1, argv + argc);
  if (args.size () == 0) {
    display_help ();
    exit (0);
  }

  const char* errmsg= NULL;
  s7_pointer  old_port=
      s7_set_current_error_port (sc, s7_open_output_string (sc));
  int gc_loc= -1;
  if (old_port != s7_nil (sc)) gc_loc= s7_gc_protect (sc, old_port);

  // -m: Load the standard library by mode
  string mode_flag= "-m";
  string mode     = "default";
  int    args_N   = args.size ();
  int    i;
  for (i= 0; i < args_N; i++) {
    if (args[i] == mode_flag) {
      break;
    }
  }
  if (i < args_N && i + 1 >= args_N) {
    cerr << "No mode specified after -m" << endl;
    exit (-1);
  }
  if (i < args_N) {
    mode= args[i + 1];
    args.erase (args.begin () + i);
    args.erase (args.begin () + i);
  }

  customize_goldfish_by_mode (sc, mode, gf_boot);

  // Command options
  if (args.size () == 1 && args[0].size () > 0 && args[0][0] == '-') {
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
