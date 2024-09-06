
/******************************************************************************
 * MODULE     : sys_utils.cpp
 * DESCRIPTION: file handling
 * COPYRIGHT  : (C) 1999-2016  Joris van der Hoeven, Denis Raux
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "sys_utils.hpp"

#if defined(OS_MINGW) || defined(OS_WIN)
#include "Windows/win_sys_utils.hpp"
#endif

#if defined(OS_LINUX) || defined(OS_MACOS)
#include "Unix/unix_sys_utils.hpp"
#endif

#include "tbox/tbox.h"

string
get_env (string var) {
  tb_size_t            size       = 0;
  string               ret        = string ("");
  tb_environment_ref_t environment= tb_environment_init ();
  if (environment) {
    size= tb_environment_load (environment, as_charp (var));
    if (size >= 1) {
      tb_for_all_if (tb_char_t const*, value, environment, value) {
        ret= ret * string (value) * URL_SEPARATOR;
      }
    }
  }
  tb_environment_exit (environment);

  if (size <= 0) {
    return ret;
  }
  else {
    return ret (0, N (ret) - 1);
  }
}

void
set_env (string var, string with) {
  if (is_empty (with)) {
    return;
  }
  tb_environment_ref_t environment= tb_environment_init ();
  if (environment) {
    tb_environment_insert (environment, as_charp (with), tb_true);
    tb_environment_save (environment, as_charp (var));
    tb_environment_exit (environment);
  }
}

string
get_user_login () {
#if defined(OS_MINGW) || defined(OS_WIN)
  return get_env ("USERNAME");
#endif

#if defined(OS_LINUX) || defined(OS_MACOS)
  return unix_get_login ();
#endif

#if defined(OS_WASM)
  return "wasm_user";
#endif
}

string
get_user_name () {
#if defined(OS_MINGW) || defined(OS_WIN)
  return lolly::win_get_username ();
#endif

#if defined(OS_LINUX) || defined(OS_MACOS)
  return unix_get_username ();
#endif

#if defined(OS_WASM)
  return "wasm_user_name";
#endif
}

bool
os_win () {
#if defined(OS_WIN)
  return true;
#else
  return false;
#endif
}

bool
os_mingw () {
#ifdef OS_MINGW
  return true;
#else
  return false;
#endif
}

bool
os_macos () {
#if defined(OS_MACOS)
  return true;
#else
  return false;
#endif
}

SN
get_process_id () {
#if defined(OS_MINGW) || defined(OS_WIN)
  return win_get_process_id ();
#endif

#if defined(OS_MACOS) || defined(OS_LINUX)
  return unix_get_process_id ();
#endif

#if defined(OS_WASM)
  return 1;
#endif
}

array<string>
evaluate_system (array<string> arg, array<int> fd_in, array<string> in,
                 array<int> fd_out) {
  array<string>  out (N (fd_out));
  array<string*> ptr (N (fd_out));
  for (int i= 0; i < N (fd_out); i++)
    ptr[i]= &(out[i]);

#if defined(OS_MINGW)
  int ret= win_system (arg, fd_in, in, fd_out, ptr);
#endif

#if defined(OS_LINUX) || defined(OS_MACOS)
  int ret= unix_system (arg, fd_in, in, fd_out, ptr);
#endif

#if defined(OS_WIN) || defined(OS_WASM)
  int ret= -1;
#endif

  return append (as_string (ret), out);
}

namespace lolly {
void
init_tbox () {
  if (!tb_init (tb_null, tb_null)) exit (-1);
}

int
system (string cmd) {
#ifdef OS_WASM
  return -1;
#else
  tb_process_attr_t attr= {0};
  return (int) tb_process_run_cmd (as_charp (cmd), &attr);
#endif
}
} // namespace lolly
