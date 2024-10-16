
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
  c_string             var_ (var);
  tb_size_t            size       = 0;
  string               ret        = string ("");
  tb_environment_ref_t environment= tb_environment_init ();
  if (environment) {
    size= tb_environment_load (environment, var_);
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
  c_string             var_ (var);
  c_string             with_ (with);
  tb_environment_ref_t environment= tb_environment_init ();
  if (environment) {
    tb_environment_insert (environment, with_, tb_true);
    tb_environment_save (environment, var_);
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

bool
os_wasm () {
#if defined(OS_WASM)
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
  c_string          cmd_ (cmd);
  return (int) tb_process_run_cmd (cmd_, &attr);
#endif
}

int
system (string s, string& result) {
  tb_long_t status= -1;
  // init pipe files
  tb_pipe_file_ref_t file[2]= {0};
  if (!tb_pipe_file_init_pair (file, tb_null, 0)) {
    return status;
  }

  // init process
  c_string          cmd_ (s);
  tb_process_attr_t attr  = {0};
  attr.out.pipe           = file[1];
  attr.outtype            = TB_PROCESS_REDIRECT_TYPE_PIPE;
  tb_process_ref_t process= tb_process_init_cmd (cmd_, &attr);
  if (process) {
    // read pipe data
    tb_size_t read= 0;
    // TODO: should be a config here
    tb_byte_t data[8192];
    tb_size_t size= sizeof (data);
    tb_bool_t wait= tb_false;
    while (read < size) {
      tb_long_t real= tb_pipe_file_read (file[0], data + read, size - read);
      if (real > 0) {
        read+= real;
        wait= tb_false;
      }
      else if (!real && !wait) {
        // wait pipe
        tb_long_t ok= tb_pipe_file_wait (file[0], TB_PIPE_EVENT_READ, 1000);
        tb_check_break (ok > 0);
        wait= tb_true;
      }
      else break;
    }

    result= as_string ((tb_char_t*) data);

    // wait process
    tb_process_wait (process, &status, -1);

    // exit process
    tb_process_exit (process);
  }

  // exit pipe files
  tb_pipe_file_exit (file[0]);
  tb_pipe_file_exit (file[1]);
  return status;
}

} // namespace lolly
