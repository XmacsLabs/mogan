
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
#include "Windows/win_utf8_compat.hpp"
#else
#include "Unix/unix_sys_utils.hpp"
#endif

string
get_env (string var) {
  c_string    _var (var);
  const char* _ret= getenv (_var);
  if (_ret == NULL) {
    if (var == "PWD") return get_env ("HOME");
    return "";
  }
  string ret (_ret);
  return ret;
  // do not delete _ret !
}

void
set_env (string var, string with) {
#if defined(STD_SETENV) && !defined(OS_MINGW)
  c_string _var (var);
  c_string _with (with);
  setenv (_var, _with, 1);
#else
  char* _varw= as_charp (var * "=" * with);
  (void) putenv (_varw);
  // do not delete _varw !!!
  // -> known memory leak, but solution more complex than it is worth
#endif
}

string
get_user_login () {
#if defined(OS_MINGW) || defined(OS_WIN)
  return getenv ("USERNAME");
#else
  return unix_get_login ();
#endif
}

string
get_user_name () {
#if OS_MINGW || defined(OS_WIN)
  return lolly::win_get_username ();
#else // Linux and macOS
  return unix_get_username ();
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

array<string>
evaluate_system (array<string> arg, array<int> fd_in, array<string> in,
                 array<int> fd_out) {
  array<string>  out (N (fd_out));
  array<string*> ptr (N (fd_out));
  for (int i= 0; i < N (fd_out); i++)
    ptr[i]= &(out[i]);

#if defined(OS_MINGW)
  int ret= win_system (arg, fd_in, in, fd_out, ptr);
#elif defined(OS_WIN)
  int ret= -1;
#else
  int ret= unix_system (arg, fd_in, in, fd_out, ptr);
#endif

  return append (as_string (ret), out);
}
