/******************************************************************************
* MODULE     : wasm_sys_utils.hpp
* DESCRIPTION: dummy functions for interface
* COPYRIGHT  : (C) 2022  Yufeng Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "wasm_sys_utils.hpp"

int wasm_system (string s) {
  cout << "SYSTEM CALL 1:" << s << LF;
  return 0;
}

int wasm_system (string s, string& out) {
  cout << "SYSTEM CALL 2:" << s << LF;
  out = ""; return 0;
};

int wasm_system (string s, string& out, string& err) {
  cout << "SYSTEM CALL 3:" << s << LF;
  out = "";  err= ""; return 0;
}

int wasm_system (array<string> arg,
         array<int> fd_in, array<string> str_in,
         array<int> fd_out, array<string*> str_out) { return 0; }

string wasm_get_login () { return "emscripten";  }
string wasm_get_username () { return "emscripten-user";  }