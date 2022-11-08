/******************************************************************************
* MODULE     : wasm_sys_utils.hpp
* DESCRIPTION: dummy functions for interface
* COPYRIGHT  : (C) 2022  Yufeng Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WASM_SYS_UTILS_H
#define WASM_SYS_UTILS_H

#include "string.hpp"
#include "array.hpp"

int wasm_system (string);
int wasm_system (string, string&);
int wasm_system (string, string&, string&);

int wasm_system (array<string> arg,
		 array<int> fd_in, array<string> str_in,
		 array<int> fd_out, array<string*> str_out);

string wasm_get_login ();
string wasm_get_username ();

#endif // defined WASM_SYS_UTILS_H