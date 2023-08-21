
/******************************************************************************
* MODULE     : block.hpp
* DESCRIPTION: A block of Scheme data
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
                   2023  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BLOCK_HPP
#define BLOCK_HPP

#include "hashmap.hpp"
#include "tree.hpp"

scheme_tree string_to_scheme_tree (string s);
scheme_tree block_to_scheme_tree (string s);
string      scheme_tree_to_string (scheme_tree t);
string      scheme_tree_to_block (scheme_tree t);

#endif
