/******************************************************************************
* MODULE     : tm_block.hpp
* DESCRIPTION: A block of Scheme data
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
                   2023  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_BLOCK_HPP
#define TM_BLOCK_HPP

#include "hashmap.hpp"
#include "tree.hpp"

tree        scheme_tree_to_tree (scheme_tree t);
tree        scheme_to_tree (string s);
scheme_tree tree_to_scheme_tree (tree t);
string      tree_to_scheme (tree t);
tree scheme_tree_to_tree (scheme_tree t, hashmap<string, int> codes, bool flag);

#endif