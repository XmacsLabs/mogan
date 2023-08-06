
/******************************************************************************
* MODULE     : tree_helper.cpp
* DESCRIPTION: helpers of trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_helper.hpp"

bool
is_document (tree t) {
  return L(t) == DOCUMENT;
}

bool
is_concat (tree t) {
  return L(t) == CONCAT;
}

bool
is_format (tree t) {
  return is_document (t) || is_concat (t);
}
