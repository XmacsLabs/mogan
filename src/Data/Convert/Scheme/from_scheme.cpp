
/******************************************************************************
* MODULE     : to_scheme.cpp
* DESCRIPTION: conversion of scheme expressions to TeXmacs trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "block.hpp"
#include "convert.hpp"
#include "analyze.hpp"
#include "drd_std.hpp"
#include "path.hpp"

/******************************************************************************
* Converting scheme trees to trees
******************************************************************************/

tree
scheme_tree_to_tree (scheme_tree t, string version) {
  version= scm_unquote (version);
  tree doc, error (ERROR, "bad format or data");
  if (version_inf (version, "1.0.2.4"))
    doc= scheme_tree_to_tree (t, get_codes (version), false);
  else doc= scheme_tree_to_tree (t);
  if (!is_document (doc)) return error;
  return upgrade (doc, version);
}

/******************************************************************************
* Converting scheme strings to trees
******************************************************************************/

tree
scheme_document_to_tree (string s) {
  tree error (ERROR, "bad format or data");
  if (starts (s, "(document (apply \"TeXmacs\" ") ||
      starts (s, "(document (expand \"TeXmacs\" ") ||
      starts (s, "(document (TeXmacs "))
  {
    int i, begin=27;
    if (starts (s, "(document (expand \"TeXmacs\" ")) begin= 28;
    if (starts (s, "(document (TeXmacs ")) begin= 19;
    for (i=begin; i<N(s); i++)
      if (s[i] == ')') break;
    string version= s (begin, i);
    tree t  = string_to_scheme_tree (s);
    return scheme_tree_to_tree (t, version);
  }
  return error;
}
