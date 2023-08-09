
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


/******************************************************************************
* Tree predicates
******************************************************************************/

bool
is_document (pre_tree t) {
  return L(t) == DOCUMENT;
}

bool
is_concat (pre_tree t) {
  return L(t) == CONCAT;
}

bool
is_format (pre_tree t) {
  return is_document (t) || is_concat (t);
}

bool
is_formatting (pre_tree t) {
  return (L(t)>=WITH_LIMITS) && (L(t)<=NEW_DPAGE);
}

bool
is_table (pre_tree t) {
  return
    is_func (t, TABLE) || is_func (t, SUBTABLE) ||
    is_func (t, ROW) || is_func (t, CELL);
}

bool
is_table_format (pre_tree t) {
  return is_func (t, TFORMAT);
}

bool
is_multi_paragraph (pre_tree t) {
  switch (L(t)) {
  case DOCUMENT:
    return true;
  case SURROUND:
    return is_multi_paragraph (t[2]);
  case DATOMS:
  case DLINES:
  case DPAGES:
  case WITH:
  case MARK:
  case EXPAND_AS:
  case STYLE_WITH:
  case VAR_STYLE_WITH:
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
    return is_multi_paragraph (t[N(t)-1]);
  case VAR_INCLUDE:
    return true;
  case WITH_PACKAGE:
    return is_multi_paragraph (t[N(t)-1]);
  case LOCUS:
  case CANVAS:
    return is_multi_paragraph (t[N(t)-1]);
  default:
    {
      static hashset<tree_label> inline_set; // FIXME: use drd
      if (N(inline_set) == 0) {
	inline_set->insert (make_tree_label ("footnote"));
	inline_set->insert (make_tree_label ("footnote-anchor"));
	inline_set->insert (make_tree_label ("note-footnote"));
	inline_set->insert (make_tree_label ("note-footnote*"));
	inline_set->insert (make_tree_label ("folded-comment"));
	inline_set->insert (make_tree_label ("script-input"));
	inline_set->insert (make_tree_label ("converter-input"));
      }
      if (L(t) < START_EXTENSIONS) return false;
      else if (inline_set->contains (L(t))) return false;
      else {
	int i, n= N(t);
	for (i=0; i<n; i++)
	  if (is_multi_paragraph (t[i]))
	    return true;
	return false;
      }
    }
  }
}

bool
is_around (pre_tree t) {
  return is_func (t, AROUND, 3) || is_func (t, VAR_AROUND, 3);
}

bool
is_script (pre_tree t) {
  return
    is_func (t, LSUB) || is_func (t, LSUP) ||
    is_func (t, RSUB) || is_func (t, RSUP);
}

bool
is_script (pre_tree t, bool& right) {
  if (is_func (t, LSUB) ||
      is_func (t, LSUP)) { right=false; return true; }
  if (is_func (t, RSUB) ||
      is_func (t, RSUP)) { right=true; return true; }
  return false;
}

bool
is_prime (pre_tree t) {
  return ((L(t) == LPRIME) || (L(t) == RPRIME)) && (N(t) == 1);
}

bool
is_left_script_prime (pre_tree t) {
  return is_func (t, LSUB, 1) || is_func (t, LSUP, 1) ||
         is_func (t, LPRIME, 1);
}

bool
is_right_script_prime (pre_tree t) {
  return is_func (t, RSUB, 1) || is_func (t, RSUP, 1) ||
         is_func (t, RPRIME, 1);
}

bool
is_mod_active (pre_tree t) {
  return (N(t) == 1) && (L(t) >= STYLE_ONLY) && (L(t) <= VAR_INACTIVE);
}

bool
is_mod_active_once (pre_tree t) {
  return (N(t) == 1) &&
    ((L(t) == STYLE_ONLY) || (L(t) == ACTIVE) || (L(t) == INACTIVE));
}

bool
is_graphical_text (pre_tree t) {
  return
    is_func (t, TEXT_AT) ||
    is_func (t, MATH_AT) ||
    is_func (t, DOCUMENT_AT);
}

bool
is_empty (pre_tree t) {
  if (is_atomic (t)) return (t == "");
  if (is_document (t) || is_concat (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (!is_empty (t[i])) return false;
    return is_concat (t) || (n<=1);
  }
  return is_compound (t, "suppressed");
}

bool
is_multi_line (pre_tree t) {
  if (is_atomic (t)) return false;
  else if (is_func (t, DOCUMENT)) return true;
  else if (is_func (t, CONCAT) || is_func (t, TABLE)) return false;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (is_multi_line (t[i])) return true;
    return false;
  }
}

bool
is_extension (tree_label l) {
  return l >= START_EXTENSIONS;
}

bool
is_extension (pre_tree t) {
  return L(t) >= START_EXTENSIONS;
}

bool
is_extension (pre_tree t, int n) {
  return (L(t) >= START_EXTENSIONS) && (N(t) == n);
}
