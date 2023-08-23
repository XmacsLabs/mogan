
#include "tm_block.hpp"
#include "analyze.hpp"
#include "block.hpp"
#include "drd_std.hpp"
#include "tm_debug.hpp"
#include "tree_helper.hpp"

tree
scheme_tree_to_tree (scheme_tree t, hashmap<string, int> codes, bool flag) {
  if (is_atomic (t)) return scm_unquote (t->label);
  else if ((N (t) == 0) || is_compound (t[0])) {
    convert_error << "Invalid scheme tree " << t << "\n";
    return compound (
        "errput", concat ("The tree was ", as_string (L (t)), ": ", tree (t)));
  }
  else {
    int        i, n= N (t);
    tree_label code= (tree_label) codes[t[0]->label];
    if (flag) code= make_tree_label (t[0]->label);
    if (code == UNKNOWN) {
      tree u (EXPAND, n);
      u[0]= copy (t[0]);
      for (i= 1; i < n; i++)
        u[i]= scheme_tree_to_tree (t[i], codes, flag);
      return u;
    }
    else {
      tree u (code, n - 1);
      for (i= 1; i < n; i++)
        u[i - 1]= scheme_tree_to_tree (t[i], codes, flag);
      return u;
    }
  }
}

tree
scheme_tree_to_tree (scheme_tree t) {
  return scheme_tree_to_tree (t, STD_CODE, true);
}

tree
scheme_to_tree (string s) {
  return scheme_tree_to_tree (string_to_scheme_tree (s));
}

/******************************************************************************
 * Conversion from trees to scheme trees
 ******************************************************************************/

scheme_tree
tree_to_scheme_tree (tree t) {
  if (is_atomic (t)) return scm_quote (t->label);
  else if (is_func (t, EXPAND) && is_atomic (t[0])) {
    int  i, n= N (t);
    tree u (TUPLE, n);
    u[0]= copy (t[0]);
    for (i= 1; i < n; i++)
      u[i]= tree_to_scheme_tree (t[i]);
    return u;
  }
  else {
    int    i, n= N (t);
    tree   u (TUPLE, n + 1);
    string s= as_string (L (t));
    if (N (s) > 0 && is_digit (s[0]))
      if (is_int (s)) s= "'" * s;
    u[0]= copy (s);
    for (i= 0; i < n; i++)
      u[i + 1]= tree_to_scheme_tree (t[i]);
    return u;
  }
}

/******************************************************************************
 * Conversion of trees to scheme strings
 ******************************************************************************/

string
tree_to_scheme (tree t) {
  return scheme_tree_to_string (tree_to_scheme_tree (t));
}