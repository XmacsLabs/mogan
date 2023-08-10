
/******************************************************************************
 * MODULE     : tree_helper.hpp
 * DESCRIPTION: helpers of trees
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TREE_HELPER_H
#define TREE_HELPER_H

#include "modification.hpp"
#include "tree.hpp"
#include "tree_label.hpp"

inline tree_label
L (tree t) {
  return static_cast<tree_label> (t->op);
}

tree_label L (modification mod);

inline tree_label&
LR (tree t) {
  return *(tree_label*) (&(t->op));
}
inline string
get_label (tree t) {
  return is_atomic (t) ? t->label : copy (as_string (L (t)));
}

template <class T>
inline tree
as_tree (T x) {
  return (tree) x;
}
template <>
inline tree
as_tree (int x) {
  return as_string (x);
}
template <>
inline tree
as_tree (long int x) {
  return as_string (x);
}
template <>
inline tree
as_tree (double x) {
  return as_string (x);
}
template <>
inline tree
as_tree (pointer x) {
  (void) x;
  return "pointer";
}
template <>
inline tree
as_tree (bool x) {
  return x ? tree ("true") : tree ("false");
}

template <class T>
inline tree
as_tree (list<T> x) {
  list<T> l;
  int     i, n= N (x);
  tree    t (TUPLE, n);
  for (i= 0, l= x; i < n; i++, l= l->next)
    t[i]= as_tree (l->item);
  return t;
}

template <class T>
inline tree
as_tree (array<T> x) {
  int  i, n= N (x);
  tree t (TUPLE, n);
  for (i= 0; i < n; i++)
    t[i]= as_tree (x[i]);
  return t;
}

template <class T>
inline tree
as_tree (iterator<T> x) {
  tree t (TUPLE);
  while (x->busy ()) {
    t << as_tree (x->next ());
  }
  return t;
}

template <class T>
inline tree
as_tree (hashset<T> x) {
  tree        t (COLLECTION);
  iterator<T> iter= iterate (x);
  while (iter->busy ()) {
    t << as_tree (iter->next ());
  }
  return t;
}

template <class T, class U>
inline tree
as_tree (hashentry<T, U> x) {
  return tree (ASSOCIATE, as_tree (x.key), as_tree (x.im));
}

template <class T, class U>
inline tree
as_tree (hashmap<T, U> x) {
  tree        t (COLLECTION);
  iterator<T> iter= iterate (x);
  while (iter->busy ()) {
    T key  = iter->next ();
    U value= x[key];
    t << tree (ASSOCIATE, as_tree (key), as_tree (value));
  }
  return t;
}

inline hashmap<string, tree>
tree_hashmap (tree_label init, tree t) {
  hashmap<string, tree> ret (init);
  int                   i, n= arity (t);
  for (i= 0; i < n; i++)
    if (is_func (t[i], ASSOCIATE, 2)) ret (get_label (t[i][0]))= copy (t[i][1]);
  return ret;
}

/******************************************************************************
 * Tuples
 ******************************************************************************/

inline tree
tuple () {
  return tree (TUPLE);
}
inline tree
tuple (tree t1) {
  return tree (TUPLE, t1);
}
inline tree
tuple (tree t1, tree t2) {
  return tree (TUPLE, t1, t2);
}
inline tree
tuple (tree t1, tree t2, tree t3) {
  return tree (TUPLE, t1, t2, t3);
}
inline tree
tuple (tree t1, tree t2, tree t3, tree t4) {
  return tree (TUPLE, t1, t2, t3, t4);
}
inline tree
tuple (tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (TUPLE, t1, t2, t3, t4, t5);
}

inline bool
is_tuple (tree t) {
  return (L (t) == TUPLE);
}
inline bool
is_tuple (tree t, string s) {
  return (L (t) == TUPLE) && (N (t) >= 1) && (t[0] == s);
}
inline bool
is_tuple (tree t, const char* s) {
  return (L (t) == TUPLE) && (N (t) >= 1) && (t[0] == s);
}
inline bool
is_tuple (tree t, string s, int n) {
  return (L (t) == TUPLE) && (N (t) == (n + 1)) && (t[0] == s);
}
inline bool
is_tuple (tree t, const char* s, int n) {
  return (L (t) == TUPLE) && (N (t) == (n + 1)) && (t[0] == s);
}

/******************************************************************************
 * Compound trees
 ******************************************************************************/

tree compound (string s);
tree compound (string s, tree t1);
tree compound (string s, tree t1, tree t2);
tree compound (string s, tree t1, tree t2, tree t3);
tree compound (string s, tree t1, tree t2, tree t3, tree t4);
tree compound (string s, tree t1, tree t2, tree t3, tree t4, tree t5);
tree compound (string s, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6);
tree compound (string s, array<tree> a);
bool is_compound (tree t, string s);
bool is_compound (tree t, string s, int n);

/******************************************************************************
 * Other frequent markup
 ******************************************************************************/

inline tree
concat () {
  return tree (CONCAT);
}
inline tree
concat (tree t1) {
  return tree (CONCAT, t1);
}
inline tree
concat (tree t1, tree t2) {
  return tree (CONCAT, t1, t2);
}
inline tree
concat (tree t1, tree t2, tree t3) {
  return tree (CONCAT, t1, t2, t3);
}
inline tree
concat (tree t1, tree t2, tree t3, tree t4) {
  return tree (CONCAT, t1, t2, t3, t4);
}
inline tree
concat (tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (CONCAT, t1, t2, t3, t4, t5);
}

inline tree
document () {
  return tree (DOCUMENT);
}
inline tree
document (tree t1) {
  return tree (DOCUMENT, t1);
}
inline tree
document (tree t1, tree t2) {
  return tree (DOCUMENT, t1, t2);
}
inline tree
document (tree t1, tree t2, tree t3) {
  return tree (DOCUMENT, t1, t2, t3);
}
inline tree
document (tree t1, tree t2, tree t3, tree t4) {
  return tree (DOCUMENT, t1, t2, t3, t4);
}
inline tree
document (tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (DOCUMENT, t1, t2, t3, t4, t5);
}

bool is_document (tree t);
bool is_concat (tree t);
bool is_format (tree t);
bool is_formatting (tree t);
bool is_table (tree t);
bool is_table_format (tree t);
bool is_multi_paragraph (tree t);
bool is_around (tree t);
bool is_script (tree t);
bool is_script (tree t, bool& right);
bool is_prime (tree t);
bool is_left_script_prime (tree t);
bool is_right_script_prime (tree t);
bool is_mod_active (tree t);
bool is_mod_active_once (tree t);
bool is_graphical_text (tree t);
bool is_empty (tree t);
bool is_multi_line (tree t);
bool is_extension (tree t);
bool is_extension (tree t, int n);

inline bool
is_applicable (tree t) {
  return is_compound (t) && (N (t) >= 1) &&
         ((L (t) == MACRO) || (L (t) == FUNC) || (L (t) == XMACRO));
}

tree freeze (tree t);
inline tree
verbatim (tree t1) {
  return compound ("verbatim", t1);
}
string tree_as_string (tree t);
void   print_tree (tree t, int tab= 0);

#endif
