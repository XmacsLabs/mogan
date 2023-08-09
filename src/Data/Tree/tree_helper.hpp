
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

#include "tree.hpp"
#include "tree_label.hpp"

class tree : pre_tree {
public:

  tree () { pre_tree(); }
  tree (string l) { pre_tree(l); }
  tree (const char* l) { pre_tree(l); }
  tree (int l, int n=0) { pre_tree(l,n); }
  tree (int l, array<pre_tree> a) { pre_tree(l,a); }
  tree (pre_tree t, int n) {pre_tree(t,n);}

  tree (tree_label l, tree t1) {
    pre_tree(static_cast<int>(l), t1);
  }
  tree (tree_label l, tree t1, tree t2) {
    pre_tree(static_cast<int>(l), t1, t2);
  }
  tree (tree_label l, tree t1, tree t2, tree t3) {
    pre_tree(static_cast<int>(l), t1, t2, t3);
  }
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4) {
    pre_tree(static_cast<int>(l), t1, t2, t3, t4);
  }
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4, tree t5) {
    pre_tree(static_cast<int>(l), t1, t2, t3, t4, t5);
  }
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6) {
    pre_tree(static_cast<int>(l), t1, t2, t3, t4, t5, t6);
  }
  tree (tree_label l, tree t1, tree t2, tree t3, tree t4,
	              tree t5, tree t6, tree t7) {
    pre_tree(static_cast<int>(l), t1, t2, t3, t4, t5, t6, t7);
                }
  // tree (int l, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4,
	//               pre_tree t5, pre_tree t6, pre_tree t7, pre_tree t8);
  // friend inline tree_label L (pre_tree t);
  // friend inline tree_label& LR(pre_tree t);
};

inline tree_label L (pre_tree t) {
  return static_cast<tree_label> (t->op);
}
inline tree_label& LR (pre_tree t) {
  return *(tree_label*)(&(t->op));
}

inline string get_label (pre_tree t) {
  return is_atomic (t)? t->label: copy (as_string (L(t))); }

template<class T> inline pre_tree as_tree (T x) { return (pre_tree) x; }
template<> inline pre_tree as_tree (int x) { return as_string (x); }
template<> inline pre_tree as_tree (long int x) { return as_string (x); }
template<> inline pre_tree as_tree (double x) { return as_string (x); }
template<> inline pre_tree as_tree (pointer x) { (void) x; return "pointer"; }

template<class T> inline pre_tree
as_tree (list<T> x) {
  list<T> l;
  int i, n=N(x);
  pre_tree t (TUPLE, n);
  for (i=0, l=x; i<n; i++, l=l->next)
    t[i]= as_tree (l->item);
  return t;
}

template<class T> inline pre_tree
as_tree (array<T> x) {
  int i, n=N(x);
  pre_tree t (TUPLE, n);
  for (i=0; i<n; i++)
    t[i]= as_tree (x[i]);
  return t;
}

template<class T> inline pre_tree
as_tree (iterator<T> x) {
  pre_tree t (TUPLE);
  while (x->busy ()) {
    t << as_tree (x->next());
  }
  return t;
}

template<class T> inline pre_tree
as_tree (hashset<T> x) {
  pre_tree t (COLLECTION);
  iterator<T> iter = iterate (x);
  while (iter->busy ()) {
    t << as_tree (iter->next());
  }
  return t;
}

template<class T, class U> inline pre_tree
as_tree (hashentry<T, U> x) {
  return pre_tree (ASSOCIATE, as_tree (x.key), as_tree (x.im));
}

template<class T, class U> inline pre_tree
as_tree (hashmap<T,U> x) {
  pre_tree t (COLLECTION);
  iterator<T> iter= iterate (x);
  while (iter->busy()) {
    T key= iter->next();
    U value= x[key];
    t << pre_tree (ASSOCIATE, as_tree (key), as_tree (value));
  }
  return t;
}

inline hashmap<string, pre_tree>
tree_hashmap (tree_label init, pre_tree t) {
  hashmap<string, pre_tree> ret (init);
  int i, n= arity (t);
  for (i=0; i<n; i++)
    if (is_func (t[i], ASSOCIATE, 2))
      ret (get_label (t[i][0]))= copy (t[i][1]);
  return ret;
}


/******************************************************************************
* Tuples
******************************************************************************/

inline pre_tree tuple () {
  return pre_tree (TUPLE); }
inline pre_tree tuple (pre_tree t1) {
  return pre_tree (TUPLE, t1); }
inline pre_tree tuple (pre_tree t1, pre_tree t2) {
  return pre_tree (TUPLE, t1, t2); }
inline pre_tree tuple (pre_tree t1, pre_tree t2, pre_tree t3) {
  return pre_tree (TUPLE, t1, t2, t3); }
inline pre_tree tuple (pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4) {
  return pre_tree (TUPLE, t1, t2, t3, t4); }
inline pre_tree tuple (pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5) {
  return pre_tree (TUPLE, t1, t2, t3, t4, t5); }

inline bool is_tuple (pre_tree t) {
  return (L(t) == TUPLE); }
inline bool is_tuple (pre_tree t, string s) {
  return (L(t) == TUPLE) && (N(t) >= 1) && (t[0] == s); }
inline bool is_tuple (pre_tree t, const char* s) {
  return (L(t) == TUPLE) && (N(t) >= 1) && (t[0] == s); }
inline bool is_tuple (pre_tree t, string s, int n) {
  return (L(t) == TUPLE) && (N(t) == (n+1)) && (t[0] == s); }
inline bool is_tuple (pre_tree t, const char* s, int n) {
  return (L(t) == TUPLE) && (N(t) == (n+1)) && (t[0] == s); }


/******************************************************************************
* Other frequent markup
******************************************************************************/

inline pre_tree concat () {
  return pre_tree (CONCAT); }
inline pre_tree concat (pre_tree t1) {
  return pre_tree (CONCAT, t1); }
inline pre_tree concat (pre_tree t1, pre_tree t2) {
  return pre_tree (CONCAT, t1, t2); }
inline pre_tree concat (pre_tree t1, pre_tree t2, pre_tree t3) {
  return pre_tree (CONCAT, t1, t2, t3); }
inline pre_tree concat (pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4) {
  return pre_tree (CONCAT, t1, t2, t3, t4); }
inline pre_tree concat (pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5) {
  return pre_tree (CONCAT, t1, t2, t3, t4, t5); }

inline pre_tree document () {
  return pre_tree (DOCUMENT); }
inline pre_tree document (pre_tree t1) {
  return pre_tree (DOCUMENT, t1); }
inline pre_tree document (pre_tree t1, pre_tree t2) {
  return pre_tree (DOCUMENT, t1, t2); }
inline pre_tree document (pre_tree t1, pre_tree t2, pre_tree t3) {
  return pre_tree (DOCUMENT, t1, t2, t3); }
inline pre_tree document (pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4) {
  return pre_tree (DOCUMENT, t1, t2, t3, t4); }
inline pre_tree document (pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5) {
  return pre_tree (DOCUMENT, t1, t2, t3, t4, t5); }

bool is_document (pre_tree t);
bool is_concat (pre_tree t);
bool is_format (pre_tree t);
bool is_formatting (pre_tree t);
bool is_table (pre_tree t);
bool is_table_format (pre_tree t);
bool is_multi_paragraph (pre_tree t);
bool is_around (pre_tree t);
bool is_script (pre_tree t);
bool is_script (pre_tree t, bool& right);
bool is_prime (pre_tree t);
bool is_left_script_prime (pre_tree t);
bool is_right_script_prime (pre_tree t);
bool is_mod_active (pre_tree t);
bool is_mod_active_once (pre_tree t);
bool is_graphical_text (pre_tree t);
bool is_empty (pre_tree t);
bool is_multi_line (pre_tree t);
bool is_extension (pre_tree t);
bool is_extension (pre_tree t, int n);

inline bool
is_applicable (pre_tree t) {
  return is_compound (t) && (N(t) >= 1) &&
    ((L(t) == MACRO) || (L(t) == FUNC) || (L(t) == XMACRO));
}

#endif
