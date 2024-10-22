/** \file tree.hpp
 *  \copyright GPLv3
 *  \details tree
 *  \author Darcy Shen
 *  \date   2024
 */

#pragma once

#include "hashmap.hpp"
#include "merge_sort.hpp"
#include "observer.hpp"

using lolly::data::A;
using lolly::data::AR;
using lolly::data::arity;
using lolly::data::as_double;
using lolly::data::as_int;
using lolly::data::as_long_int;
using lolly::data::copy;
using lolly::data::is_atomic;
using lolly::data::is_bool;
using lolly::data::is_compound;
using lolly::data::is_double;
using lolly::data::is_int;
using lolly::data::lolly_tree_rep;
using lolly::data::N;
using lolly::data::to_string;
using lolly::data::operator!=;
using lolly::data::operator<<;
using lolly::data::operator*;
using lolly::data::destroy_tree_rep;

typedef tree                     scheme_tree;
typedef lolly_tree_rep<observer> tree_rep;

inline bool
operator== (tree t, int lab) {
  return lolly::data::operator== (t, lab);
}

inline bool
operator== (tree t, string s) {
  return lolly::data::operator== (t, s);
}

inline bool
operator== (tree t, const char* s) {
  return lolly::data::operator== (t, s);
}

inline bool
operator== (tree left, tree right) {
  return lolly::data::operator== (left, right);
}

inline tree
operator<< (tree t, string s) {
  t << tree (s);
  return t;
}

inline int
hash (tree t) {
  if (is_atomic<observer> (t)) return hash (t->label);
  else return t->op ^ hash (A<observer> (t));
}

inline int
hash (array<tree> a) {
  int i, h= 0, n= N (a);
  for (i= 0; i < n; i++) {
    h= (h << 7) + (h >> 25);
    h= h + hash (a[i]);
  }
  return h;
}

inline int
right_index (tree t) {
  return is_atomic (t) ? N (t->label) : 1;
}

inline string
as_string (tree t) {
  return lolly::data::to_string (t);
}

inline tree_rep*
inside (tree t) {
  return t.inside ();
}

inline tree
tree_replace (tree t, tree w, tree b) {
  if (t == w) return b;
  else if (is_atomic (t)) return t;
  else {
    int  i, n= N (t);
    tree r (t, n);
    for (i= 0; i < n; i++)
      r[i]= tree_replace (t[i], w, b);
    return r;
  }
}

struct less_eq_associate {
  static inline bool leq (tree& a, tree& b) {
    return as_string (a[0]) <= as_string (b[0]);
  }
};

template <class T, class U>
inline tree
make_collection (hashmap<T, U> h) {
  tree        t= as_tree (h);
  array<tree> a= A (t);
  merge_sort_leq<tree, less_eq_associate> (a);
  int i, n= N (a);
  for (i= 0; i < n; i++)
    t[i]= a[i];
  return t;
}
