
/******************************************************************************
 * MODULE     : tree.hpp
 * DESCRIPTION: fixed size trees with reference counting
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TREE_H
#define TREE_H

#include "array.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#include "observer.hpp"
#include "string.hpp"

/******************************************************************************
 * The tree class 'tree'
 ******************************************************************************/

class tree;
class tree_rep;
class atomic_rep;
class compound_rep;
class blackbox;
template <class T> class iterator;
template <class T> class hashset;
template <class T, class U> class hashmap;
template <class T, class U> class hashentry;

tree copy (tree t);

class tree {
  tree_rep* rep; // can be atomic or compound or generic

public:
  inline tree (tree_rep* rep2);
  inline tree (const tree& x);
  inline ~tree ();
  inline atomic_rep* operator->();
  inline tree&       operator= (tree x);

  inline tree ();
  inline tree (string l);
  inline tree (const char* l);
  inline tree (int l, int n= 0);
  inline tree (int l, array<tree> a);
  inline tree (tree t, int n);
  tree (int l, tree t1);
  tree (int l, tree t1, tree t2);
  tree (int l, tree t1, tree t2, tree t3);
  tree (int l, tree t1, tree t2, tree t3, tree t4);
  tree (int l, tree t1, tree t2, tree t3, tree t4, tree t5);
  tree (int l, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6);
  tree (int l, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6, tree t7);
  tree (int l, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6, tree t7,
        tree t8);
  inline tree& operator[] (int i);
  tree         operator() (int start, int end);

  friend inline int          N (tree t);
  friend inline int          arity (tree t);
  friend inline array<tree>  A (tree t);
  friend inline array<tree>& AR (tree t);
  friend inline bool         is_atomic (tree t);
  friend inline bool         is_compound (tree t);
  friend inline bool         is_generic (tree t);
  friend inline bool         operator== (tree t, int lab);
  friend inline bool         operator!= (tree t, int lab);
  friend inline bool         operator== (tree t, string s);
  friend inline bool         operator!= (tree t, string s);
  friend inline bool         operator== (tree t, const char* s);
  friend inline bool         operator!= (tree t, const char* s);
  friend inline tree_rep*    inside (tree t);
  friend inline bool         strong_equal (tree t, tree u);
  friend inline bool         is_func (tree t, int l);
  friend inline bool         is_func (tree t, int l, int i);

  friend tree        copy (tree t);
  friend bool        operator== (tree t, tree u);
  friend bool        operator!= (tree t, tree u);
  friend tree&       operator<< (tree& t, tree t2);
  friend tree&       operator<< (tree& t, array<tree> a);
  friend tm_ostream& operator<< (tm_ostream& out, tree t);
  friend tree        operator* (tree t1, tree t2);
  friend list<tree>  as_trees (list<pointer> l);
  friend blackbox    as_blackbox (const tree& t);
};

class tree_rep : concrete_struct {
public:
  int      op;
  observer obs;
  inline tree_rep (int op2) : op (op2) {}
  friend class tree;
};

class atomic_rep : public tree_rep {
public:
  string label;
  inline atomic_rep (string l) : tree_rep (/*STRING*/ 0), label (l) {}
  friend class tree;
};

class compound_rep : public tree_rep {
public:
  array<tree> a;
  inline compound_rep (int l, array<tree> a2) : tree_rep (l), a (a2) {}
  friend class tree;
};

typedef tree scheme_tree;

/******************************************************************************
 * Routines for trees
 ******************************************************************************/

#ifdef debug_trees
#define CHECK_ATOMIC(t)                                                        \
  if (((t).rep)->op != /*STRING*/ 0) {                                         \
    failed_error << "The tree : " << (t) << "\n";                              \
    TM_FAILED ("atomic tree expected");                                        \
  }
#define CHECK_COMPOUND(t)                                                      \
  if (((t).rep)->op == /*STRING*/) {                                           \
    failed_error << "The tree : " << (t) << "\n";                              \
    TM_FAILED ("compound tree expected");                                      \
  }
#else
#define CHECK_ATOMIC(t)
#define CHECK_COMPOUND(t)
#endif

void destroy_tree_rep (tree_rep* rep);
inline tree::tree (tree_rep* rep2) : rep (rep2) { rep->ref_count++; }
inline tree::tree (const tree& x) : rep (x.rep) { rep->ref_count++; }
inline tree::~tree () {
  if ((--rep->ref_count) == 0) {
    destroy_tree_rep (rep);
    rep= NULL;
  }
}
inline atomic_rep*
tree::operator->() {
  CHECK_ATOMIC (*this);
  return static_cast<atomic_rep*> (rep);
}
inline tree&
tree::operator= (tree x) {
  x.rep->ref_count++;
  if ((--rep->ref_count) == 0) destroy_tree_rep (rep);
  rep= x.rep;
  return *this;
}

inline tree::tree () : rep (tm_new<atomic_rep> (string ())) {}
inline tree::tree (const char* s) : rep (tm_new<atomic_rep> (s)) {}
inline tree::tree (string s) : rep (tm_new<atomic_rep> (s)) {}
inline tree::tree (int l, int n)
    : rep (tm_new<compound_rep> (l, array<tree> (n))) {}
inline tree::tree (int l, array<tree> a) : rep (tm_new<compound_rep> (l, a)) {}
inline tree::tree (tree t, int n)
    : rep (tm_new<compound_rep> (t.rep->op, array<tree> (n))) {
  CHECK_COMPOUND (t);
}

inline tree&
tree::operator[] (int i) {
  CHECK_COMPOUND (*this);
  return (static_cast<compound_rep*> (rep))->a[i];
}
inline int
N (tree t) {
  CHECK_COMPOUND (t);
  return N ((static_cast<compound_rep*> (t.rep))->a);
}
inline int
arity (tree t) {
  if (t.rep->op == /*STRING*/ 0) return 0;
  else return N ((static_cast<compound_rep*> (t.rep))->a);
}
inline int
right_index (tree t) {
  return is_atomic (t) ? N (t->label) : 1;
}
inline array<tree>
A (tree t) {
  CHECK_COMPOUND (t);
  return (static_cast<compound_rep*> (t.rep))->a;
}
inline array<tree>&
AR (tree t) {
  CHECK_COMPOUND (t);
  return (static_cast<compound_rep*> (t.rep))->a;
}

inline bool
is_atomic (tree t) {
  return (((int) t.rep->op) == 0);
}
inline bool
is_compound (tree t) {
  return (((int) t.rep->op) > /*STRING*/ 0);
}
inline bool
is_generic (tree t) {
  return ((int) t.rep->op) < 0;
}
inline bool
operator== (tree t, int lab) {
  return (t.rep->op == lab) && (N (t) == 0);
}
inline bool
operator!= (tree t, int lab) {
  return (t.rep->op != lab) || (N (t) != 0);
}
inline bool
operator== (tree t, string s) {
  return (t.rep->op == /*STRING*/ 0) && (t->label == s);
}
inline bool
operator!= (tree t, string s) {
  return (t.rep->op != /*STRING*/ 0) || (t->label != s);
}
inline bool
operator== (tree t, const char* s) {
  return (t.rep->op == /*STRING*/ 0) && (t->label == s);
}
inline bool
operator!= (tree t, const char* s) {
  return (t.rep->op != /*STRING*/ 0) || (t->label != s);
}
inline tree_rep*
inside (tree t) {
  return t.rep;
}
inline bool
strong_equal (tree t, tree u) {
  return t.rep == u.rep;
}

inline bool
is_func (tree t, int l) {
  return (t.rep->op == l) && (N (t) != 0);
}
inline bool
is_func (tree t, int l, int i) {
  return (t.rep->op == l) && (N (t) == i);
}

inline bool
is_bool (tree t) {
  return is_atomic (t) && is_bool (t->label);
}
inline bool
is_int (tree t) {
  return is_atomic (t) && is_int (t->label);
}
inline bool
is_double (tree t) {
  return is_atomic (t) && is_double (t->label);
}
inline bool
is_string (tree t) {
  return is_atomic (t);
}
inline bool
as_bool (tree t) {
  if (is_atomic (t)) return as_bool (t->label);
  else return false;
}
inline int
as_int (tree t) {
  if (is_atomic (t)) return as_int (t->label);
  else return 0;
}
inline long int
as_long_int (tree t) {
  if (is_atomic (t)) return as_long_int (t->label);
  else return 0;
}
inline double
as_double (tree t) {
  if (is_atomic (t)) return as_double (t->label);
  else return 0.0;
}
inline string
as_string (tree t) {
  if (is_atomic (t)) return t->label;
  else return "";
}
tree replace (tree t, tree w, tree b);

/******************************************************************************
 * Miscellaneous
 ******************************************************************************/

int hash (tree t);

struct less_eq_associate {
  static inline bool leq (tree& a, tree& b) {
    return as_string (a[0]) <= as_string (b[0]);
  }
};

template <class T, class U>
static tree
make_collection (hashmap<T, U> h) {
  tree        t= as_tree (h);
  array<tree> a= A (t);
  merge_sort_leq<tree, less_eq_associate> (a);
  int i, n= N (a);
  for (i= 0; i < n; i++)
    t[i]= a[i];
  return t;
}

template <> struct type_helper<tree> {
  static int         id;
  static inline tree init_val () { return tree (); }
};

#endif // defined TREE_H
