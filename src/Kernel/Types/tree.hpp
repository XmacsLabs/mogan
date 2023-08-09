
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

#include "tree_label.hpp"
#include "observer.hpp"
#include "array.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"

// using enum tree_label;

/******************************************************************************
* The tree class 'tree'
******************************************************************************/

class pre_tree;
class tree_rep;
class atomic_rep;
class compound_rep;
class generic_rep;
class blackbox;
template<class T> class iterator;
template<class T> class hashset;
template<class T, class U> class hashmap;
template<class T, class U> class hashentry;

pre_tree copy (pre_tree t);

class pre_tree {
  tree_rep* rep; // can be atomic or compound or generic
  inline pre_tree (tree_rep* rep2);

public:
  inline pre_tree (const pre_tree& x);
  inline ~pre_tree ();
  inline atomic_rep* operator -> ();
  inline pre_tree& operator = (pre_tree x);

  inline pre_tree ();
  inline pre_tree (string l);
  inline pre_tree (const char* l);
  inline pre_tree (int l, int n=0);
  inline pre_tree (int l, array<pre_tree> a);
  inline pre_tree (pre_tree t, int n);
  pre_tree (int l, pre_tree t1);
  pre_tree (int l, pre_tree t1, pre_tree t2);
  pre_tree (int l, pre_tree t1, pre_tree t2, pre_tree t3);
  pre_tree (int l, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4);
  pre_tree (int l, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5);
  pre_tree (int l, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5, pre_tree t6);
  pre_tree (int l, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4,
	              pre_tree t5, pre_tree t6, pre_tree t7);
  pre_tree (int l, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4,
	              pre_tree t5, pre_tree t6, pre_tree t7, pre_tree t8);
  inline pre_tree& operator [] (int i);
  pre_tree operator () (int start, int end);

  friend inline int N (pre_tree t);
  friend inline int arity (pre_tree t);
//  friend inline tree_label L (tree t);
//  friend inline tree_label& LR (tree t);
  friend inline array<pre_tree> A (pre_tree t);
  friend inline array<pre_tree>& AR (pre_tree t);
  friend inline bool is_atomic (pre_tree t);
  friend inline bool is_compound (pre_tree t);
  friend inline bool is_generic (pre_tree t);
  friend inline bool operator == (pre_tree t, int lab);
  friend inline bool operator != (pre_tree t, int lab);
  friend inline bool operator == (pre_tree t, string s);
  friend inline bool operator != (pre_tree t, string s);
  friend inline bool operator == (pre_tree t, const char* s);
  friend inline bool operator != (pre_tree t, const char* s);
  friend inline tree_rep* inside (pre_tree t);
  friend inline bool strong_equal (pre_tree t, pre_tree u);
  friend inline bool is_func (pre_tree t, int l);
  friend inline bool is_func (pre_tree t, int l, int i);

  friend pre_tree copy (pre_tree t);
  friend pre_tree freeze (pre_tree t);
  friend bool operator == (pre_tree t, pre_tree u);
  friend bool operator != (pre_tree t, pre_tree u);
  friend pre_tree& operator << (pre_tree& t, pre_tree t2);
  friend pre_tree& operator << (pre_tree& t, array<pre_tree> a);
  friend tm_ostream& operator << (tm_ostream& out, pre_tree t);
  friend pre_tree operator * (pre_tree t1, pre_tree t2);
  friend void print_tree (pre_tree t, int tab);
  friend list<pre_tree> as_trees (list<pointer> l);
  friend class tree_pointer_rep;
  friend class tree_position_rep;
  friend class tree_addendum_rep;
  friend class edit_observer_rep;
  friend class undo_observer_rep;
  friend class tree_links_rep;
  friend class link_repository_rep;
#ifdef QTTEXMACS
  friend class QTMTreeModel;  // hack: wouldn't need it with a widget_observer
#endif
  friend blackbox as_blackbox (const pre_tree& t);
};

class tree_rep: concrete_struct {
public:
  int op;
  observer obs;
  inline tree_rep (int op2): op (op2) {}
  friend class pre_tree;
};

class atomic_rep: public tree_rep {
public:
  string label;
  inline atomic_rep (string l): tree_rep (/*STRING*/ 0), label (l) {}
  friend class pre_tree;
};

class compound_rep: public tree_rep {
public:
  array<pre_tree> a;
  inline compound_rep (int l, array<pre_tree> a2): tree_rep (l), a (a2) {}
  friend class pre_tree;
};

// generic_rep in generic_tree.hpp

template<> struct type_helper<pre_tree> {
  static int  id;
  static pre_tree init;
  static inline pre_tree init_val () { return pre_tree (); }
};

typedef pre_tree scheme_tree;

/******************************************************************************
* Routines for trees
******************************************************************************/

#ifdef debug_trees
#define CHECK_ATOMIC(t) \
  if (((t).rep)->op != /*STRING*/ 0) { \
    failed_error << "The tree : " << (t) << "\n"; \
    TM_FAILED ("atomic tree expected"); \
  }
#define CHECK_COMPOUND(t) \
  if (((t).rep)->op == /*STRING*/) { \
    failed_error << "The tree : " << (t) << "\n"; \
    TM_FAILED ("compound tree expected"); \
  }
#else
#define CHECK_ATOMIC(t)
#define CHECK_COMPOUND(t)
#endif

void destroy_tree_rep (tree_rep* rep);
inline pre_tree::pre_tree (tree_rep* rep2): rep (rep2) { rep->ref_count++; }
inline pre_tree::pre_tree (const pre_tree& x): rep (x.rep) { rep->ref_count++; }
inline pre_tree::~pre_tree () {
  if ((--rep->ref_count)==0) { destroy_tree_rep (rep); rep= NULL; } }
inline atomic_rep* pre_tree::operator -> () {
  CHECK_ATOMIC (*this);
  return static_cast<atomic_rep*> (rep); }
inline pre_tree& pre_tree::operator = (pre_tree x) {
  x.rep->ref_count++;
  if ((--rep->ref_count)==0) destroy_tree_rep (rep);
  rep= x.rep;
  return *this; }

inline pre_tree::pre_tree ():
  rep (tm_new<atomic_rep> (string ())) {}
inline pre_tree::pre_tree (const char *s):
  rep (tm_new<atomic_rep> (s)) {}
inline pre_tree::pre_tree (string s):
  rep (tm_new<atomic_rep> (s)) {}
inline pre_tree::pre_tree (int l, int n):
  rep (tm_new<compound_rep> (l, array<pre_tree> (n))) {}
inline pre_tree::pre_tree (int l, array<pre_tree> a):
  rep (tm_new<compound_rep> (l, a)) {}
inline pre_tree::pre_tree (pre_tree t, int n):
  rep (tm_new<compound_rep> (t.rep->op, array<pre_tree> (n))) {
    CHECK_COMPOUND (t); }

inline pre_tree& pre_tree::operator [] (int i) {
  CHECK_COMPOUND (*this);
  return (static_cast<compound_rep*> (rep))->a[i]; }
inline int N (pre_tree t) {
  CHECK_COMPOUND (t);
  return N ((static_cast<compound_rep*> (t.rep))->a); }
inline int arity (pre_tree t) {
  if (t.rep->op == /*STRING*/ 0) return 0;
  else return N ((static_cast<compound_rep*> (t.rep))->a); }
inline int right_index (pre_tree t) {
  return is_atomic (t)? N(t->label): 1; }
// inline tree_label L (tree t) {
//   return static_cast<tree_label> (t.rep->op); 
//   // return t.rep->op;
// }
// inline tree_label& LR (tree t) {
//   return *(tree_label*)(&(t.rep->op));
//   // return (tree_label) (t.rep->op);
//   // return static_cast<tree_label> (t.rep->op);
//   // return t.rep->op;
// }
inline array<pre_tree> A (pre_tree t) {
  CHECK_COMPOUND (t);
  return (static_cast<compound_rep*> (t.rep))->a; }
inline array<pre_tree>& AR (pre_tree t) {
  CHECK_COMPOUND (t);
  return (static_cast<compound_rep*> (t.rep))->a; }

inline bool is_atomic (pre_tree t) { return (((int) t.rep->op) == 0); }
inline bool is_compound (pre_tree t) { return (((int) t.rep->op) > /*STRING*/ 0); }
inline bool is_generic (pre_tree t) { return ((int) t.rep->op) < 0; }

inline bool operator == (pre_tree t, int lab) {
  return (t.rep->op == lab) && (N(t)==0); }
inline bool operator != (pre_tree t, int lab) {
  return (t.rep->op != lab) || (N(t)!=0); }
inline bool operator == (pre_tree t, string s) {
  return (t.rep->op == /*STRING*/ 0) && (t->label == s); }
inline bool operator != (pre_tree t, string s) {
  return (t.rep->op != /*STRING*/ 0) || (t->label != s); }
inline bool operator == (pre_tree t, const char* s) {
  return (t.rep->op == /*STRING*/ 0) && (t->label == s); }
inline bool operator != (pre_tree t, const char* s) {
  return (t.rep->op != /*STRING*/ 0) || (t->label != s); }
inline tree_rep* inside (pre_tree t) {
  return t.rep; }
inline bool strong_equal (pre_tree t, pre_tree u) {
  return t.rep == u.rep; }

inline bool is_func (pre_tree t, int l) {
  return (t.rep->op==l) && (N(t)!=0); }
inline bool is_func (pre_tree t, int l, int i) {
  return (t.rep->op==l) && (N(t)==i); }

inline bool is_bool (pre_tree t) { return is_atomic (t) && is_bool (t->label); }
inline bool is_int (pre_tree t) { return is_atomic (t) && is_int (t->label); }
inline bool is_double (pre_tree t) { return is_atomic (t) && is_double(t->label); }
inline bool is_string (pre_tree t) { return is_atomic (t); }
inline bool as_bool (pre_tree t) {
  if (is_atomic (t)) return as_bool (t->label);
  else return false; }
inline int as_int (pre_tree t) {
  if (is_atomic (t)) return as_int (t->label);
  else return 0; }
inline long int as_long_int (pre_tree t) {
  if (is_atomic (t)) return as_long_int (t->label);
  else return 0; }
inline double as_double (pre_tree t) {
  if (is_atomic (t)) return as_double (t->label);
  else return 0.0; }
inline string as_string (pre_tree t) {
  if (is_atomic (t)) return t->label;
  else return ""; }
string tree_as_string (pre_tree t);
pre_tree replace (pre_tree t, pre_tree w, pre_tree b);
inline pre_tree bool_as_tree (bool f) {
  return (f? pre_tree ("true"): pre_tree ("false")); }

/******************************************************************************
* Compound trees
******************************************************************************/

pre_tree compound (string s);
pre_tree compound (string s, pre_tree t1);
pre_tree compound (string s, pre_tree t1, pre_tree t2);
pre_tree compound (string s, pre_tree t1, pre_tree t2, pre_tree t3);
pre_tree compound (string s, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4);
pre_tree compound (string s, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5);
pre_tree compound (string s, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5, pre_tree t6);
pre_tree compound (string s, array<pre_tree> a);
bool is_compound (pre_tree t, string s);
bool is_compound (pre_tree t, string s, int n);


inline pre_tree verbatim (pre_tree t1) {
  return compound ("verbatim", t1); }

/******************************************************************************
* Miscellaneous
******************************************************************************/

pre_tree   correct (pre_tree t);
int    hash (pre_tree t);

class formatted {
public:
  pre_tree rep;
  inline formatted (pre_tree t): rep (t) {}
  inline formatted (const formatted& f): rep (f.rep) {}
};

void print_tree (pre_tree t, int tab=0);

struct less_eq_associate {
  static inline bool leq (pre_tree& a, pre_tree& b) {
    return as_string(a[0]) <= as_string(b[0]); }
};

template <class T, class U> static pre_tree
make_collection (hashmap<T,U> h) {
  pre_tree t= as_tree (h);
  array<pre_tree> a= A(t);
  merge_sort_leq <pre_tree, less_eq_associate> (a);
  int i, n=N(a);
  for (i=0; i<n; i++) t[i] = a[i];
  return t;
}

#endif // defined TREE_H
