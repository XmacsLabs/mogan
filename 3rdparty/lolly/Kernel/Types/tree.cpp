
/******************************************************************************
 * MODULE     : tree.cpp
 * DESCRIPTION: fixed size trees with reference counting
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tree.hpp"
#include "generic_tree.hpp"
#include "hashset.hpp"
#include "iterator.hpp"

/******************************************************************************
 * Main routines for trees
 ******************************************************************************/

int type_helper<tree>::id= new_type_identifier ();

void
destroy_tree_rep (tree_rep* rep) {
  if (((int) rep->op) == 0) tm_delete (static_cast<atomic_rep*> (rep));
  else if (((int) rep->op) > 0) tm_delete (static_cast<compound_rep*> (rep));
  else tm_delete (static_cast<generic_rep*> (rep));
}

tree::tree (int l, tree t1) : rep (tm_new<compound_rep> (l, array<tree> (1))) {
  (static_cast<compound_rep*> (rep))->a[0]= t1;
}

tree::tree (int l, tree t1, tree t2)
    : rep (tm_new<compound_rep> (l, array<tree> (2))) {
  (static_cast<compound_rep*> (rep))->a[0]= t1;
  (static_cast<compound_rep*> (rep))->a[1]= t2;
}

tree::tree (int l, tree t1, tree t2, tree t3)
    : rep (tm_new<compound_rep> (l, array<tree> (3))) {
  (static_cast<compound_rep*> (rep))->a[0]= t1;
  (static_cast<compound_rep*> (rep))->a[1]= t2;
  (static_cast<compound_rep*> (rep))->a[2]= t3;
}

tree::tree (int l, tree t1, tree t2, tree t3, tree t4)
    : rep (tm_new<compound_rep> (l, array<tree> (4))) {
  (static_cast<compound_rep*> (rep))->a[0]= t1;
  (static_cast<compound_rep*> (rep))->a[1]= t2;
  (static_cast<compound_rep*> (rep))->a[2]= t3;
  (static_cast<compound_rep*> (rep))->a[3]= t4;
}

tree::tree (int l, tree t1, tree t2, tree t3, tree t4, tree t5)
    : rep (tm_new<compound_rep> (l, array<tree> (5))) {
  (static_cast<compound_rep*> (rep))->a[0]= t1;
  (static_cast<compound_rep*> (rep))->a[1]= t2;
  (static_cast<compound_rep*> (rep))->a[2]= t3;
  (static_cast<compound_rep*> (rep))->a[3]= t4;
  (static_cast<compound_rep*> (rep))->a[4]= t5;
}

tree::tree (int l, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6)
    : rep (tm_new<compound_rep> (l, array<tree> (6))) {
  (static_cast<compound_rep*> (rep))->a[0]= t1;
  (static_cast<compound_rep*> (rep))->a[1]= t2;
  (static_cast<compound_rep*> (rep))->a[2]= t3;
  (static_cast<compound_rep*> (rep))->a[3]= t4;
  (static_cast<compound_rep*> (rep))->a[4]= t5;
  (static_cast<compound_rep*> (rep))->a[5]= t6;
}

tree::tree (int l, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6,
            tree t7)
    : rep (tm_new<compound_rep> (l, array<tree> (7))) {
  (static_cast<compound_rep*> (rep))->a[0]= t1;
  (static_cast<compound_rep*> (rep))->a[1]= t2;
  (static_cast<compound_rep*> (rep))->a[2]= t3;
  (static_cast<compound_rep*> (rep))->a[3]= t4;
  (static_cast<compound_rep*> (rep))->a[4]= t5;
  (static_cast<compound_rep*> (rep))->a[5]= t6;
  (static_cast<compound_rep*> (rep))->a[6]= t7;
}

tree::tree (int l, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6,
            tree t7, tree t8)
    : rep (tm_new<compound_rep> (l, array<tree> (8))) {
  (static_cast<compound_rep*> (rep))->a[0]= t1;
  (static_cast<compound_rep*> (rep))->a[1]= t2;
  (static_cast<compound_rep*> (rep))->a[2]= t3;
  (static_cast<compound_rep*> (rep))->a[3]= t4;
  (static_cast<compound_rep*> (rep))->a[4]= t5;
  (static_cast<compound_rep*> (rep))->a[5]= t6;
  (static_cast<compound_rep*> (rep))->a[6]= t7;
  (static_cast<compound_rep*> (rep))->a[7]= t8;
}

tree
tree::operator() (int begin, int end) {
  int  i;
  tree r (rep->op, end - begin);
  for (i= begin; i < end; i++)
    r[i - begin]= (static_cast<compound_rep*> (rep))->a[i];
  return r;
}

bool
operator== (tree t, tree u) {
  if (strong_equal (t, u)) return true;
  return (t->op == u->op) &&
         (is_atomic (t) ? (t->label == u->label) : (A (t) == A (u)));
}

bool
operator!= (tree t, tree u) {
  if (strong_equal (t, u)) return false;
  return (t->op != u->op) ||
         (is_atomic (t) ? (t->label != u->label) : (A (t) != A (u)));
}

tree
copy (tree t) {
  if (is_atomic (t)) return tree (copy (t->label));
  else {
    int  i, n= N (t);
    tree t2 (t, n);
    for (i= 0; i < n; i++)
      t2[i]= copy (t[i]);
    return t2;
  }
}

tree
operator* (tree t1, tree t2) {
  int i;
  if (is_atomic (t1)) t1= tree (t2->op, t1);
  if (is_atomic (t2)) t2= tree (t1->op, t2);
  tree r (t1, N (t1) + N (t2));
  for (i= 0; i < N (t1); i++)
    r[i]= t1[i];
  for (i= 0; i < N (t2); i++)
    r[i + N (t1)]= t2[i];
  return r;
}

tree&
operator<< (tree& t, tree t2) {
  CHECK_COMPOUND (t);
  (static_cast<compound_rep*> (t.rep))->a << t2;
  return t;
}

tree&
operator<< (tree& t, array<tree> a) {
  CHECK_COMPOUND (t);
  (static_cast<compound_rep*> (t.rep))->a << a;
  return t;
}

tm_ostream&
operator<< (tm_ostream& out, tree t) {
  if (is_atomic (t)) return out << t->label;
  else if (is_compound (t)) {
    int i, n= N (t);
    out << as_string (t->op);
    if (n == 0) return out << "()";
    out << " (";
    for (i= 0; i < n - 1; i++)
      out << t[i] << ", ";
    out << t[i] << ")";
    return out;
  }
  else out << as_blackbox (t);
  return out;
}

int
hash (array<tree> a) {
  int i, h= 0, n= N (a);
  for (i= 0; i < n; i++) {
    h= (h << 7) + (h >> 25);
    h= h + hash (a[i]);
  }
  return h;
}

int
hash (tree t) {
  if (is_atomic (t)) return hash (t->label);
  else return t->op ^ hash (A (t));
}

tree
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
