
/******************************************************************************
* MODULE     : tree.cpp
* DESCRIPTION: fixed size trees with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "generic_tree.hpp"
#include "hashset.hpp"
#include "iterator.hpp"

/******************************************************************************
* Main routines for trees
******************************************************************************/

tree type_helper<tree>::init (UNINIT);
int type_helper<tree>::id  = new_type_identifier ();

void
destroy_tree_rep (tree_rep* rep) {
  if (((int) rep->op) == 0) tm_delete (static_cast<atomic_rep*> (rep));
  else if (((int) rep->op) > 0) tm_delete (static_cast<compound_rep*>(rep));
  else tm_delete (static_cast<generic_rep*>(rep));
}

tree::tree (int l, tree t1):
  rep (tm_new<compound_rep> (l, array<tree> (1)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
}

tree::tree (int l, tree t1, tree t2):
  rep (tm_new<compound_rep> (l, array<tree> (2)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
}

tree::tree (int l, tree t1, tree t2, tree t3):
  rep (tm_new<compound_rep> (l, array<tree> (3)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
}

tree::tree (int l, tree t1, tree t2, tree t3, tree t4):
  rep (tm_new<compound_rep> (l, array<tree> (4)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
}

tree::tree (int l, tree t1, tree t2, tree t3, tree t4, tree t5):
  rep (tm_new<compound_rep> (l, array<tree> (5)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
}

tree::tree (int l,
	    tree t1, tree t2, tree t3, tree t4, tree t5, tree t6):
  rep (tm_new<compound_rep> (l, array<tree> (6)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
  (static_cast<compound_rep*> (rep))->a[5]=t6;
}

tree::tree (int l,
	    tree t1, tree t2, tree t3, tree t4, tree t5, tree t6, tree t7):
  rep (tm_new<compound_rep> (l, array<tree> (7)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
  (static_cast<compound_rep*> (rep))->a[5]=t6;
  (static_cast<compound_rep*> (rep))->a[6]=t7;
}

tree::tree (int l,
	    tree t1, tree t2, tree t3, tree t4,
	    tree t5, tree t6, tree t7, tree t8):
  rep (tm_new<compound_rep> (l, array<tree> (8)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
  (static_cast<compound_rep*> (rep))->a[5]=t6;
  (static_cast<compound_rep*> (rep))->a[6]=t7;
  (static_cast<compound_rep*> (rep))->a[7]=t8;
}

tree
tree::operator () (int begin, int end) {
  int i;
  tree r (rep->op, end-begin);
  for (i=begin; i<end; i++)
    r[i-begin]= (static_cast<compound_rep*> (rep))->a[i];
  return r;
}

bool
operator == (tree t, tree u) {
  if (strong_equal (t, u)) return true;
  return (L(t)==L(u)) &&
    (L(t)==STRING? (t->label==u->label): (A(t)==A(u)));
}

bool
operator != (tree t, tree u) {
  if (strong_equal (t, u)) return false;
  return (L(t)!=L(u)) ||
    (L(t)==STRING? (t->label!=u->label): (A(t)!=A(u)));
}

tree
copy (tree t) {
  if (is_atomic (t)) return tree (copy (t->label));
  else {
    int i, n= N(t);
    tree t2 (t, n);
    for (i=0; i<n; i++) t2[i]= copy (t[i]);
    return t2;
  }
}

tree
operator * (tree t1, tree t2) {
  int i;
  if (is_atomic (t1)) t1= tree (L(t2), t1);
  if (is_atomic (t2)) t2= tree (L(t1), t2);
  tree r (t1, N(t1)+N(t2));
  for (i=0; i<N(t1); i++) r[i]= t1[i];
  for (i=0; i<N(t2); i++) r[i+N(t1)]= t2[i];
  return r;
}

tree&
operator << (tree& t, tree t2) {
  CHECK_COMPOUND (t);
  (static_cast<compound_rep*> (t.rep))->a << t2;
  return t;
}

tree&
operator << (tree& t, array<tree> a) {
  CHECK_COMPOUND (t);
  (static_cast<compound_rep*> (t.rep))->a << a;
  return t;
}

tm_ostream&
operator << (tm_ostream& out, tree t) {
  if (is_atomic (t)) return out << t->label;
  else if (is_compound (t)) {
    int i, n= N(t);
    out << as_string (L(t));
    if (n==0) return out << "()";
    out << " (";
    for (i=0; i< n-1; i++)
      out << t[i] << ", ";
    out << t[i] << ")";
    return out;
  }
  else out << as_blackbox (t);
  return out;
}

void
print_tree (tree t, int tab) {
  int i;
  for (i=0; i<tab; i++) cout << " ";
  if (is_atomic (t)) cout << t->label << "\n";
  else {
    cout << as_string (L(t)) << "\n";
    for (i=0; i<N(t); i++) print_tree (t[i], tab+2);
  }
}

int
hash (array<tree> a) {
  int i, h=0, n=N(a);
  for (i=0; i<n; i++) {
    h=(h<<7) + (h>>25);
    h=h + hash(a[i]);
  }
  return h;
}

int
hash (tree t) {
  if (is_atomic (t)) return hash (t->label);
  else return ((int) L(t)) ^ hash (A(t));
}

string
tree_as_string (tree t) {
  if (is_atomic (t)) return t->label;
  else if (is_concat (t) || is_document (t)) {
    int i, n= N(t);
    string cumul;
    for (i=0; i<n; i++)
      cumul << tree_as_string (t[i]);
    return cumul;
  }
  else if (is_func (t, WITH))
    return tree_as_string (t[N(t)-1]);
  else if (is_compound (t, "nbsp", 0))
    return " ";
  return "";
}

tree
replace (tree t, tree w, tree b) {
  if (t == w) return b;
  else if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= replace (t[i], w, b);
    return r;
  }
}

/******************************************************************************
* Compound trees
******************************************************************************/

tree
compound (string s) {
  return tree (make_tree_label (s));
}

tree
compound (string s, tree t1) {
  return tree (make_tree_label (s), t1);
}

tree
compound (string s, tree t1, tree t2) {
  return tree (make_tree_label (s), t1, t2);
}

tree
compound (string s, tree t1, tree t2, tree t3) {
  return tree (make_tree_label (s), t1, t2, t3);
}

tree
compound (string s, tree t1, tree t2, tree t3, tree t4) {
  return tree (make_tree_label (s), t1, t2, t3, t4);
}

tree
compound (string s, tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (make_tree_label (s), t1, t2, t3, t4, t5);
}

tree
compound (string s, tree t1, tree t2, tree t3, tree t4, tree t5, tree t6) {
  return tree (make_tree_label (s), t1, t2, t3, t4, t5, t6);
}

tree
compound (string s, array<tree> a) {
  return tree (make_tree_label (s), a);
}

bool
is_compound (tree t, string s) {
  return as_string (L(t)) == s;
}

bool
is_compound (tree t, string s, int n) {
  return (as_string (L(t)) == s) && (N(t) == n);
}
