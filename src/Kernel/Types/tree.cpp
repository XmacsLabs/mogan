
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
#include "drd_std.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "tree_helper.hpp"

/******************************************************************************
* Main routines for trees
******************************************************************************/

pre_tree type_helper<pre_tree>::init (UNINIT);
int type_helper<pre_tree>::id  = new_type_identifier ();

void
destroy_tree_rep (tree_rep* rep) {
  if (((int) rep->op) == 0) tm_delete (static_cast<atomic_rep*> (rep));
  else if (((int) rep->op) > 0) tm_delete (static_cast<compound_rep*>(rep));
  else tm_delete (static_cast<generic_rep*>(rep));
}

pre_tree::pre_tree (int l, pre_tree t1):
  rep (tm_new<compound_rep> (l, array<pre_tree> (1)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
}

pre_tree::pre_tree (int l, pre_tree t1, pre_tree t2):
  rep (tm_new<compound_rep> (l, array<pre_tree> (2)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
}

pre_tree::pre_tree (int l, pre_tree t1, pre_tree t2, pre_tree t3):
  rep (tm_new<compound_rep> (l, array<pre_tree> (3)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
}

pre_tree::pre_tree (int l, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4):
  rep (tm_new<compound_rep> (l, array<pre_tree> (4)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
}

pre_tree::pre_tree (int l, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5):
  rep (tm_new<compound_rep> (l, array<pre_tree> (5)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
}

pre_tree::pre_tree (int l,
	    pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5, pre_tree t6):
  rep (tm_new<compound_rep> (l, array<pre_tree> (6)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
  (static_cast<compound_rep*> (rep))->a[5]=t6;
}

pre_tree::pre_tree (int l,
	    pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5, pre_tree t6, pre_tree t7):
  rep (tm_new<compound_rep> (l, array<pre_tree> (7)))
{
  (static_cast<compound_rep*> (rep))->a[0]=t1;
  (static_cast<compound_rep*> (rep))->a[1]=t2;
  (static_cast<compound_rep*> (rep))->a[2]=t3;
  (static_cast<compound_rep*> (rep))->a[3]=t4;
  (static_cast<compound_rep*> (rep))->a[4]=t5;
  (static_cast<compound_rep*> (rep))->a[5]=t6;
  (static_cast<compound_rep*> (rep))->a[6]=t7;
}

pre_tree::pre_tree (int l,
	    pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4,
	    pre_tree t5, pre_tree t6, pre_tree t7, pre_tree t8):
  rep (tm_new<compound_rep> (l, array<pre_tree> (8)))
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

pre_tree
pre_tree::operator () (int begin, int end) {
  int i;
  pre_tree r (rep->op, end-begin);
  for (i=begin; i<end; i++)
    r[i-begin]= (static_cast<compound_rep*> (rep))->a[i];
  return r;
}

bool
operator == (pre_tree t, pre_tree u) {
  if (strong_equal (t, u)) return true;
  return (L(t)==L(u)) &&
    (L(t)==STRING? (t->label==u->label): (A(t)==A(u)));
}

bool
operator != (pre_tree t, pre_tree u) {
  if (strong_equal (t, u)) return false;
  return (L(t)!=L(u)) ||
    (L(t)==STRING? (t->label!=u->label): (A(t)!=A(u)));
}

pre_tree
copy (pre_tree t) {
  if (is_atomic (t)) return pre_tree (copy (t->label));
  else {
    int i, n= N(t);
    pre_tree t2 (t, n);
    for (i=0; i<n; i++) t2[i]= copy (t[i]);
    return t2;
  }
}

pre_tree
freeze (pre_tree t) {
  if (is_atomic (t)) return copy (t->label);
  if (is_func (t, UNFREEZE, 1)) return t[0];
  else {
    int i, n= N(t);
    pre_tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= freeze (t[i]);
    return r;
  }
}

pre_tree
operator * (pre_tree t1, pre_tree t2) {
  int i;
  if (is_atomic (t1)) t1= pre_tree (L(t2), t1);
  if (is_atomic (t2)) t2= pre_tree (L(t1), t2);
  pre_tree r (t1, N(t1)+N(t2));
  for (i=0; i<N(t1); i++) r[i]= t1[i];
  for (i=0; i<N(t2); i++) r[i+N(t1)]= t2[i];
  return r;
}

pre_tree&
operator << (pre_tree& t, pre_tree t2) {
  CHECK_COMPOUND (t);
  (static_cast<compound_rep*> (t.rep))->a << t2;
  return t;
}

pre_tree&
operator << (pre_tree& t, array<pre_tree> a) {
  CHECK_COMPOUND (t);
  (static_cast<compound_rep*> (t.rep))->a << a;
  return t;
}

tm_ostream&
operator << (tm_ostream& out, pre_tree t) {
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
print_tree (pre_tree t, int tab) {
  int i;
  for (i=0; i<tab; i++) cout << " ";
  if (is_atomic (t)) cout << t->label << "\n";
  else {
    cout << as_string (L(t)) << "\n";
    for (i=0; i<N(t); i++) print_tree (t[i], tab+2);
  }
}

int
hash (array<pre_tree> a) {
  int i, h=0, n=N(a);
  for (i=0; i<n; i++) {
    h=(h<<7) + (h>>25);
    h=h + hash(a[i]);
  }
  return h;
}

int
hash (pre_tree t) {
  if (is_atomic (t)) return hash (t->label);
  else return ((int) L(t)) ^ hash (A(t));
}

string
tree_as_string (pre_tree t) {
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

pre_tree
replace (pre_tree t, pre_tree w, pre_tree b) {
  if (t == w) return b;
  else if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    pre_tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= replace (t[i], w, b);
    return r;
  }
}

/******************************************************************************
* Compound trees
******************************************************************************/

pre_tree
compound (string s) {
  return pre_tree (make_tree_label (s));
}

pre_tree
compound (string s, pre_tree t1) {
  return pre_tree (make_tree_label (s), t1);
}

pre_tree
compound (string s, pre_tree t1, pre_tree t2) {
  return pre_tree (make_tree_label (s), t1, t2);
}

pre_tree
compound (string s, pre_tree t1, pre_tree t2, pre_tree t3) {
  return pre_tree (make_tree_label (s), t1, t2, t3);
}

pre_tree
compound (string s, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4) {
  return pre_tree (make_tree_label (s), t1, t2, t3, t4);
}

pre_tree
compound (string s, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5) {
  return pre_tree (make_tree_label (s), t1, t2, t3, t4, t5);
}

pre_tree
compound (string s, pre_tree t1, pre_tree t2, pre_tree t3, pre_tree t4, pre_tree t5, pre_tree t6) {
  return pre_tree (make_tree_label (s), t1, t2, t3, t4, t5, t6);
}

pre_tree
compound (string s, array<pre_tree> a) {
  return pre_tree (make_tree_label (s), a);
}

bool
is_compound (pre_tree t, string s) {
  return as_string (L(t)) == s;
}

bool
is_compound (pre_tree t, string s, int n) {
  return (as_string (L(t)) == s) && (N(t) == n);
}
