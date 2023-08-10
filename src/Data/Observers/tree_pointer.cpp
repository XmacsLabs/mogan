
/******************************************************************************
 * MODULE     : tree_pointer.cpp
 * DESCRIPTION: Persistently point to trees (also used for linking)
 * COPYRIGHT  : (C) 2005  Joris van der Hoeven
 *******************************************************************************
 * An inverse path observer maintains the inverse path of the position
 * of the corresponding tree with respect to the global meta-tree.
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "link.hpp"
#include "list.hpp"
#include "modification.hpp"
#include "observers.hpp"
#include "scheme.hpp"

/******************************************************************************
 * Definition of the tree_pointer_rep class
 ******************************************************************************/

class tree_pointer_rep : public observer_rep {
private:
  tree_rep* ptr;
  bool      flag;
  string    cb;

public:
  tree_pointer_rep (tree ref, bool flag2)
      : ptr (inside (ref)), flag (flag2), cb ("") {}
  tree_pointer_rep (tree ref, string cb2)
      : ptr (inside (ref)), flag (true), cb (cb2) {}
  int         get_type () { return OBSERVER_POINTER; }
  tm_ostream& print (tm_ostream& out) { return out << " pointer"; }

  void announce (tree& ref, modification mod);
  void done (tree& ref, modification mod);
  void touched (tree& ref, path p);

  void notify_assign (tree& ref, tree t);
  void notify_insert (tree& ref, int pos, int nr);
  void notify_remove (tree& ref, int pos, int nr);
  void notify_split (tree& ref, int pos, tree prev);
  void notify_var_split (tree& ref, tree t1, tree t2);
  void notify_join (tree& ref, int pos, tree next);
  void notify_var_join (tree& ref, tree t, int offset);
  void notify_assign_node (tree& ref, int op);
  void notify_insert_node (tree& ref, int pos);
  void notify_remove_node (tree& ref, int pos);
  void notify_detach (tree& ref, tree closest, bool right);

  list<observer> get_tree_pointers ();
  bool           set_tree (tree t);
  bool           get_tree (tree& t);
};

/******************************************************************************
 * Call back routines for announcements
 ******************************************************************************/

void
tree_pointer_rep::announce (tree& ref, modification mod) {
  // cout << "Announce " << mod << "\n";
  (void) ref;
  link_announce (observer (this), mod);
  if (N (cb) != 0 && ip_attached (obtain_ip (ref)))
    call (cb, symbol_object ("announce"), ref, mod);
}

void
tree_pointer_rep::done (tree& ref, modification mod) {
  // cout << "Done " << mod->p << "\n";
  if (N (cb) != 0 && ip_attached (obtain_ip (ref)))
    call (cb, symbol_object ("done"), ref, mod);
}

void
tree_pointer_rep::touched (tree& ref, path p) {
  // cout << "Touched " << p << "\n";
  if (N (cb) != 0 && ip_attached (obtain_ip (ref)))
    call (cb, symbol_object ("touched"), ref, p);
}

/******************************************************************************
 * Specific routines for tree_pointer observers
 ******************************************************************************/

list<observer>
tree_pointer_rep::get_tree_pointers () {
  return list<observer> (observer (this));
}

bool
tree_pointer_rep::get_tree (tree& t) {
  t= tree (ptr);
  return true;
}

bool
tree_pointer_rep::set_tree (tree t) {
  if (ptr != inside (t)) {
    tree ref (ptr);
    remove_observer (ref->obs, observer (this));
    ptr= inside (t);
    insert_observer (t->obs, observer (this));
  }
  return true;
}

/******************************************************************************
 * Call back routines for modifications
 ******************************************************************************/

void
tree_pointer_rep::notify_assign (tree& ref, tree t) {
  // cout << "Notify assign " << ref << ", " << t << "\n";
  (void) ref;
  (void) set_tree (t);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_insert (tree& ref, int pos, int nr) {
  // cout << "Notify insert " << ref << ", " << pos << ", " << nr << "\n";
  (void) ref;
  (void) pos;
  (void) nr;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_remove (tree& ref, int pos, int nr) {
  // cout << "Notify remove " << ref << ", " << pos << ", " << nr << "\n";
  (void) ref;
  (void) pos;
  (void) nr;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_split (tree& ref, int pos, tree prev) {
  (void) ref;
  (void) pos;
  (void) prev;
}

void
tree_pointer_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  // cout << "Notify var split " << ref << ", " << t1 << ", " << t2 << "\n";
  (void) t2;
  (void) ref;
  (void) set_tree (t1); // always at the left
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_join (tree& ref, int pos, tree next) {
  (void) ref;
  (void) pos;
  (void) next;
}

void
tree_pointer_rep::notify_var_join (tree& ref, tree t, int offset) {
  // cout << "Notify var join " << ref << ", " << t << ", " << offset << "\n";
  (void) ref;
  (void) offset;
  (void) set_tree (t);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_assign_node (tree& ref, int op) {
  // cout << "Notify assign node " << ref << ", " << as_string (op) << "\n";
  (void) ref;
  (void) op;
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_insert_node (tree& ref, int pos) {
  // cout << "Notify insert node " << ref << ", " << pos << "\n";
  (void) ref;
  (void) pos;
  if (flag) {
    remove_observer (ref[pos]->obs, observer (this));
    ptr= inside (ref);
    insert_observer (ref->obs, observer (this));
  }
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_remove_node (tree& ref, int pos) {
  // cout << "Notify remove node " << ref << ", " << pos << "\n";
  (void) set_tree (ref[pos]);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

void
tree_pointer_rep::notify_detach (tree& ref, tree closest, bool right) {
  // cout << "Notify detach " << ref << ", " << closest <<", "<< right << "\n";
  (void) right;
  (void) ref;
  (void) set_tree (closest);
  // cout << "position -> " << obtain_position (observer (this)) << "\n";
}

/******************************************************************************
 * Public interface
 ******************************************************************************/

observer
tree_pointer (tree ref, bool flag) {
  return tm_new<tree_pointer_rep> (ref, flag);
}

observer
scheme_observer (tree ref, string cb) {
  return tm_new<tree_pointer_rep> (ref, cb);
}

tree
obtain_tree (observer obs) {
  tree t;
  (void) obs->get_tree (t);
  return t;
}

observer
tree_pointer_new (tree ref) {
  observer obs= tree_pointer (ref);
  attach_observer (ref, obs);
  return obs;
}

void
tree_pointer_delete (observer obs) {
  tree ref= obtain_tree (obs);
  detach_observer (ref, obs);
}
