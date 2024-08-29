
/******************************************************************************
 * MODULE     : observer.cpp
 * DESCRIPTION: Observers of trees
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "blackbox.hpp"
#include "hashmap.hpp"
#include "modification.hpp"

observer nil_observer;

tm_ostream&
operator<< (tm_ostream& out, observer o) {
  out << "<observer";
  if (is_nil (o)) out << " null";
  else o->print (out);
  out << ">";
  return out;
}

void
touch (tree& ref) {
  // cout << "Touch " << ref << "\n";
  if (!is_nil (ref->obs)) ref->obs->touched (ref, path ());
}

/******************************************************************************
 * Default virtual routines
 ******************************************************************************/

void
observer_rep::announce (tree& ref, modification mod) {
  // cout << "Modify: " << mod << "\n";
  switch (mod->k) {
  case MOD_ASSIGN:
    announce_assign (ref, mod->p, mod->t);
    break;
  case MOD_INSERT:
    announce_insert (ref, mod->p, mod->t);
    break;
  case MOD_REMOVE:
    announce_remove (ref, path_up (mod->p), last_item (mod->p));
    break;
  case MOD_SPLIT:
    announce_split (ref, mod->p);
    break;
  case MOD_JOIN:
    announce_join (ref, mod->p);
    break;
  case MOD_ASSIGN_NODE:
    announce_assign_node (ref, mod->p, mod->t->op);
    break;
  case MOD_INSERT_NODE:
    announce_insert_node (ref, mod->p, mod->t);
    break;
  case MOD_REMOVE_NODE:
    announce_remove_node (ref, mod->p);
    break;
  case MOD_SET_CURSOR:
    announce_set_cursor (ref, mod->p, mod->t);
    break;
  }
}

void
observer_rep::done (tree& ref, modification mod) {
  (void) ref;
  (void) mod;
}

void
observer_rep::touched (tree& ref, path p) {
  (void) ref;
  (void) p;
}

void
observer_rep::announce_assign (tree& ref, path p, tree t) {
  (void) ref;
  (void) p;
  (void) t;
}

void
observer_rep::announce_insert (tree& ref, path p, tree ins) {
  (void) ref;
  (void) p;
  (void) ins;
}

void
observer_rep::announce_remove (tree& ref, path p, int nr) {
  (void) ref;
  (void) p;
  (void) nr;
}

void
observer_rep::announce_split (tree& ref, path p) {
  (void) ref;
  (void) p;
}

void
observer_rep::announce_join (tree& ref, path p) {
  (void) ref;
  (void) p;
}

void
observer_rep::announce_assign_node (tree& ref, path p, int op) {
  (void) ref;
  (void) p;
  (void) op;
}

void
observer_rep::announce_insert_node (tree& ref, path p, tree ins) {
  (void) ref;
  (void) p;
  (void) ins;
}

void
observer_rep::announce_remove_node (tree& ref, path p) {
  (void) ref;
  (void) p;
}

void
observer_rep::announce_set_cursor (tree& ref, path p, tree data) {
  (void) ref;
  (void) p;
  (void) data;
}

void
observer_rep::notify_assign (tree& ref, tree t) {
  (void) ref;
  (void) t;
}

void
observer_rep::notify_insert (tree& ref, int pos, int nr) {
  (void) ref;
  (void) pos;
  (void) nr;
}

void
observer_rep::notify_remove (tree& ref, int pos, int nr) {
  (void) ref;
  (void) pos;
  (void) nr;
}

void
observer_rep::notify_split (tree& ref, int pos, tree prev) {
  (void) ref;
  (void) pos;
  (void) prev;
}

void
observer_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  (void) ref;
  (void) t1;
  (void) t2;
}

void
observer_rep::notify_join (tree& ref, int pos, tree next) {
  (void) ref;
  (void) pos;
  (void) next;
}

void
observer_rep::notify_var_join (tree& ref, tree t, int offset) {
  (void) ref;
  (void) t;
  (void) offset;
}

void
observer_rep::notify_assign_node (tree& ref, int op) {
  (void) ref;
  (void) op;
}

void
observer_rep::notify_insert_node (tree& ref, int pos) {
  (void) ref;
  (void) pos;
}

void
observer_rep::notify_remove_node (tree& ref, int pos) {
  (void) ref;
  (void) pos;
}

void
observer_rep::notify_set_cursor (tree& ref, int pos, tree data) {
  (void) ref;
  (void) pos;
  (void) data;
}

void
observer_rep::notify_detach (tree& ref, tree closest, bool right) {
  (void) ref;
  (void) closest;
  (void) right;
}

bool
observer_rep::get_ip (path& ip) {
  (void) ip;
  return false;
}

bool
observer_rep::set_ip (path ip) {
  (void) ip;
  return false;
}

bool
observer_rep::get_position (tree& t, int& index) {
  (void) t;
  (void) index;
  return false;
}

bool
observer_rep::set_position (tree t, int index) {
  (void) t;
  (void) index;
  return false;
}

observer&
observer_rep::get_child (int which) {
  (void) which;
  return nil_observer;
}

list<observer>
observer_rep::get_tree_pointers () {
  return list<observer> ();
}

bool
observer_rep::get_tree (tree& t) {
  (void) t;
  return false;
}

bool
observer_rep::get_contents (int kind, blackbox& bb) {
  (void) kind;
  (void) bb;
  return false;
}

bool
observer_rep::set_highlight (int lan, int col, int start, int end) {
  (void) col;
  (void) start;
  (void) end;
  (void) lan;
  return false;
}

bool
observer_rep::get_highlight (int lan, array<int>& cols) {
  (void) lan;
  (void) cols;
  return false;
}
