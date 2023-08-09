
/******************************************************************************
* MODULE     : observer.hpp
* DESCRIPTION: Observers of trees
* COPYRIGHT  : (C) 2004  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef OBSERVER_H
#define OBSERVER_H

#include "string.hpp"

class tree;
class observer;
class modification;
class blackbox;
template<class T> class list;
template<class T> class array;
typedef list<int> path;

#define OBSERVER_UNKNOWN    0
#define OBSERVER_LIST       1
#define OBSERVER_IP         2
#define OBSERVER_POINTER    3
#define OBSERVER_POSITION   4
#define OBSERVER_ADDENDUM   5
#define OBSERVER_EDIT       6
#define OBSERVER_UNDO       7
#define OBSERVER_HIGHLIGHT  8
#define OBSERVER_WIDGET     9

#define ADDENDUM_PLAYER     1

/******************************************************************************
* The observer class
******************************************************************************/

extern int observer_count;
class observer_rep: public abstract_struct {
public:
  inline observer_rep () { TM_DEBUG(observer_count++); }
  inline virtual ~observer_rep () { TM_DEBUG(observer_count--); }
  inline virtual int get_type () { return OBSERVER_UNKNOWN; }
  inline virtual tm_ostream& print (tm_ostream& out) { return out; }

  // Announcing modifications in subtrees
  virtual void announce             (tree& ref, modification mod);
  virtual void announce_assign      (tree& ref, path p, tree t);
  virtual void announce_insert      (tree& ref, path p, tree ins);
  virtual void announce_remove      (tree& ref, path p, int nr);
  virtual void announce_split       (tree& ref, path p);
  virtual void announce_join        (tree& ref, path p);
  virtual void announce_assign_node (tree& ref, path p, int op);
  virtual void announce_insert_node (tree& ref, path p, tree ins);
  virtual void announce_remove_node (tree& ref, path p);
  virtual void announce_set_cursor  (tree& ref, path p, tree data);
  virtual void done                 (tree& ref, modification mod);
  virtual void touched              (tree& ref, path p);

  // Call back routines for tree modifications
  virtual void notify_assign        (tree& ref, tree t);
  virtual void notify_insert        (tree& ref, int pos, int nr);
  virtual void notify_remove        (tree& ref, int pos, int nr);
  virtual void notify_split         (tree& ref, int pos, tree prev);
  virtual void notify_var_split     (tree& ref, tree t1, tree t2);
  virtual void notify_join          (tree& ref, int pos, tree next);
  virtual void notify_var_join      (tree& ref, tree t, int offset);
  virtual void notify_assign_node   (tree& ref, int op);
  virtual void notify_insert_node   (tree& ref, int pos);
  virtual void notify_remove_node   (tree& ref, int pos);
  virtual void notify_set_cursor    (tree& ref, int pos, tree data);
  virtual void notify_detach        (tree& ref, tree closest, bool right);

  // Extra routines for particular types of observers
  virtual bool get_ip (path& ip);
  virtual bool set_ip (path ip);
  virtual bool get_position (tree& t, int& index);
  virtual bool set_position (tree t, int index);
  virtual observer& get_child (int which);
  virtual list<observer> get_tree_pointers ();
  virtual bool get_tree (tree& t);
  virtual bool get_contents (int kind, blackbox& bb);
  virtual bool set_highlight (int lan, int col, int start, int end);
  virtual bool get_highlight (int lan, array<int>& cols);
};

class observer {
public:
  ABSTRACT_NULL(observer);
  inline friend bool operator == (observer o1, observer o2) {
    return o1.rep == o2.rep; }
  inline friend bool operator != (observer o1, observer o2) {
    return o1.rep != o2.rep; }
  inline friend int hash (observer o1) {
    return hash ((pointer) o1.rep); }
};
ABSTRACT_NULL_CODE(observer);

tm_ostream& operator << (tm_ostream& out, observer o);

extern observer nil_observer;

/******************************************************************************
* Modification routines for trees and other observer-related facilities
******************************************************************************/

void touch       (tree& ref);

#endif // defined OBSERVER_H
