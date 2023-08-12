
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
#include "tree_label.hpp"

class tree;
class hard_link_rep;
class observer;
class modification;
class blackbox;
template<class T> class list;
template<class T> class array;
typedef hard_link_rep* weak_link;
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
  virtual void announce_assign_node (tree& ref, path p, tree_label op);
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
  virtual void notify_assign_node   (tree& ref, tree_label op);
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

class editor_rep;
class archiver_rep;

extern observer nil_observer;
observer ip_observer (path ip);
observer list_observer (observer o1, observer o2);
observer tree_pointer (tree t, bool flag= false);
observer scheme_observer (tree t, string cb);
observer tree_position (tree t, int index);
observer edit_observer (editor_rep* ed);
observer undo_observer (archiver_rep* arch);
observer highlight_observer (int lan, array<int> cols);

/******************************************************************************
* Modification routines for trees and other observer-related facilities
******************************************************************************/

extern bool busy_modifying;
extern bool busy_versioning;
bool busy_tree (tree& ref);

void assign      (tree& ref, tree t);
void insert      (tree& ref, int pos, tree t);
void remove      (tree& ref, int pos, int nr);
void split       (tree& ref, int pos, int at);
void join        (tree& ref, int pos);
void assign_node (tree& ref, tree_label op);
void insert_node (tree& ref, int pos, tree t);
void remove_node (tree& ref, int pos);
void set_cursor  (tree& ref, int pos, tree data);
void touch       (tree& ref);

void assign      (path p, tree t);
void insert      (path p, tree ins);
void remove      (path p, int nr);
void split       (path p);
void join        (path p);
void assign_node (path p, tree_label op);
void insert_node (path p, tree ins);
void remove_node (path p);
void set_cursor  (path p, tree data);
void touch       (path p);

void insert_observer (observer& o, observer what);
void remove_observer (observer& o, observer what);
void attach_observer (tree& ref, observer o);
void detach_observer (tree& ref, observer o);
void clean_observers (tree& ref);

path obtain_ip (tree& ref);
void attach_ip (tree& ref, path ip);
void detach_ip (tree& ref);
bool ip_attached (path ip);

tree obtain_tree (observer o);
observer tree_pointer_new (tree t);
void tree_pointer_delete (observer o);

path obtain_position (observer o);

observer tree_addendum_new (tree t, int kind, blackbox bb, bool keep= true);
void tree_addendum_delete (observer o);
void tree_addendum_delete (tree t, int type);

observer search_observer (tree& ref, int type);
bool admits_edit_observer (tree t);

void attach_highlight (tree& ref, int lan);
void attach_highlight (tree& ref, int lan, int col, int start, int end);
bool has_highlight (tree& ref, int lan);
array<int> obtain_highlight (tree& ref, int lan);
void detach_highlight (tree& ref, int lan);

void stretched_print (tree t, bool ips= false, int indent= 0);

#endif // defined OBSERVER_H
