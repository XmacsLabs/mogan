
/******************************************************************************
 * MODULE     : object.hpp
 * DESCRIPTION: Implementation of scheme objects
 * COPYRIGHT  : (C) 1999-2011 Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef OBJECT_H
#define OBJECT_H

#include "command.hpp"
#include "modification.hpp"
#include "patch.hpp"
#include "path.hpp"
#include "tree.hpp"
#include "url.hpp"

#include "s7_tm.hpp" // interface to S7

class object_rep : concrete_struct {
  friend class object;
};

class tmscm_object_rep;

class object {
public:
  CONCRETE (object);
  object ();
  object (tmscm_object_rep* o);
  object (void*);  // left intentionally undefined to inhibith implicit
                   // conversion of pointers to bool
  object (bool b); // implicit conversion to bool is dangerous!!! (all pointers
                   // match this conversion)
  object (int i);
  object (double x);
  object (const char* s);
  object (string s);
  object (tree t);
  object (list<string> l);
  object (list<tree> l);
  object (path p);
  object (url u);
  object (array<double> a);
  object (modification m);
  object (patch p);
};
CONCRETE_CODE (object);

class tmscm_object_rep : public object_rep {
  tmscm handle;

  tmscm_object_rep (tmscm obj);
  ~tmscm_object_rep ();

  friend class object;
  friend tmscm                                 object_to_tmscm (object o);
  friend object                                tmscm_to_object (tmscm obj);
  template <typename C, typename A1> friend C* tm_new (const A1& a1);
};

inline tmscm
object_to_tmscm (object o) {
  tmscm_object_rep* oo= static_cast<tmscm_object_rep*> (o.operator->());
  return tmscm_caar (oo->handle);
  // return tmscm_caar ((tmscm )o->lookup ());
}
inline object
tmscm_to_object (tmscm obj) {
  return tm_new<tmscm_object_rep> (obj);
}

tm_ostream& operator<< (tm_ostream& out, object obj);
bool        operator== (object obj1, object obj2);
bool        operator!= (object obj1, object obj2);
int         hash (object obj);

#endif // defined OBJECT_H
