
/******************************************************************************
 * MODULE     : object_l5.cpp
 * DESCRIPTION: Implementation of scheme objects (L5)
 * COPYRIGHT  : (C) 1999-2011 Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "object_l5.hpp"
#include "object.hpp"

/******************************************************************************
 * Widgets
 ******************************************************************************/

bool
tmscm_is_widget (tmscm u) {
  return (tmscm_is_blackbox (u) &&
          (type_box (tmscm_to_blackbox (u)) == type_helper<widget>::id));
}

tmscm
widget_to_tmscm (widget o) {
  return blackbox_to_tmscm (close_box<widget> (o));
}

widget
tmscm_to_widget (tmscm o) {
  return open_box<widget> (tmscm_to_blackbox (o));
}

widget
as_widget (object obj) {
  tmscm w= object_to_tmscm (obj);
  if (!tmscm_is_widget (w)) return widget ();
  return tmscm_to_widget (w);
}

bool
is_widget (object obj) {
  return tmscm_is_widget (object_to_tmscm (obj));
}

/******************************************************************************
 *  Widget Factory
 ******************************************************************************/

bool
tmscm_is_promise_widget (tmscm u) {
  return (tmscm_is_blackbox (u) && (type_box (tmscm_to_blackbox (u)) ==
                                    type_helper<promise_widget>::id));
}

tmscm
promise_widget_to_tmscm (promise_widget o) {
  return blackbox_to_tmscm (close_box<promise_widget> (o));
}

promise_widget
tmscm_to_promise_widget (tmscm o) {
  return open_box<promise_widget> (tmscm_to_blackbox (o));
}

class object_promise_widget_rep : public promise_rep<widget> {
  object obj;

public:
  object_promise_widget_rep (object obj2) : obj (obj2) {}
  tm_ostream& print (tm_ostream& out) { return out << obj; }
  widget      eval () {
    tmscm result= call_scheme (object_to_tmscm (obj));
    if (tmscm_is_widget (result)) return tmscm_to_widget (result);
    else {
      TM_FAILED ("widget expected");
      return glue_widget ();
    }
  }
};

promise<widget>
as_promise_widget (object obj) {
  return tm_new<object_promise_widget_rep> (obj);
}
