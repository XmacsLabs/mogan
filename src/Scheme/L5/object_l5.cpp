
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
         (type_box (tmscm_to_blackbox(u)) == type_helper<widget>::id));
}


tmscm 
widget_to_tmscm (widget o) {
  return blackbox_to_tmscm (close_box<widget> (o));
}

widget
tmscm_to_widget (tmscm o) {
  return open_box<widget> (tmscm_to_blackbox (o));
}
