
/******************************************************************************
 * MODULE     : object_l5.hpp
 * DESCRIPTION: Implementation of scheme objects (L5)
 * COPYRIGHT  : (C) 1999-2011 Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef OBJECT_L5_HPP
#define OBJECT_L5_HPP

#include "promise.hpp"
#include "s7_tm.hpp"
#include "widget.hpp"

typedef promise<widget> promise_widget;

bool   tmscm_is_widget (tmscm obj);
tmscm  widget_to_tmscm (widget o);
widget tmscm_to_widget (tmscm o);
widget as_widget (object obj);
bool   is_widget (object obj);

bool            tmscm_is_promise_widget (tmscm u);
tmscm           promise_widget_to_tmscm (promise_widget o);
promise_widget  tmscm_to_promise_widget (tmscm o);
promise<widget> as_promise_widget (object obj);

#define TMSCM_ASSERT_WIDGET(o, arg, rout)                                      \
  TMSCM_ASSERT (tmscm_is_widget (o), o, arg, rout)
#define TMSCM_ASSERT_PROMISE_WIDGET(o, arg, rout)                              \
  TMSCM_ASSERT (tmscm_is_promise_widget (o), o, arg, rout)

#endif
