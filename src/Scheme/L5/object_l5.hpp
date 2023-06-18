
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

#include "s7_tm.hpp"
#include "widget.hpp"

bool tmscm_is_widget (tmscm obj);
tmscm widget_to_tmscm (widget o);
widget tmscm_to_widget (tmscm o);

#define TMSCM_ASSERT_WIDGET(o,arg,rout) \
TMSCM_ASSERT (tmscm_is_widget (o), o, arg, rout)

#endif
