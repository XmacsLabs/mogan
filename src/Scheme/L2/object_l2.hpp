
/******************************************************************************
 * MODULE     : object_l2.hpp
 * DESCRIPTION: Implementation of scheme objects (L2)
 * COPYRIGHT  : (C) 1999-2011 Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef OBJECT_L2_HPP
#define OBJECT_L2_HPP

#include "s7_tm.hpp"
#include "url.hpp"

typedef array<url> array_url;

bool tmscm_is_url (tmscm obj);
#define TMSCM_ASSERT_URL(u, arg, rout)                                         \
  TMSCM_ASSERT (tmscm_is_url (u) || tmscm_is_string (u), u, arg, rout)

tmscm      url_to_tmscm (url u);
url        tmscm_to_url (tmscm obj);
tmscm      array_url_to_tmscm (array<url> a);
array<url> tmscm_to_array_url (tmscm p);
bool       tmscm_is_array_url (tmscm p);

#define TMSCM_ASSERT_ARRAY_URL(p, arg, rout)                                   \
  TMSCM_ASSERT (tmscm_is_array_url (p), p, arg, rout)

#endif
