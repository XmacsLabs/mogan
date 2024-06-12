
/******************************************************************************
 * MODULE     : object_l3.hpp
 * DESCRIPTION: Implementation of scheme objects (L3)
 * COPYRIGHT  : (C) 1999-2011 Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef OBJECT_L3_HPP
#define OBJECT_L3_HPP

#include "patch.hpp"
#include "s7_tm.hpp"

typedef array<patch> array_patch;

bool
tmscm_is_patch (tmscm obj);
patch
tmscm_to_patch (tmscm obj);
tmscm
patch_to_tmscm (patch p);

bool
tmscm_is_array_patch (tmscm p);
array<patch>
tmscm_to_array_patch (tmscm p);
tmscm
array_patch_to_tmscm (array<patch> a);

#define TMSCM_ASSERT_ARRAY_PATCH(p, arg, rout)                                 \
  TMSCM_ASSERT (tmscm_is_array_patch (p), p, arg, rout)

#endif
