/******************************************************************************
 * MODULE     : xkerning.hpp
 * DESCRIPTION: Extended kerning for text boxes
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *                  2025  Mogan STEM authors
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef XKERNING_H
#define XKERNING_H

#include "basic.hpp"

struct xkerning_rep : concrete_struct {
  SI padding;
  SI left;
  SI right;
  xkerning_rep (SI p, SI l, SI r) : padding (p), left (l), right (r) {}
  ~xkerning_rep () {}
};

class xkerning {
  CONCRETE_NULL (xkerning);
  xkerning (SI p, SI l, SI r) : rep (tm_new<xkerning_rep> (p, l, r)) {};
};

CONCRETE_NULL_CODE (xkerning);

static inline bool
is_nil_or_zero (xkerning xk) {
  return is_nil (xk) || (xk->left == 0 && xk->right == 0 && xk->padding == 0);
}

#endif // XKERNING_H
