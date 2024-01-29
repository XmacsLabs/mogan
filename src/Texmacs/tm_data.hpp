
/******************************************************************************
 * MODULE     : tm_data.hpp
 * DESCRIPTION: Buffer management for TeXmacs server
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TM_DATA_H
#define TM_DATA_H
#include "server.hpp"
#include "tm_window.hpp"

extern array<tm_buffer> bufs;

/* Commodity macros */
inline void
set_message (tree left, tree right, bool temp= false) {
  get_server ()->set_message (left, right, temp);
}

#endif // defined TM_DATA_H
