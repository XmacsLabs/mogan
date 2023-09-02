
/******************************************************************************
 * MODULE     : glue_l2.cpp
 * DESCRIPTION: L2 Glue for linking TeXmacs commands to scheme
 * COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "init_glue_l2.hpp"
#include "object_l1.hpp"
#include "object_l2.hpp"
#include "s7_tm.hpp"

#include "file.hpp"
#include "persistent.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tm_url.hpp"
#include "tree.hpp"

#include "glue_file.cpp"
#include "glue_misc.cpp"
#include "glue_url.cpp"

tmscm
urlP (tmscm t) {
  bool b= tmscm_is_url (t);
  return bool_to_tmscm (b);
}

void
initialize_glue_l2 () {
  tmscm_install_procedure ("url?", urlP, 1, 0, 0);
  initialize_glue_url ();
  initialize_glue_file ();
  initialize_glue_misc ();
}