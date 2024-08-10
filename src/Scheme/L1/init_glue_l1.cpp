
/******************************************************************************
 * MODULE     : init_glue_l1.cpp
 * DESCRIPTION: L1 Glue for linking TeXmacs commands to scheme
 * COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "init_glue_l1.hpp"
#include "object_l1.hpp"
#include "s7_tm.hpp"

#include "drd_mode.hpp"
#include "string.hpp"

tmscm
blackboxP (tmscm t) {
  bool b= tmscm_is_blackbox (t);
  return bool_to_tmscm (b);
}

tmscm
treeP (tmscm t) {
  bool b= tmscm_is_blackbox (t) &&
          (type_box (tmscm_to_blackbox (t)) == type_helper<tree>::id);
  return bool_to_tmscm (b);
}

tmscm
observerP (tmscm t) {
  bool b= tmscm_is_blackbox (t) &&
          (type_box (tmscm_to_blackbox (t)) == type_helper<observer>::id);
  return bool_to_tmscm (b);
}

tmscm
modificationP (tmscm t) {
  bool b= tmscm_is_modification (t);
  return bool_to_tmscm (b);
}

tmscm
contentP (tmscm t) {
  bool b= tmscm_is_content (t);
  return bool_to_tmscm (b);
}

#include "glue_drd.cpp"

void
initialize_glue_l1 () {
  tmscm_install_procedure ("blackbox?", blackboxP, 1, 0, 0);
  tmscm_install_procedure ("tm?", contentP, 1, 0, 0);
  tmscm_install_procedure ("tree?", treeP, 1, 0, 0);
  tmscm_install_procedure ("observer?", observerP, 1, 0, 0);
  tmscm_install_procedure ("modification?", modificationP, 1, 0, 0);

  initialize_glue_drd ();
}
