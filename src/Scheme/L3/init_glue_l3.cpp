
/******************************************************************************
 * MODULE     : init_glue_l3.cpp
 * DESCRIPTION: L3 Glue for linking TeXmacs commands to scheme
 * COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "init_glue_l3.hpp"
#include "object_l1.hpp"
#include "object_l2.hpp"
#include "object_l3.hpp"
#include "s7_tm.hpp"

#include "drd_mode.hpp"
#include "file.hpp"
#include "modification.hpp"
#include "patch.hpp"
#include "path.hpp"
#include "persistent.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tm_url.hpp"
#include "tmfs_url.hpp"
#include "tree.hpp"
#include "tree_cursor.hpp"
#include "tree_observer.hpp"
#include "url.hpp"

#include "glue_drd.cpp"
#include "glue_file.cpp"
#include "glue_url.cpp"

string
xmacs_version () {
  return XMACS_VERSION;
}

#include "glue_misc.cpp"

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

tree
var_apply (tree& t, modification m) {
  apply (t, copy (m));
  return t;
}

tree
var_clean_apply (tree& t, modification m) {
  return clean_apply (t, copy (m));
}

patch
branch_patch (array<patch> a) {
  return patch (true, a);
}

tree
var_clean_apply (tree t, patch p) {
  return clean_apply (copy (p), t);
}

tree
var_apply (tree& t, patch p) {
  apply (copy (p), t);
  return t;
}

tmscm
patchP (tmscm t) {
  bool b= tmscm_is_patch (t);
  return bool_to_tmscm (b);
}

#include "glue_modification.cpp"
#include "glue_patch.cpp"
#include "glue_path.cpp"

void
initialize_glue_l3 () {
  tmscm_install_procedure ("patch?", patchP, 1, 0, 0);
  tmscm_install_procedure ("observer?", observerP, 1, 0, 0);
  tmscm_install_procedure ("tm?", contentP, 1, 0, 0);
  tmscm_install_procedure ("modification?", modificationP, 1, 0, 0);

  initialize_glue_drd ();
  initialize_glue_url ();
  initialize_glue_file ();
  initialize_glue_misc ();
  initialize_glue_path ();
  initialize_glue_modification ();
  initialize_glue_patch ();
}
