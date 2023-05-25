#include "glue_l1.hpp"
#include "object_l1.hpp"
#include "s7_tm.hpp"

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

/******************************************************************************
 * Content
 ******************************************************************************/

#define content tree
#define TMSCM_ASSERT_CONTENT(p, arg, rout)                                     \
  TMSCM_ASSERT (tmscm_is_content (p), p, arg, rout)
#define content_to_tmscm tree_to_tmscm

tree
tmscm_to_content (tmscm p) {
  if (tmscm_is_string (p)) return tmscm_to_string (p);
  if (tmscm_is_tree (p)) return tmscm_to_tree (p);
  if (tmscm_is_pair (p)) {
    if (!tmscm_is_symbol (tmscm_car (p))) return "?";
    tree t (make_tree_label (tmscm_to_symbol (tmscm_car (p))));
    p= tmscm_cdr (p);
    while (!tmscm_is_null (p)) {
      t << tmscm_to_content (tmscm_car (p));
      p= tmscm_cdr (p);
    }
    return t;
  }
  return "?";
}

bool
tmscm_is_content (tmscm p) {
  if (tmscm_is_string (p) || tmscm_is_tree (p)) return true;
  else if (!tmscm_is_pair (p) || !tmscm_is_symbol (tmscm_car (p))) return false;
  else {
    for (p= tmscm_cdr (p); !tmscm_is_null (p); p= tmscm_cdr (p))
      if (!tmscm_is_pair (p) || !tmscm_is_content (tmscm_car (p))) return false;
    return true;
  }
}

tmscm
contentP (tmscm t) {
  bool b= tmscm_is_content (t);
  return bool_to_tmscm (b);
}

void
initialize_glue_l1 () {
  tmscm_install_procedure ("blackbox?", blackboxP, 1, 0, 0);
  tmscm_install_procedure ("tm?", contentP, 1, 0, 0);
  tmscm_install_procedure ("tree?", treeP, 1, 0, 0);
  tmscm_install_procedure ("observer?", observerP, 1, 0, 0);
  tmscm_install_procedure ("modification?", modificationP, 1, 0, 0);
}
