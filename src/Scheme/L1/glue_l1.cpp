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
