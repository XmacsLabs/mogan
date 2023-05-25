#include "glue_l1.hpp"
#include "object_l1.hpp"
#include "s7_tm.hpp"

tmscm
treeP (tmscm t) {
  bool b= tmscm_is_blackbox (t) &&
          (type_box (tmscm_to_blackbox (t)) == type_helper<tree>::id);
  return bool_to_tmscm (b);
}

void
initialize_glue_l1 () {
  tmscm_install_procedure ("tree?", treeP, 1, 0, 0);
}
