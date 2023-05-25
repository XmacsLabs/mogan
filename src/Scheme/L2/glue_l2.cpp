#include "glue_l2.hpp"
#include "object_l2.hpp"
#include "s7_tm.hpp"

tmscm
urlP (tmscm t) {
  bool b= tmscm_is_url (t);
  return bool_to_tmscm (b);
}

void
initialize_glue_l2 () {
  tmscm_install_procedure ("url?", urlP, 1, 0, 0);
}