#include "s7_blackbox.hpp"
#include "convert.hpp"
#include "object_l1.hpp"
#include "object_l2.hpp"
#include "patch.hpp"
#include "string.hpp"
#include "tree.hpp"
#include "widget.hpp"

/******************************************************************************
 * Blackbox
 ******************************************************************************/

static tmscm
blackbox_to_string (s7_scheme* sc, tmscm args) {
  // FIXME: take into account sc!

  tmscm blackbox_smob= s7_car (args);

  string s    = "<blackbox>";
  int    type_= type_box (tmscm_to_blackbox (blackbox_smob));
  if (type_ == type_helper<tree>::id) {
    tree t= tmscm_to_tree (blackbox_smob);
    s     = "<tree " * tree_to_texmacs (t) * ">";
  }
  else if (type_ == type_helper<observer>::id) {
    s= "<observer>";
  }
  else if (type_ == type_helper<widget>::id) {
    s= "<widget>";
  }
  else if (type_ == type_helper<promise<widget>>::id) {
    s= "<promise-widget>";
  }
  else if (type_ == type_helper<command>::id) {
    s= "<command>";
  }
  else if (type_ == type_helper<url>::id) {
    url u= tmscm_to_url (blackbox_smob);
    s    = "<url " * as_string (u) * ">";
  }
  else if (type_ == type_helper<modification>::id) {
    s= "<modification>";
  }
  else if (type_ == type_helper<patch>::id) {
    s= "<patch>";
  }

  return string_to_tmscm (s);
}

static s7_pointer
free_blackbox (s7_scheme* sc, s7_pointer obj) {
  blackbox* ptr= (blackbox*) s7_c_object_value (obj);
  tm_delete (ptr);

  // Don't remove this, segmentation error could happen :)
  return (NULL);
}

static s7_pointer
mark_blackbox (s7_scheme* sc, s7_pointer obj) {
  return (NULL);
}

void
initialize_smobs (int blackbox_tag) {
  s7_c_type_set_gc_free (tm_s7, blackbox_tag, free_blackbox);
  s7_c_type_set_gc_mark (tm_s7, blackbox_tag, mark_blackbox);
  s7_c_type_set_is_equal (tm_s7, blackbox_tag, blackbox_is_equal);
  s7_c_type_set_to_string (tm_s7, blackbox_tag, blackbox_to_string);
}
