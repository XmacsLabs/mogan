#include "glue_l2.hpp"
#include "object.hpp"

/******************************************************************************
* Urls
******************************************************************************/

bool
tmscm_is_url (tmscm u) {
  return (tmscm_is_blackbox (u)
              && (type_box (tmscm_to_blackbox(u)) == type_helper<url>::id))
         || (tmscm_is_string(u));
}

tmscm 
url_to_tmscm (url u) {
  return blackbox_to_tmscm (close_box<url> (u));
}

url
tmscm_to_url (tmscm obj) {
  if (tmscm_is_string (obj))
#ifdef OS_MINGW
    return url_system (tmscm_to_string (obj));
#else
  return tmscm_to_string (obj);
#endif
  return open_box<url> (tmscm_to_blackbox (obj));
}

url
as_url (object obj) {
  tmscm t= object_to_tmscm (obj);
  if (!tmscm_is_url (t)) return url ("");
  return tmscm_to_url (t);
}

bool is_url (object obj) { return tmscm_is_url (object_to_tmscm (obj)); }
