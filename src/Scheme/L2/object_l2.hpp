#ifndef GLUE_L2_HPP
#define GLUE_L2_HPP

#include "s7_tm.hpp"
#include "url.hpp"

bool    tmscm_is_url (tmscm obj);
tmscm   url_to_tmscm (url u);
url     tmscm_to_url (tmscm obj);

#endif
