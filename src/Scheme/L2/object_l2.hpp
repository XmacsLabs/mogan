#ifndef OBJECT_L2_HPP
#define OBJECT_L2_HPP

#include "s7_tm.hpp"
#include "url.hpp"

typedef array<url> array_url;

bool    tmscm_is_url (tmscm obj);
#define TMSCM_ASSERT_URL(u,arg,rout) \
TMSCM_ASSERT (tmscm_is_url (u) || tmscm_is_string (u), u, arg, rout)

tmscm   url_to_tmscm (url u);
url     tmscm_to_url (tmscm obj);
tmscm array_url_to_tmscm (array<url> a);
array<url> tmscm_to_array_url (tmscm p);
bool tmscm_is_array_url (tmscm p);

#define TMSCM_ASSERT_ARRAY_URL(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_array_url (p), p, arg, rout)

#endif
