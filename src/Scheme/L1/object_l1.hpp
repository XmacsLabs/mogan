#ifndef OBJECT_L1_HPP
#define OBJECT_L1_HPP

#include "tree.hpp"
#include "path.hpp"
#include "command.hpp"
#include "modification.hpp"
#include "s7_tm.hpp"


/******************************************************************************
* Basic assertions
******************************************************************************/

#define TMSCM_ASSERT_STRING(s,arg,rout) \
TMSCM_ASSERT (tmscm_is_string (s), s, arg, rout)
#define TMSCM_ASSERT_BOOL(flag,arg,rout) \
TMSCM_ASSERT (tmscm_is_bool (flag), flag, arg, rout)
#define TMSCM_ASSERT_INT(i,arg,rout) \
TMSCM_ASSERT (tmscm_is_int (i), i, arg, rout);
#define TMSCM_ASSERT_DOUBLE(i,arg,rout) \
  TMSCM_ASSERT (tmscm_is_double (i), i, arg, rout);
//TMSCM_ASSERT (SCM_REALP (i), i, arg, rout);
#define TMSCM_ASSERT_PATCH(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_patch (p), p, arg, rout)
#define TMSCM_ASSERT_BLACKBOX(t,arg,rout) \
TMSCM_ASSERT (tmscm_is_blackbox (t), t, arg, rout)
#define TMSCM_ASSERT_SYMBOL(s,arg,rout) \
  TMSCM_ASSERT (tmscm_is_symbol (s), s, arg, rout)
//TMSCM_ASSERT (SCM_NFALSEP (tmscm_symbol_p (s)), s, arg, rout)

#define TMSCM_ASSERT_OBJECT(a,b,c)
// no check


typedef list<string> list_string;
typedef list<tree> list_tree;


bool    tmscm_is_tree (tmscm obj);
tmscm   tree_to_tmscm (tree t);
tree    tmscm_to_tree (tmscm obj);

#define TMSCM_ASSERT_TREE(t,arg,rout) \
TMSCM_ASSERT (tmscm_is_tree (t), t, arg, rout)


tmscm   tree_label_to_tmscm (tree_label l);
tree_label tmscm_to_tree_label (tmscm p);

#define TMSCM_ASSERT_TREE_LABEL(p,arg,rout) \
TMSCM_ASSERT_SYMBOL(p,arg,rout)


bool tmscm_is_command (tmscm u);
tmscm command_to_tmscm (command o);
command tmscm_to_command (tmscm o);

#define TMSCM_ASSERT_COMMAND(o,arg,rout) \
TMSCM_ASSERT (tmscm_is_command (o), o, arg, rout)


bool tmscm_is_observer (tmscm o);
tmscm observer_to_tmscm (observer o);
observer tmscm_to_observer (tmscm obj);

#define TMSCM_ASSERT_OBSERVER(o,arg,rout) \
TMSCM_ASSERT (tmscm_is_observer (o), o, arg, rout)


bool    tmscm_is_list_string (tmscm obj);
tmscm   list_string_to_tmscm (list<string> l);
list<string> tmscm_to_list_string (tmscm obj);

#define TMSCM_ASSERT_LIST_STRING(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_list_string (p), p, arg, rout)


bool    tmscm_is_list_tree (tmscm obj);
tmscm   list_tree_to_tmscm (list<tree> l);
list<tree> tmscm_to_list_tree (tmscm obj);

#define TMSCM_ASSERT_LIST_TREE(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_list_tree (p), p, arg, rout)


bool    tmscm_is_path (tmscm obj);
tmscm   path_to_tmscm (path p);
path    tmscm_to_path (tmscm obj);

#define TMSCM_ASSERT_PATH(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_path (p), p, arg, rout)


bool    tmscm_is_modification (tmscm obj);
tmscm   modification_to_tmscm (modification m);
modification tmscm_to_modification (tmscm obj);

#define TMSCM_ASSERT_MODIFICATION(m,arg,rout) \
TMSCM_ASSERT (tmscm_is_modification (m), m, arg, rout)

#endif
