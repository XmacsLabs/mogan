#ifndef GLUE_L1_HPP
#define GLUE_L1_HPP

#include "tree.hpp"
#include "path.hpp"
#include "modification.hpp"
#include "s7_tm.hpp"

typedef list<string> list_string;
typedef list<tree> list_tree;


bool    tmscm_is_tree (tmscm obj);
tmscm   tree_to_tmscm (tree t);
tree    tmscm_to_tree (tmscm obj);
tmscm   tree_label_to_tmscm (tree_label l);
tree_label tmscm_to_tree_label (tmscm p);

bool    tmscm_is_list_string (tmscm obj);
tmscm   list_string_to_tmscm (list<string> l);
list<string> tmscm_to_list_string (tmscm obj);

bool    tmscm_is_list_tree (tmscm obj);
tmscm   list_tree_to_tmscm (list<tree> l);
list<tree> tmscm_to_list_tree (tmscm obj);

bool    tmscm_is_path (tmscm obj);
tmscm   path_to_tmscm (path p);
path    tmscm_to_path (tmscm obj);

bool    tmscm_is_modification (tmscm obj);
tmscm   modification_to_tmscm (modification m);
modification tmscm_to_modification (tmscm obj);


#define TMSCM_ASSERT_TREE(t,arg,rout) \
TMSCM_ASSERT (tmscm_is_tree (t), t, arg, rout)
#define TMSCM_ASSERT_PATH(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_path (p), p, arg, rout)

#endif
