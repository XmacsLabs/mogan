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
bool tmscm_is_content (tmscm p);
tmscm   tree_to_tmscm (tree t);
tree    tmscm_to_tree (tmscm obj);
tmscm   tree_label_to_tmscm (tree_label l);
tree_label tmscm_to_tree_label (tmscm p);
tmscm scheme_tree_to_tmscm (scheme_tree t);
scheme_tree tmscm_to_scheme_tree (tmscm p);

#define TMSCM_ASSERT_TREE(t,arg,rout) \
TMSCM_ASSERT (tmscm_is_tree (t), t, arg, rout)
#define TMSCM_ASSERT_SCHEME_TREE(p,arg,rout)
#define TMSCM_ASSERT_TREE_LABEL(p,arg,rout) \
TMSCM_ASSERT_SYMBOL(p,arg,rout)
#define TMSCM_ASSERT_CONTENT(p, arg, rout) \
  TMSCM_ASSERT (tmscm_is_content (p), p, arg, rout)
#define content_to_tmscm tree_to_tmscm


bool tmscm_is_command (tmscm u);
tmscm command_to_tmscm (command o);
command tmscm_to_command (tmscm o);

bool tmscm_is_observer (tmscm o);
tmscm observer_to_tmscm (observer o);
observer tmscm_to_observer (tmscm obj);

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

#define TMSCM_ASSERT_COMMAND(o,arg,rout) \
TMSCM_ASSERT (tmscm_is_command (o), o, arg, rout)
#define TMSCM_ASSERT_OBSERVER(o,arg,rout) \
TMSCM_ASSERT (tmscm_is_observer (o), o, arg, rout)
#define TMSCM_ASSERT_LIST_STRING(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_list_string (p), p, arg, rout)
#define TMSCM_ASSERT_LIST_TREE(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_list_tree (p), p, arg, rout)
#define TMSCM_ASSERT_PATH(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_path (p), p, arg, rout)
#define TMSCM_ASSERT_MODIFICATION(m,arg,rout) \
TMSCM_ASSERT (tmscm_is_modification (m), m, arg, rout)

typedef array<int> array_int;
typedef array<string> array_string;
typedef array<tree> array_tree;
typedef array<path> array_path;
typedef array<double> array_double;
typedef array<array<double> > array_array_double;
typedef array<array<array<double> > > array_array_array_double;

tmscm array_int_to_tmscm (array<int> a);
tmscm array_double_to_tmscm (array<double> a);
tmscm array_array_double_to_tmscm (array<array_double> a);
tmscm array_array_array_double_to_tmscm (array<array_array_double> a);

array<int> tmscm_to_array_int (tmscm p);
array<double> tmscm_to_array_double (tmscm p);
array<array_double> tmscm_to_array_array_double (tmscm p);
array<array_array_double> tmscm_to_array_array_array_double (tmscm p);

bool tmscm_is_array_int (tmscm p);
bool tmscm_is_array_string (tmscm p);
bool tmscm_is_array_double (tmscm p);
bool tmscm_is_array_array_double (tmscm p);
bool tmscm_is_array_array_array_double (tmscm p);

#define TMSCM_ASSERT_ARRAY_INT(p, arg, rout)                                   \
  TMSCM_ASSERT (tmscm_is_array_int (p), p, arg, rout)
#define TMSCM_ASSERT_ARRAY_DOUBLE(p, arg, rout)                                \
  TMSCM_ASSERT (tmscm_is_array_double (p), p, arg, rout)
#define TMSCM_ASSERT_ARRAY_ARRAY_DOUBLE(p, arg, rout)                          \
  TMSCM_ASSERT (tmscm_is_array_array_double (p), p, arg, rout)
#define TMSCM_ASSERT_ARRAY_ARRAY_ARRAY_DOUBLE(p, arg, rout)                    \
  TMSCM_ASSERT (tmscm_is_array_array_array_double (p), p, arg, rout)

#endif
