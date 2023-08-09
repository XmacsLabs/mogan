
/******************************************************************************
* MODULE     : drd_info.hpp
* DESCRIPTION: data relation descriptions
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef DRD_INFO_H
#define DRD_INFO_H
#include "tree.hpp"
#include "rel_hashmap.hpp"
#include "tag_info.hpp"

class drd_info;
class drd_info_rep: concrete_struct {
public:
  string name;
  rel_hashmap<tree_label,tag_info> info;
  hashmap<string,pre_tree> env;

public:
  drd_info_rep (string name);
  drd_info_rep (string name, drd_info base);
  pre_tree get_locals ();
  bool set_locals (pre_tree t);
  bool contains (string l);

  /* Properties of the tag itself */
  void set_type (tree_label tag, int tp);
  int  get_type (tree_label tag);
  void freeze_type (tree_label tag);
  int  get_type (pre_tree t);

  void set_arity (tree_label tag, int arity, int extra, int am, int cm);
  int  get_arity_mode (tree_label tag);
  int  get_arity_base (tree_label tag);
  int  get_arity_extra (tree_label tag);
  int  get_child_mode (tree_label tag);
  int  get_nr_indices (tree_label tag);
  void freeze_arity (tree_label tag);
  int  get_old_arity (tree_label l);
  int  get_minimal_arity (tree_label l);
  int  get_maximal_arity (tree_label l);
  bool correct_arity (tree_label l, int i);
  bool insert_point (tree_label l, int i, int n);
  bool is_dynamic (pre_tree t, bool hack= true);

  void set_border (tree_label tag, int mode);
  int  get_border (tree_label tag);
  void freeze_border (tree_label tag);
  bool is_child_enforcing (pre_tree t);
  bool is_parent_enforcing (pre_tree t);
  bool var_without_border (tree_label tag);

  void set_block (tree_label tag, int is_block);
  int  get_block (tree_label tag);
  void freeze_block (tree_label tag);

  void set_with_like (tree_label tag, bool is_with_like);
  bool get_with_like (tree_label tag);
  void freeze_with_like (tree_label tag);
  bool is_with_like (pre_tree t);

  void set_var_type (tree_label tag, int vt);
  int  get_var_type (tree_label tag);
  void freeze_var_type (tree_label tag);

  void set_attribute (tree_label tag, string which, pre_tree val);
  pre_tree get_attribute (tree_label tag, string which);
  void set_name (tree_label tag, string val);
  void set_long_name (tree_label tag, string val);
  void set_syntax (tree_label tag, pre_tree val);
  string get_name (tree_label tag);
  string get_long_name (tree_label tag);
  pre_tree   get_syntax (tree_label tag);
  pre_tree   get_syntax (pre_tree t, path p= path (-1));

  /* Properties of the children of the tag */
  void set_type (tree_label tag, int nr, int tp);
  int  get_type (tree_label tag, int nr);
  void freeze_type (tree_label tag, int nr);
  int  get_type_child (pre_tree t, int child);

  void set_accessible (tree_label tag, int nr, int access_mode);
  int  get_accessible (tree_label tag, int nr);
  void freeze_accessible (tree_label tag, int nr);
  bool all_accessible (tree_label tag);
  bool none_accessible (tree_label tag);
  bool is_accessible_child (pre_tree t, int child);
  bool is_accessible_path (pre_tree t, path p);

  void set_writability (tree_label tag, int nr, int writability);
  int  get_writability (tree_label tag, int nr);
  void freeze_writability (tree_label tag, int nr);
  int  get_writability_child (pre_tree t, int child);

  void set_block (tree_label tag, int nr, int require_block);
  int  get_block (tree_label tag, int nr);
  void freeze_block (tree_label tag, int nr);

  void set_env (tree_label tag, int nr, pre_tree env);
  pre_tree get_env (tree_label tag, int nr);
  void freeze_env (tree_label tag, int nr);
  pre_tree get_env_child (pre_tree t, int child, pre_tree env);
  pre_tree get_env_child (pre_tree t, int child, string var, pre_tree val);
  pre_tree get_env_descendant (pre_tree t, path p, pre_tree env);
  pre_tree get_env_descendant (pre_tree t, path p, string var, pre_tree val);

  void set_child_name (tree_label tag, int nr, string val);
  void set_child_long_name (tree_label tag, int nr, string val);
  string get_child_name (tree_label tag, int nr);
  string get_child_long_name (tree_label tag, int nr);
  string get_child_name (pre_tree t, int child);
  string get_child_long_name (pre_tree t, int child);

  /* Heuristic initialization */
  void set_environment (hashmap<string,pre_tree> env);
  pre_tree arg_access (pre_tree t, pre_tree arg, pre_tree env, int& type, bool& found);
  bool heuristic_with_like (pre_tree t, pre_tree arg);
  bool heuristic_init_macro (string var, pre_tree macro);
  bool heuristic_init_xmacro (string var, pre_tree xmacro);
  bool heuristic_init_parameter (string var, string val);
  bool heuristic_init_parameter (string var, pre_tree val);
  void heuristic_init (hashmap<string,pre_tree> env);

  friend class drd_info;
  friend tm_ostream& operator << (tm_ostream& out, drd_info drd);
};

class drd_info {
  CONCRETE(drd_info);
  drd_info (string name);
  drd_info (string name, drd_info base);
  operator pre_tree ();
};
CONCRETE_CODE(drd_info);

pre_tree drd_env_write (pre_tree env, string var, pre_tree val);
pre_tree drd_env_merge (pre_tree env, pre_tree t);
pre_tree drd_env_read (pre_tree env, string var, pre_tree val= pre_tree (UNINIT));

#endif // defined DRD_INFO_H
