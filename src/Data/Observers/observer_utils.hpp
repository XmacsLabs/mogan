
/******************************************************************************
 * MODULE     : observer_utils.hpp
 * DESCRIPTION: Observer Utils
 * COPYRIGHT  : (C) 2006  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef OBSERVER_UTIL_HPP
#define OBSERVER_UTIL_HPP

#include "tree.hpp"

void assign (tree &ref, tree t);
void insert (tree &ref, int pos, tree t);
void remove (tree &ref, int pos, int nr);
void split (tree &ref, int pos, int at);
void join (tree &ref, int pos);
void assign_node (tree &ref, tree_label op);
void insert_node (tree &ref, int pos, tree t);
void remove_node (tree &ref, int pos);
void set_cursor (tree &ref, int pos, tree data);
void touch (tree &ref);

void assign (path p, tree t);
void insert (path p, tree ins);
void remove (path p, int nr);
void split (path p);
void join (path p);
void assign_node (path p, tree_label op);
void insert_node (path p, tree ins);
void remove_node (path p);
void set_cursor (path p, tree data);
void touch (path p);

#endif
