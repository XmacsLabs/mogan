
/******************************************************************************
* MODULE     : tree_helper.hpp
* DESCRIPTION: helpers of trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/


#ifndef TREE_HELPER_H
#define TREE_HELPER_H

#include "tree.hpp"
#include "tree_label.hpp"

/******************************************************************************
* Tuples
******************************************************************************/

inline tree tuple () {
  return tree (TUPLE); }
inline tree tuple (tree t1) {
  return tree (TUPLE, t1); }
inline tree tuple (tree t1, tree t2) {
  return tree (TUPLE, t1, t2); }
inline tree tuple (tree t1, tree t2, tree t3) {
  return tree (TUPLE, t1, t2, t3); }
inline tree tuple (tree t1, tree t2, tree t3, tree t4) {
  return tree (TUPLE, t1, t2, t3, t4); }
inline tree tuple (tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (TUPLE, t1, t2, t3, t4, t5); }

inline bool is_tuple (tree t) {
  return (L(t) == TUPLE); }
inline bool is_tuple (tree t, string s) {
  return (L(t) == TUPLE) && (N(t) >= 1) && (t[0] == s); }
inline bool is_tuple (tree t, const char* s) {
  return (L(t) == TUPLE) && (N(t) >= 1) && (t[0] == s); }
inline bool is_tuple (tree t, string s, int n) {
  return (L(t) == TUPLE) && (N(t) == (n+1)) && (t[0] == s); }
inline bool is_tuple (tree t, const char* s, int n) {
  return (L(t) == TUPLE) && (N(t) == (n+1)) && (t[0] == s); }


/******************************************************************************
* Other frequent markup
******************************************************************************/

inline tree concat () {
  return tree (CONCAT); }
inline tree concat (tree t1) {
  return tree (CONCAT, t1); }
inline tree concat (tree t1, tree t2) {
  return tree (CONCAT, t1, t2); }
inline tree concat (tree t1, tree t2, tree t3) {
  return tree (CONCAT, t1, t2, t3); }
inline tree concat (tree t1, tree t2, tree t3, tree t4) {
  return tree (CONCAT, t1, t2, t3, t4); }
inline tree concat (tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (CONCAT, t1, t2, t3, t4, t5); }

inline tree document () {
  return tree (DOCUMENT); }
inline tree document (tree t1) {
  return tree (DOCUMENT, t1); }
inline tree document (tree t1, tree t2) {
  return tree (DOCUMENT, t1, t2); }
inline tree document (tree t1, tree t2, tree t3) {
  return tree (DOCUMENT, t1, t2, t3); }
inline tree document (tree t1, tree t2, tree t3, tree t4) {
  return tree (DOCUMENT, t1, t2, t3, t4); }
inline tree document (tree t1, tree t2, tree t3, tree t4, tree t5) {
  return tree (DOCUMENT, t1, t2, t3, t4, t5); }

bool is_document (tree t);
bool is_concat (tree t);
bool is_format (tree t);

#endif
