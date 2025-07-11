
/******************************************************************************
 * MODULE     : scheme.hpp
 * DESCRIPTION: Abstract interface for the manipulation of scheme objects
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef SCHEME_HH
#define SCHEME_HH

#include "object.hpp"

void start_scheme (int argc, char** argv, void (*call_back) (int, char**));
int  initialize_scheme ();

object null_object ();
object list_object (object obj1);
object list_object (object obj1, object obj2);
object list_object (object obj1, object obj2, object obj3);
object as_list_object (array<object> objs);
object symbol_object (string s);
object cons (object obj1, object obj2);
object car (object obj);
object cdr (object obj);
object caar (object obj);
object cdar (object obj);
object cadr (object obj);
object cddr (object obj);
object caddr (object obj);
object cadddr (object obj);

bool is_null (object obj);
bool is_list (object obj);
bool is_bool (object obj);
bool is_int (object obj);
bool is_double (object obj);
bool is_string (object obj);
bool is_symbol (object obj);
bool is_tree (object obj);
bool is_path (object obj);
bool is_url (object obj);
bool is_array_double (object obj);
bool is_modification (object obj);
bool is_patch (object obj);

bool          as_bool (object obj);
int           as_int (object obj);
double        as_double (object obj);
string        as_string (object obj);
string        as_symbol (object obj);
tree          as_tree (object obj);
scheme_tree   as_scheme_tree (object obj);
list<string>  as_list_string (object obj);
list<tree>    as_list_tree (object obj);
path          as_path (object obj);
array<object> as_array_object (object obj);
url           as_url (object obj);
array<double> as_array_double (object obj);
modification  as_modification (object obj);
patch         as_patch (object obj);
command       as_command (object obj);
#ifdef WIDGET_H // FIXME: dirty hack
#endif

object tree_to_stree (tree t);
tree   stree_to_tree (object obj);
tree   content_to_tree (object obj);
object string_to_object (string s);
string object_to_string (object obj);
object scheme_cmd (const char* s);
object scheme_cmd (string s);
object scheme_cmd (object cmd);

object eval (const char* expr);
object eval (string expr);
object eval (object expr);
object eval_secure (string expr);
object eval_file (string name);
bool   exec_file (url u);
void   exec_delayed (object cmd);
void   exec_delayed_pause (object cmd);
void   exec_pending_commands ();
void   clear_pending_commands ();
void   protected_call (object cmd);

object call (const char* fun);
object call (const char* fun, object a1);
object call (const char* fun, object a1, object a2);
object call (const char* fun, object a1, object a2, object a3);
object call (const char* fun, object a1, object a2, object a3, object a4);
object call (const char* fun, array<object> a);
object call (string fun);
object call (string fun, object a1);
object call (string fun, object a1, object a2);
object call (string fun, object a1, object a2, object a3);
object call (string fun, object a1, object a2, object a3, object a4);
object call (string fun, array<object> a);
object call (object fun);
object call (object fun, object a1);
object call (object fun, object a1, object a2);
object call (object fun, object a1, object a2, object a3);
object call (object fun, object a1, object a2, object a3, object a4);
object call (object fun, array<object> a);

#endif // defined SCHEME_HH
