
/******************************************************************************
 * MODULE     : preferences.cpp
 * DESCRIPTION: User preferences for TeXmacs
 * COPYRIGHT  : (C) 2012  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "preferences.hpp"
#include "analyze.hpp"
#include "block.hpp"
#include "file.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tree_helper.hpp"

tree texmacs_settings= tuple ();

/******************************************************************************
 * Old style settings files
 ******************************************************************************/

static string
line_read (string s, int& i) {
  int start= i, n= N (s);
  for (start= i; i < n; i++)
    if (s[i] == '\n') break;
  string r= s (start, i);
  if (i < n) i++;
  return r;
}

void
get_old_settings (string s) {
  int i= 0, j;
  while (i < N (s)) {
    string l= line_read (s, i);
    for (j= 0; j < N (l); j++)
      if (l[j] == '=') {
        string left= l (0, j);
        while ((j < N (l)) && ((l[j] == '=') || (l[j] == ' ')))
          j++;
        string right= l (j, N (l));
        set_setting (left, right);
      }
  }
}

/******************************************************************************
 * Subroutines for the TeXmacs settings
 ******************************************************************************/

string
get_setting (string var, string def) {
  int i, n= N (texmacs_settings);
  for (i= 0; i < n; i++)
    if (is_tuple (texmacs_settings[i], var, 1)) {
      return scm_unquote (as_string (texmacs_settings[i][1]));
    }
  return def;
}

void
set_setting (string var, string val) {
  int i, n= N (texmacs_settings);
  for (i= 0; i < n; i++)
    if (is_tuple (texmacs_settings[i], var, 1)) {
      texmacs_settings[i][1]= scm_quote (val);
      return;
    }
  texmacs_settings << tuple (var, scm_quote (val));
}

/******************************************************************************
 * Changing the user preferences
 ******************************************************************************/

bool                    user_prefs_modified= false;
hashmap<string, string> user_prefs ("");
void                    notify_preference (string var);

bool
has_user_preference (string var) {
  return user_prefs->contains (var);
}

void
set_user_preference (string var, string val) {
  if (val == "default") user_prefs->reset (var);
  else user_prefs (var)= val;
  user_prefs_modified= true;
  notify_preference (var);
}

void
reset_user_preference (string var) {
  user_prefs->reset (var);
  user_prefs_modified= true;
  notify_preference (var);
}

string
get_user_preference (string var, string val) {
  if (user_prefs->contains (var)) return user_prefs[var];
  else return val;
}

/******************************************************************************
 * Loading and saving user preferences
 ******************************************************************************/

void
load_user_preferences () {
  url  prefs_file= get_tm_preference_path ();
  tree p (TUPLE);
  if (exists (prefs_file)) {
    p= block_to_scheme_tree (string_load (prefs_file));
  }
  while (is_func (p, TUPLE, 1))
    p= p[0];
  for (int i= 0; i < N (p); i++)
    if (is_func (p[i], TUPLE, 2) && is_atomic (p[i][0]) &&
        is_atomic (p[i][1]) && is_quoted (p[i][0]->label) &&
        is_quoted (p[i][1]->label)) {
      string var      = scm_unquote (p[i][0]->label);
      string val      = scm_unquote (p[i][1]->label);
      user_prefs (var)= val;
    }
  user_prefs_modified= false;
}

void
save_user_preferences () {
  if (!user_prefs_modified) return;
  url              prefs_file= get_tm_preference_path ();
  iterator<string> it        = iterate (user_prefs);
  array<string>    a;
  while (it->busy ())
    a << it->next ();
  merge_sort (a);
  string s;
  for (int i= 0; i < N (a); i++)
    s << "(" << scm_quote (a[i]) << " " << scm_quote (user_prefs[a[i]])
      << ")\n";
  if (save_string (prefs_file, s))
    std_warning << "The user preferences could not be saved\n";
  user_prefs_modified= false;
}
