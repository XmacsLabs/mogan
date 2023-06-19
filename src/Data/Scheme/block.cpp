
/******************************************************************************
* MODULE     : block.cpp
* DESCRIPTION: A block of Scheme data
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
                   2023  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "block.hpp"
#include "analyze.hpp"
#include "drd_std.hpp"
#include "tm_debug.hpp"

/******************************************************************************
 * Handling escape characters
 ******************************************************************************/

string
unslash (string s) {
  int    i, n= N (s);
  string r;
  for (i= 0; i < n; i++)
    if ((s[i] == '\\') && ((i + 1) < n)) switch (s[++i]) {
      case '0':
        r << ((char) 0);
        break;
      case 'n':
        r << '\n';
        break;
      case 't':
        r << '\t';
        break;
      default:
        r << s[i];
      }
    else r << s[i];
  return r;
}

/******************************************************************************
 * Converting strings to scheme trees
 ******************************************************************************/

static bool
is_spc (char c) {
  return (c == ' ') || (c == '\t') || (c == '\n');
}

static scheme_tree
string_to_scheme_tree (string s, int &i) {
  for (; i < N (s); i++)
    switch (s[i]) {

    case ' ':
    case '\t':
    case '\n':
      break;
    case '(': {
      scheme_tree p (TUPLE);
      i++;
      while (true) {
        while ((i < N (s)) && is_spc (s[i]))
          i++;
        if ((i == N (s)) || (s[i] == ')')) break;
        p << string_to_scheme_tree (s, i);
      }
      if (i < N (s)) i++;
      return p;
    }

    case '\'':
      i++;
      return scheme_tree (TUPLE, "\'", string_to_scheme_tree (s, i));

    case '\"': { // "
      int start= i++;
      while ((i < N (s)) && (s[i] != '\"')) { // "
        if ((i < N (s) - 1) && (s[i] == '\\')) i++;
        i++;
      }
      if (i < N (s)) i++;
      return scheme_tree (unslash (s (start, i)));
    }

    case ';':
      while ((i < N (s)) && (s[i] != '\n'))
        i++;
      break;

    default: {
      int start= i;
      while ((i < N (s)) && (!is_spc (s[i])) && (s[i] != '(') &&
             (s[i] != ')')) {
        if ((i < N (s) - 1) && (s[i] == '\\')) i++;
        i++;
      }
      return scheme_tree (unslash (s (start, i)));
    }
    }

  return "";
}

scheme_tree
string_to_scheme_tree (string s) {
  s    = replace (s, "\015", "");
  int i= 0;
  return string_to_scheme_tree (s, i);
}

scheme_tree
block_to_scheme_tree (string s) {
  scheme_tree p (TUPLE);
  int         i= 0;
  while ((i < N (s)) && (is_spc (s[i]) || s[i] == ')'))
    i++;
  while (i < N (s)) {
    p << string_to_scheme_tree (s, i);
    while ((i < N (s)) && (is_spc (s[i]) || s[i] == ')'))
      i++;
  }
  return p;
}

/******************************************************************************
 * Handling escape characters
 ******************************************************************************/

string
slash (string s) {
  int    i, n= N (s);
  string r;
  for (i= 0; i < n; i++)
    switch (s[i]) {
    case '(':
    case ')':
    case ' ':
    case '\'':
      if ((n < 2) || (s[0] != '\042') || (s[n - 1] != '\042')) r << "\\";
      r << s[i];
      break;
    case '\\':
      r << '\\' << s[i];
      break;
    case '\042':
      if (((i == 0) && (s[n - 1] == '\042')) ||
          ((i == (n - 1)) && (s[0] == '\042')))
        r << s[i];
      else r << "\\" << s[i];
      break;
    case ((char) 0):
      r << "\\0";
      break;
    case '\t':
      r << "\\t";
      break;
    case '\n':
      r << "\\n";
      break;
    default:
      r << s[i];
    }
  return r;
}

/******************************************************************************
 * Converting scheme trees to strings
 ******************************************************************************/

static void
scheme_tree_to_string (string &out, scheme_tree p) {
  if (!is_tuple (p)) {
    string s= p->label;
    if (is_quoted (s)) out << scm_quote (raw_unquote (s));
    else out << slash (s);
  }
  else {
    if (is_tuple (p, "\'", 1)) {
      out << "\'";
      scheme_tree_to_string (out, p[1]);
    }
    else {
      int i, n= N (p);
      out << "(";
      for (i= 0; i < n; i++) {
        if (i > 0) out << " ";
        scheme_tree_to_string (out, p[i]);
      }
      out << ")";
    }
  }
}

string
scheme_tree_to_string (scheme_tree p) {
  string out;
  scheme_tree_to_string (out, p);
  return out;
}

string
scheme_tree_to_block (scheme_tree p) {
  string out;
  int    i, n= N (p);
  for (i= 0; i < n; i++)
    out << scheme_tree_to_string (p[i]) << "\n";
  return out;
}

tree
scheme_tree_to_tree (scheme_tree t, hashmap<string, int> codes, bool flag) {
  if (is_atomic (t)) return scm_unquote (t->label);
  else if ((N (t) == 0) || is_compound (t[0])) {
    convert_error << "Invalid scheme tree " << t << "\n";
    return compound (
        "errput", concat ("The tree was ", as_string (L (t)), ": ", tree (t)));
  }
  else {
    int        i, n= N (t);
    tree_label code= (tree_label) codes[t[0]->label];
    if (flag) code= make_tree_label (t[0]->label);
    if (code == UNKNOWN) {
      tree u (EXPAND, n);
      u[0]= copy (t[0]);
      for (i= 1; i < n; i++)
        u[i]= scheme_tree_to_tree (t[i], codes, flag);
      return u;
    }
    else {
      tree u (code, n - 1);
      for (i= 1; i < n; i++)
        u[i - 1]= scheme_tree_to_tree (t[i], codes, flag);
      return u;
    }
  }
}

tree
scheme_tree_to_tree (scheme_tree t) {
  return scheme_tree_to_tree (t, STD_CODE, true);
}

tree
scheme_to_tree (string s) {
  return scheme_tree_to_tree (string_to_scheme_tree (s));
}

/******************************************************************************
 * Conversion from trees to scheme trees
 ******************************************************************************/

scheme_tree
tree_to_scheme_tree (tree t) {
  if (is_atomic (t)) return scm_quote (t->label);
  else if (is_func (t, EXPAND) && is_atomic (t[0])) {
    int  i, n= N (t);
    tree u (TUPLE, n);
    u[0]= copy (t[0]);
    for (i= 1; i < n; i++)
      u[i]= tree_to_scheme_tree (t[i]);
    return u;
  }
  else {
    int    i, n= N (t);
    tree   u (TUPLE, n + 1);
    string s= as_string (L (t));
    if (N (s) > 0 && is_digit (s[0]))
      if (is_int (s)) s= "'" * s;
    u[0]= copy (s);
    for (i= 0; i < n; i++)
      u[i + 1]= tree_to_scheme_tree (t[i]);
    return u;
  }
}

/******************************************************************************
 * Conversion of trees to scheme strings
 ******************************************************************************/

string
tree_to_scheme (tree t) {
  return scheme_tree_to_string (tree_to_scheme_tree (t));
}
