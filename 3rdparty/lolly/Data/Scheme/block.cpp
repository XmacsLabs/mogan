
/******************************************************************************
* MODULE     : block.cpp
* DESCRIPTION: A block of Scheme data
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
                   2023  Darcy Shen
                   2023  Charonxin
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "block.hpp"
#include "analyze.hpp"

/******************************************************************************
 * Handling escape characters
 ******************************************************************************/

static int UNKNOWN= 1;
static int TUPLE  = 247;

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
string_to_scheme_tree (string s, int& i) {
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

inline bool
is_tuple (tree t) {
  return t->op == TUPLE;
}

inline bool
is_tuple (tree t, const char* s, int n) {
  return (t->op == TUPLE) && (N (t) == (n + 1)) && (t[0] == s);
}

static void
scheme_tree_to_string (string& out, scheme_tree p) {
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
