
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
#include "tree_helper.hpp"

using namespace moebius;

/******************************************************************************
 * Handling escape characters
 ******************************************************************************/
void
unslash (string& s, int i, int end_index, string& r, int& r_index) {
  char ch= s[i];
  while (i < end_index) {
    if ((ch == '\\') && ((i + 1) < end_index)) {
      i++;
      ch= s[i];
      switch (ch) {
      case '0':
        r[r_index]= ((char) 0);
        r_index++;
        break;
      case 'n':
        r[r_index]= '\n';
        r_index++;
        break;
      case 't':
        r[r_index]= '\t';
        r_index++;
        break;
      default:
        r[r_index]= ch;
        r_index++;
      }
    }
    else {
      r[r_index]= ch;
      r_index++;
    }
    i++;
    ch= s[i];
  }
}

/******************************************************************************
 * Converting strings to scheme trees
 ******************************************************************************/

static bool
is_spc (char c) {
  return (c == ' ') || (c == '\t') || (c == '\n');
}

static bool
is_paren_or_spc (char c) {
  return (c == ' ') || (c == '\t') || (c == '\n') || (c == '(') || (c == ')');
};

static scheme_tree
string_to_scheme_tree (string& s, int& i, const int length) {
  for (; i < length; i++)
    switch (s[i]) {

    case ' ':
    case '\t':
    case '\n':
      break;
    case '(': {
      scheme_tree p (TUPLE);
      i++;
      while (true) {
        while (is_spc (s[i]) && (i < length))
          i++;
        if ((i == length) || (s[i] == ')')) break;
        p << string_to_scheme_tree (s, i, length);
      }
      if (i < length) i++;
      return p;
    }

    case '\'':
      i++;
      return scheme_tree (TUPLE, tree ("\'"),
                          string_to_scheme_tree (s, i, length));

    case '\"': { // "
      i++;
      int       end_index  = i;
      const int start_index= i;
      char      ch         = s[end_index];
      while (!(ch == '\"') && end_index < length) {
        if ((ch == '\\') && (end_index < length - 1)) end_index++;
        end_index++;
        ch= s[end_index];
      }
      const int r_size      = 1; // N ("\"");
      int       quoted_index= r_size;
      string    quoted (r_size + end_index - i);
      quoted[0]= '"';
      unslash (s, start_index, end_index, quoted, quoted_index);
      if (i < length) {
        i= end_index + 1;
      }
      else {
        i= end_index;
      };
      quoted->resize (quoted_index + 1);
      quoted[quoted_index]= '"';
      return scheme_tree (quoted);
    }

    case ';':
      while ((i < length) && (s[i] != '\n'))
        i++;
      break;

    default: {
      int       end_index  = i;
      const int start_index= i;
      char      ch         = s[end_index];
      while (!(is_paren_or_spc (ch)) && end_index < length) {
        if ((ch == '\\') && (end_index < length - 1)) end_index++;
        end_index++;
        ch= s[end_index];
      }
      const int r_size     = 0; // empty string
      int       token_index= r_size;
      string    token (r_size + end_index - i);
      unslash (s, start_index, end_index, token, token_index);
      i= end_index;
      token->resize (token_index);
      return scheme_tree (token);
    }
    }

  return scheme_tree ("");
}

scheme_tree
string_to_scheme_tree (string s) {
  s               = replace (s, "\015", "");
  int       i     = 0;
  const int length= N (s);
  return string_to_scheme_tree (s, i, length);
}

scheme_tree
block_to_scheme_tree (string s) {
  scheme_tree p (TUPLE);
  int         i     = 0;
  const int   length= N (s);
  while ((i < length) && (is_spc (s[i]) || s[i] == ')'))
    i++;
  while (i < length) {
    p << string_to_scheme_tree (s, i, length);
    while ((i < length) && (is_spc (s[i]) || s[i] == ')'))
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
