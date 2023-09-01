
/******************************************************************************
* MODULE     : file.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "analyze.hpp"
#include "array.hpp"
#include "url.hpp"


url
url_numbered (url dir, string prefix, string postfix, int i) {
  if (!exists (dir)) mkdir (dir);
  for (; true; i++) {
    url name= dir * (prefix * as_string (i) * postfix);
    if (!exists (name)) return name;
  }
  return dir * (prefix * "x" * postfix);
}

url
url_scratch (string prefix, string postfix, int i) {
  url dir ("$TEXMACS_HOME_PATH/texts/scratch");
  return url_numbered (dir, prefix, postfix, i);
}

bool
is_scratch (url u) {
  return head (u) == url ("$TEXMACS_HOME_PATH/texts/scratch");
}

string
file_format (url u) {
#if defined(KERNEL_L3)
  return "texmacs-file";
#else
  if (is_rooted_tmfs (u)) {
    return as_string (call ("tmfs-format", object (u)));
  }
  else return suffix_to_format (suffix (u));
#endif
}

static int
compute_score (string what, string in, int pos, string suf) {
  int score= 1;
  if (pos > 0 && !is_iso_alpha (in [pos-1]))
    if (pos + N(what) + 1 < N(in) && !is_iso_alpha (in [pos+N(what)]))
      score *= 10;
  if (suf == "tm") {
    if (precedes (in, pos, "<")) score= 0;
    else if (precedes (in, pos, "<\\")) score= 0;
    else if (precedes (in, pos, "<|")) score= 0;
    else if (precedes (in, pos, "</")) score= 0;
    else if (precedes (in, pos, "compound|")) score= 0;
    else if (precedes (in, pos, "<name|")) score *= 10;
    else if (precedes (in, pos, "<tmstyle|")) score *= 10;
    else if (precedes (in, pos, "<tmdtd|")) score *= 10;
    else if (precedes (in, pos, "<explain-macro|")) score *= 10;
    else if (precedes (in, pos, "<var-val|")) score *= 10;
  }
  else if (suf == "scm") {
    if (precedes (in, pos, "define ")) score *= 10;
    else if (precedes (in, pos, "define-public ")) score *= 10;
    else if (precedes (in, pos, "define (")) score *= 10;
    else if (precedes (in, pos, "define-public (")) score *= 10;
    else if (precedes (in, pos, "define-macro ")) score *= 10;
    else if (precedes (in, pos, "define-public-macro ")) score *= 10;
    else if (precedes (in, pos, "define-macro (")) score *= 10;
    else if (precedes (in, pos, "define-public-macro (")) score *= 10;
  }
  return score;
}

static int
compute_score (string what, string in, array<int> pos, string suf) {
  int score= 0, i= 0, n= N(pos);
  for (i=0; i<n; i++)
    score += compute_score (what, in, pos[i], suf);
  return score;
}

string
escape_cork_words (string s) {
  int i;
  string r;
  for (i=0; i<N(s); i++) {
    if (s[i]=='<') {
      int j;
      for (j=i+1; j<N(s); j++)
        if (s[j]=='>') break;
      if (j<N(s)) j++;
      if (i+7==j && s[i+1]=='#' && s[j-1]=='>') {
        r << "\\<";
        r << s(i+1, j-1);
        r << "\\>";
        i=j-1;
      }
    } else {
      r << s[i];
    }
  }
  return r;
}

#if !defined(KERNEL_L3)
int
search_score (url u, array<string> a) {
  int n= N(a);
  string in= grep_load (u);
  if (N(in) == 0) return 0;

  string suf= suffix (u);
  if (suf == "tmml") {
    for (int i=0; i<n; i++)
      a[i]= cork_to_utf8 (a[i]);
  } else if (suf == "tm") {
    in= locase_all (in);
    for (int i=0; i<n; i++)
      a[i]= locase_all (escape_cork_words (a[i]));
  } else {
    in= locase_all (in);
    for (int i=0; i<n; i++)
      a[i]= locase_all (a[i]);
  }

  int score= 1;
  for (int i=0; i<n; i++) {
    string what= a[i];
    array<int> pos= search (what, in);
    score *= compute_score (what, in, pos, suf);
    if (score == 0) return 0;
    if (score > 1000000) score= 1000000;
  }
  return score;
}
#endif
