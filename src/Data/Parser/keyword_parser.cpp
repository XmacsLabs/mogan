
/******************************************************************************
 * MODULE     : keyword_parser.cpp
 * DESCRIPTION: shared keyword parsing routines
 * COPYRIGHT  : (C) 2020  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "keyword_parser.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "iterator.hpp"
#include "scheme.hpp"
#include "tree.hpp"
#include "tree_helper.hpp"

keyword_parser_rep::keyword_parser_rep () {
  current_keyword= "";
  keyword_group  = hashmap<string, string> ();
  extra_chars    = array<char> ();
}

void
keyword_parser_rep::insert_extra_char (char extra_char) {
  extra_chars << extra_char;
}

bool
read_keyword (string s, int& i, string& result, array<char> extras) {
  int opos= i;
  int s_N = N (s);
  // a keyword must start with alpha or start with extra chars
  if (i < s_N && (is_alpha (s[i] || contains (s[i], extras)))) i++;
  // a keyword is consist of alpha/number/extra chars
  while (i < s_N &&
         (is_alpha (s[i]) || is_digit (s[i]) || contains (s[i], extras))) {
    i++;
  }
  result= s (opos, i);
  return i > opos;
}

bool
keyword_parser_rep::can_parse (string s, int pos) {
  string word;
  bool   hit= read_keyword (s, pos, word, extra_chars) &&
            keyword_group->contains (word);
  if (hit) current_keyword= word;
  return hit;
}

void
keyword_parser_rep::do_parse (string s, int& pos) {
  pos+= N (current_keyword);
}

void
keyword_parser_rep::use_keywords_of_lang (string lang_code) {
  string use_modules= "(use-modules (prog " * lang_code * "-lang))";
  eval (use_modules);
  string get_list_of_keywords_tree=
      "(map tm->tree (" * lang_code * "-keywords))";
  list<tree> l= as_list_tree (eval (get_list_of_keywords_tree));
  if (DEBUG_PARSER)
    debug_packrat << "Keywords definition of [" << lang_code << "] loaded!\n";
  int l_N= N (l);
  for (int i= 0; i < l_N; i++) {
    tree   group_words  = l[i];
    string group        = get_label (group_words);
    int    group_words_N= N (group_words);
    for (int j= 0; j < group_words_N; j++) {
      string word= get_label (group_words[j]);
      // number->string is actually number-<gtr>string
      put (utf8_to_cork (word), group);
    }
  }
}
