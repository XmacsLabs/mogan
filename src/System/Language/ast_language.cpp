
/******************************************************************************
 * MODULE     : ast_language.cpp
 * DESCRIPTION: AST Syntax-highlighter for programming languages
 * COPYRIGHT  : (C) 2024  UnbSky
 *******************************************************************************
 * This software falls under the GNU general public license and comes WITHOUT
 * ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
 * If you don't have this file, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 ******************************************************************************/

#include "analyze.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "cork.hpp"
#include "impl_language.hpp"
#include "iterator.hpp"
#include "observers.hpp"
#include "path.hpp"
#include "scheme.hpp"
#include "tm_url.hpp"
#include "tree_helper.hpp"

// TODO:Write Scheme tests

ast_language_rep::ast_language_rep (string name) : language_rep (name) {

  ast_parser= ts_parser_new ();
  ts_parser_set_language (ast_parser, tree_sitter_cpp ());

  if (DEBUG_PARSER)
    debug_packrat << "Building the " * name * " language parser" << LF;

  string use_modules= "(use-modules (code " * name * "-lang))";
  eval (use_modules);

  tree keytoken_config= get_parser_config (name, "keytoken");
  customize_keytokens (keytoken_config);

  tree theme_config= get_parser_config (name, "light_theme");
  customize_highlight_theme (theme_config);
}

tree
ast_language_rep::get_parser_config (string lan, string key) {
  string cmd= "(tm->tree (parser-feature " * raw_quote (lan) * " " *
              raw_quote (key) * "))";
  return as_tree (eval (cmd));
}

void
ast_language_rep::customize_keytokens (tree config) {
  for (int i= 0; i < N (config); i++) {
    tree   group_of= config[i];
    string group   = get_label (group_of);
    for (int j= 0; j < N (group_of); j++) {
      string word= get_label (group_of[j]);
      // word = cork_to_utf8(word);
      if (!is_empty (word)) {
        keytoken_group (word)= group;
      }
    }
  }
}

void
ast_language_rep::customize_highlight_theme (tree config) {
  theme_group= hashmap<string, string> ();
  for (int i= 0; i < N (config); i++) {
    tree   group_of= config[i];
    string col     = get_label (group_of);
    for (int j= 0; j < N (group_of); j++) {
      string token_name= get_label (group_of[j]);
      if (!is_empty (token_name)) {
        theme_group (token_name)= col (1, N (col));
      }
    }
  }
}

extern tree the_et;
text_property
ast_language_rep::advance (tree t, int& pos) {
  // Jump NBSP "-" op=807
  string s= t->label;
  tree&  my_father (subtree (the_et, reverse (obtain_ip (t)->next)));
  if (my_father->op == NBSP_OP) {
    pos+= 1;
    token_type= "none";
    if (s == " ") return &tp_space_rep;
    else return &tp_normal_rep;
  }
  int       start_index= 0;
  int       code_hash  = 0;
  string_u8 code= lang_ast_parser.get_code_str (t, start_index, code_hash);

  if (lang_ast_parser.check_to_compile (code_hash) || code_hash == 0 ||
      lang_ast_parser.get_token_index () == lang_ast_parser.get_token_num ()) {
    lang_ast_parser.do_ast_parse (code);
    lang_ast_parser.set_token_start (start_index);

    // cout << "Current Start Index:" << start_index << " "
    //      << lang_ast_parser.get_token_index () << "\nCurrent Line:[" << s
    //      << "]\nFullCode:\n"
    //      << N (s) << " " << N (code) << " [\n"
    //      << code << "]\n";
  }
  if (pos >= N (s)) return &tp_normal_rep;

  // Avoid Error (if the token index is incorrect, avoid throwing an error
  // directly and instead highlight the error)
  if (lang_ast_parser.get_token_index () > lang_ast_parser.get_token_num ()) {
    cout << "ERROR token index out of bounds\n";
    token_type= "INNER_ERROR";
    pos+= 1;
    return &tp_normal_rep;
  }

  int token_end     = lang_ast_parser.current_token_end ();
  int token_property= lang_ast_parser.current_token_property ();
  int barcket_index = lang_ast_parser.current_brackets_index ();
  token_type        = lang_ast_parser.current_token_type ();
  lang_ast_parser.next_token ();
  pos= token_end - start_index;
  // Colorful Barckets
  if (barcket_index > 0) {
    token_type= token_type * as_string (barcket_index % 3);
  }
  // Keyword and Operator
  if (keytoken_group->contains (token_type)) {
    token_type= keytoken_group[token_type];
  }

  // cout << "nextpos " << pos << " |N (s) " << N (s) << " |token_end "
  //      << token_end << " |nextindex " << lang_ast_parser.get_token_index ()
  //      << " |maxindex " << lang_ast_parser.get_token_num ()
  //      << " |tokenproperty " << token_property << " |tokentype " <<
  //      token_type
  //      << "\n";

  // token_property
  if (token_property == 0) {
    return &tp_space_rep;
  }
  else {
    return &tp_normal_rep;
  }
}

array<int>
ast_language_rep::get_hyphens (string s) {
  int        i;
  array<int> penalty (N (s) + 1);
  penalty[0]= HYPH_INVALID;
  int len   = N (s);
  for (i= 1; i < len; i++)
    if (s[i - 1] == '-' && is_alpha (s[i])) penalty[i]= HYPH_STD;
    else penalty[i]= HYPH_INVALID;
  penalty[i]= HYPH_INVALID;
  return penalty;
}

void
ast_language_rep::hyphenate (string s, int after, string& left, string& right) {
  left = s (0, after);
  right= s (after, N (s));
}

string
ast_language_rep::get_color (tree t, int start, int end) {
  static string col= "#000000";
  if (start >= end) return col;

  if (theme_group->contains (token_type)) {
    col= theme_group[token_type];
  }
  else {
    // if(token_type != "Space") cout << "Unkown Theme Token Type: " <<
    // token_type << " Token:" << t->label(start,end) << "\n";
  }
  return col;
}