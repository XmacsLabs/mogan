
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
#include <moebius/tree_label.hpp>

using moebius::make_tree_label;

// TODO:Write Scheme tests

ast_language_rep::ast_language_rep (string name) : language_rep (name) {
  string lan_name= name (0, N (name) - 4);

  if (DEBUG_PARSER)
    debug_packrat << "Building the " * name * " language parser" << LF;

  string use_modules= "(use-modules (code " * name * "-lang))";
  eval (use_modules);

  tree lang_id_config= get_parser_config (lan_name, "id");
  if (N (lang_id_config) > 0) {
    string lang_id = get_label (lang_id_config[0]);
    lang_ast_parser= tm_new<lang_parser> (lang_id);
  }
  else {
    lang_ast_parser= tm_new<lang_parser> (lan_name);
  }

  tree keytoken_config= get_parser_config (lan_name, "keytoken");
  customize_keytokens (keytoken_config);

  tree brackets_config= get_parser_config (lan_name, "brackets");
  customize_brackets_match (brackets_config);

  tree theme_config= get_parser_config (lan_name, "light_theme");
  customize_highlight_theme (theme_config);

  tree special_symbol_config= get_parser_config (lan_name, "special_symbol");
  customize_special_symbol (special_symbol_config);

  tree nbsp_tree (make_tree_label ("nbsp"));
  nbsp_op= nbsp_tree->op;
}

tree
ast_language_rep::get_parser_config (string lan, string key) {
  string cmd= "(stree->tree (parser-feature " * raw_quote (lan) * " " *
              raw_quote (key) * "))";
  return as_tree (eval (cmd));
}

void
ast_language_rep::customize_keytokens (tree config) {
  for (tree group_of : config) {
    string group= get_label (group_of);
    for (tree data_list : group_of) {
      string word= get_label (data_list);
      // word = cork_to_utf8(word);
      if (!is_empty (word)) {
        keytoken_group (word)= group;
      }
    }
  }
}

void
ast_language_rep::customize_brackets_match (tree config) {
  lang_ast_parser->reset_brackets_pair ();
  match_group= hashmap<string, int> ();
  for (tree group_of : config) {
    int cycle= as_int (get_label (group_of));
    if (N (group_of) == 2) {
      string forward        = get_label (group_of[0]);
      string backward       = get_label (group_of[1]);
      match_group (forward) = cycle;
      match_group (backward)= cycle;
      lang_ast_parser->add_brackets_pair (forward, backward);
    }
  }
}

void
ast_language_rep::customize_highlight_theme (tree config) {
  theme_group= hashmap<string, string> ();
  for (tree group_of : config) {
    string col= get_label (group_of);
    for (tree data_list : group_of) {
      string token_name= get_label (data_list);
      if (!is_empty (token_name)) {
        theme_group (token_name)= col (1, N (col) - 1);
      }
    }
  }
}

void
ast_language_rep::customize_special_symbol (tree config) {
  tokenize_set= hashset<string> ();
  for (tree group_of : config) {
    string special_type= get_label (group_of);
    for (tree data_list : group_of) {
      string symbol_name= get_label (data_list);
      if (special_type == "tokenize") tokenize_set->insert (symbol_name);
    }
  }
}

extern tree the_et;
text_property
ast_language_rep::advance (tree t, int& pos) {
  // Jump NBSP ["-"," "] in detail
  string s= t->label;
  tree&  father_node (subtree (the_et, reverse (obtain_ip (t)->next)));
  if (father_node->op == nbsp_op) {
    pos+= 1;
    token_type= "none";
    if (s == " ") return &tp_space_rep;
    else return &tp_normal_rep;
  }

  // Line Changed Check
  if (lang_ast_parser->check_line_changed (t) ||
      lang_ast_parser->get_token_index () ==
          lang_ast_parser->get_token_num ()) {
    start_index   = 0;
    int  code_hash= 0;
    tree root_node= lang_ast_parser->get_root_node (t, start_index, code_hash);

    // Need Recompile Check
    if (lang_ast_parser->check_to_compile (code_hash) || code_hash == 0 ||
        lang_ast_parser->get_token_index () ==
            lang_ast_parser->get_token_num ()) {
      lang_ast_parser->do_ast_parse (root_node);
      lang_ast_parser->set_token_start (start_index);
      // cout << "Do AST Parse\n";
    }

    // string_u8 code= lang_ast_parser->code_string;
    // cout << "Current Start Index:" << start_index << " "
    //       << lang_ast_parser->get_token_index () << "\nCurrent Line:[" << s
    //       << "]\nFullCode:\n"
    //       << N (s) << " " << N (code) << " [\n"
    //       << code << "]\n";
  }

  if (pos >= N (s)) return &tp_normal_rep;

  // Avoid Error (if the token index is incorrect, avoid throwing an error
  // directly and instead highlight the error)
  if (lang_ast_parser->get_token_index () >=
      lang_ast_parser->get_token_num ()) {
    // cout << "ERROR token index out of bounds\n";
    token_type= "INNER_ERROR";
    pos+= 1;
    return &tp_normal_rep;
  }

  int      token_end     = lang_ast_parser->current_token_end ();
  int      token_property= lang_ast_parser->current_token_property ();
  uint32_t bracket_index = lang_ast_parser->current_brackets_index ();
  token_type             = lang_ast_parser->current_token_type ();
  lang_ast_parser->next_token ();
  // Colorful brackets
  if (bracket_index > 0u) {
    token_type=
        token_type * as_string ((bracket_index - 1u) % match_group[token_type]);
  }
  // Symbol (symbol in scheme)
  if (tokenize_set->contains (token_type)) {
    string token= s (pos, token_end - start_index);
    // cout << token << LF;
    if (keytoken_group->contains (token)) {
      token_type= keytoken_group[token];
    }
  }
  // Keyword and Operator
  else if (keytoken_group->contains (token_type)) {
    token_type= keytoken_group[token_type];
  }
  pos= token_end - start_index;

  // cout << "nextpos " << pos << " |N (s) " << N (s) << " |token_end "
  //      << token_end << " |nextindex " << lang_ast_parser->get_token_index ()
  //      << " |maxindex " << lang_ast_parser->get_token_num ()
  //      << " |tokenproperty " << token_property << " |tokentype " <<
  //      token_type << "\n";

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
    // if (token_type != "Space")
    //   cout << "Unkown Theme Token Type: " << token_type
    //        << " Token:" << t->label (start, end) << "\n";
  }
  return col;
}
