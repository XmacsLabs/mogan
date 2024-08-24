
/******************************************************************************
 * MODULE     : impl_language.hpp
 * COPYRIGHT  : (C) 1999-2024  Joris van der Hoeven, Darcy Shen, UnbSky
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef IMPL_LANGUAGE_H
#define IMPL_LANGUAGE_H

#include "Treesitter/lang_parser.hpp"
#include "blanks_parser.hpp"
#include "escaped_char_parser.hpp"
#include "identifier_parser.hpp"
#include "inline_comment_parser.hpp"
#include "keyword_parser.hpp"
#include "language.hpp"
#include "number_parser.hpp"
#include "operator_parser.hpp"
#include "preprocessor_parser.hpp"
#include "string_parser.hpp"

extern text_property_rep tp_normal_rep;
extern text_property_rep tp_hyph_rep;
extern text_property_rep tp_thin_space_rep;
extern text_property_rep tp_space_rep;
extern text_property_rep tp_space_before_rep;
extern text_property_rep tp_dspace_rep;
extern text_property_rep tp_nb_thin_space_rep;
extern text_property_rep tp_nb_space_rep;
extern text_property_rep tp_nb_dspace_rep;
extern text_property_rep tp_period_rep;
extern text_property_rep tp_cjk_normal_rep;
extern text_property_rep tp_cjk_no_break_rep;
extern text_property_rep tp_cjk_period_rep;
extern text_property_rep tp_cjk_wide_period_rep;
extern text_property_rep tp_cjk_no_break_period_rep;
extern text_property_rep tp_half_rep;
extern text_property_rep tp_operator_rep;
extern text_property_rep tp_short_apply_rep;
extern text_property_rep tp_apply_rep;

int  line_number (tree t);
int  number_of_lines (tree t);
tree line_inc (tree t, int i);
bool in_comment (int pos, tree t);

struct abstract_language_rep : language_rep {
  hashmap<string, string>   colored;
  string                    current_parser;
  blanks_parser_rep         blanks_parser;
  inline_comment_parser_rep inline_comment_parser;
  number_parser_rep         number_parser;
  escaped_char_parser_rep   escaped_char_parser;
  keyword_parser_rep        keyword_parser;
  operator_parser_rep       operator_parser;
  identifier_parser_rep     identifier_parser;
  string_parser_rep         string_parser;
  preprocessor_parser_rep   preprocessor_parser;

  abstract_language_rep (string s) : language_rep (s){};
  virtual bool belongs_to_identifier (char c);
  void parse_identifier (hashmap<string, string>& t, string s, int& pos);
  void parse_type (hashmap<string, string>& t, string s, int& pos);
  void parse_keyword (hashmap<string, string>& t, string s, int& pos);
  void parse_constant (hashmap<string, string>& t, string s, int& pos);
};

struct verb_language_rep : language_rep {
  verb_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int>    get_hyphens (string s);
  void          hyphenate (string s, int after, string& left, string& right);
  string        get_color (tree t, int start, int end);
};

struct prog_language_rep : abstract_language_rep {
  prog_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int>    get_hyphens (string s);
  void          hyphenate (string s, int after, string& left, string& right);
  string        get_color (tree t, int start, int end);

  void customize_keyword (keyword_parser_rep parser, tree config);
  void customize_identifier (identifier_parser_rep parser, tree config);
  void customize_operator (tree config);
  void customize_number (tree config);
  void customize_string (tree config);
  void customize_preprocessor (tree config);
  void customize_comment (tree config);
  tree get_parser_config (string lan, string key);
};

struct scheme_language_rep : language_rep {
  hashmap<string, string> colored;
  scheme_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int>    get_hyphens (string s);
  void          hyphenate (string s, int after, string& left, string& right);
  string        get_color (tree t, int start, int end);
};

struct ast_language_rep : language_rep {
  lang_parser*            lang_ast_parser;
  hashmap<string, string> keytoken_group;
  hashmap<string, string> theme_group;
  hashmap<string, int>    match_group;
  hashset<string>         tokenize_set;
  string                  token_type;
  int                     nbsp_op;
  int                     start_index;

  ast_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int>    get_hyphens (string s);
  void          hyphenate (string s, int after, string& left, string& right);
  string        get_color (tree t, int start, int end);

  tree get_parser_config (string lan, string key);
  void customize_keytokens (tree config);
  void customize_highlight_theme (tree config);
  void customize_brackets_match (tree config);
  void customize_special_symbol (tree config);
};

struct cpp_language_rep : abstract_language_rep {
  cpp_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int>    get_hyphens (string s);
  void          hyphenate (string s, int after, string& left, string& right);
  string        get_color (tree t, int start, int end);

  void   parse_preprocessing (string s, int& pos);
  string get_identifier_type (string s, int& pos);
};

#endif // defined IMPL_LANGUAGE_H
