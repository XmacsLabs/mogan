
/******************************************************************************
 * MODULE     : prog_language.cpp
 * DESCRIPTION: Parser and syntax-highlighter for programming languages
 * COPYRIGHT  : (C) 2020  Darcy Shen
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
#include "preferences.hpp"
#include "scheme.hpp"
#include "tm_url.hpp"
#include "tree_helper.hpp"

prog_language_rep::prog_language_rep (string name)
    : abstract_language_rep (name) {
  if (DEBUG_PARSER)
    debug_packrat << "Building the " * name * " language parser" << LF;

  string use_modules= "(use-modules (code " * name * "-lang))";
  eval (use_modules);

  tree keyword_config= get_parser_config (name, "keyword");
  customize_keyword (keyword_parser, keyword_config);

  tree identifier_config= get_parser_config (name, "identifier");
  customize_identifier (identifier_parser, identifier_config);

  tree operator_config= get_parser_config (name, "operator");
  customize_operator (operator_config);

  tree number_config= get_parser_config (name, "number");
  customize_number (number_config);

  tree string_config= get_parser_config (name, "string");
  customize_string (string_config);

  tree comment_config= get_parser_config (name, "comment");
  customize_comment (comment_config);

  tree preprocessor_config= get_parser_config (name, "preprocessor");
  customize_preprocessor (preprocessor_config);
}

tree
prog_language_rep::get_parser_config (string lan, string key) {
  string cmd= "(tm->tree (parser-feature " * raw_quote (lan) * " " *
              raw_quote (key) * "))";
  return as_tree (eval (cmd));
}

void
prog_language_rep::customize_keyword (keyword_parser_rep p_keyword_parser,
                                      tree               config) {
  int config_N= N (config);
  for (int i= 0; i < config_N; i++) {
    tree   group_of_keywords  = config[i];
    int    group_of_keywords_N= N (group_of_keywords);
    string group              = get_label (group_of_keywords);
    if (group == "extra_chars") {
      for (int j= 0; j < group_of_keywords_N; j++) {
        string extra_char= get_label (group_of_keywords[j]);
        if (N (extra_char) == 1) {
          p_keyword_parser.insert_extra_char (extra_char[0]);
        }
      }
    }
    else {
      for (int j= 0; j < group_of_keywords_N; j++) {
        string word= get_label (group_of_keywords[j]);
        // number->string is actually number-<gtr>string
        p_keyword_parser.put (utf8_to_cork (word), group);
      }
    }
  }
}

void
prog_language_rep::customize_identifier (
    identifier_parser_rep p_identifier_parser, tree config) {
  int config_N= N (config);
  for (int i= 0; i < config_N; i++) {
    tree   group_of_keywords  = config[i];
    int    group_of_keywords_N= N (group_of_keywords);
    string group              = get_label (group_of_keywords);
    if (group == "extra_chars") {
      array<char> extra_chars= array<char> ();
      for (int j= 0; j < group_of_keywords_N; j++) {
        string extra_char= get_label (group_of_keywords[j]);
        if (N (extra_char) == 1) {
          extra_chars << extra_char[0];
        }
      }
      p_identifier_parser.set_extra_chars (extra_chars);
    }
    if (group == "start_chars") {
      array<char> start_chars= array<char> ();
      for (int j= 0; j < group_of_keywords_N; j++) {
        string start_char= get_label (group_of_keywords[j]);
        if (N (start_char) == 1) {
          start_chars << start_char[0];
        }
      }
      p_identifier_parser.set_start_chars (start_chars);
    }
  }
}

void
prog_language_rep::customize_operator (tree config) {
  for (int i= 0; i < N (config); i++) {
    tree   group_of_opers= config[i];
    string group         = get_label (group_of_opers);
    for (int j= 0; j < N (group_of_opers); j++) {
      string word= get_label (group_of_opers[j]);
      operator_parser.put (tm_encode (word), group);
    }
  }
}

void
prog_language_rep::customize_number (tree config) {
  for (int i= 0; i < N (config); i++) {
    tree   feature= config[i];
    string name   = get_label (feature);
    if (name == "bool_features") {
      for (int j= 0; j < N (feature); j++) {
        string key= get_label (feature[j]);
        number_parser.insert_bool_feature (key);
      }
    }
    else if (name == "separator" && N (feature) == 1) {
      string key= get_label (feature[0]);
      number_parser.support_separator (key);
    }
    else if (name == "suffix") {
      customize_keyword (number_parser.get_suffix_parser (), feature);
    }
  }
}

void
prog_language_rep::customize_string (tree config) {
  hashmap<string, string> pairs;
  int                     config_N= N (config);
  for (int i= 0; i < config_N; i++) {
    tree   feature  = config[i];
    string name     = get_label (feature);
    int    feature_N= N (feature);
    if (name == "bool_features") {
      for (int j= 0; j < feature_N; j++) {
        string key= get_label (feature[j]);
        escaped_char_parser.insert_bool_feature (key);
      }
    }
    else if (name == "escape_sequences") {
      array<string> escape_seq;
      for (int j= 0; j < feature_N; j++) {
        string key= get_label (feature[j]);
        escape_seq << key;
      }
      escaped_char_parser.set_sequences (escape_seq);
    }
    else if (name == "pairs") {
      for (int j= 0; j < feature_N; j++) {
        string key = get_label (feature[j]);
        pairs (key)= key;
      }
    }
  }

  string_parser.set_escaped_char_parser (escaped_char_parser);
  if (N (pairs) == 0) {
    pairs ("\"")= "\"";
    pairs ("\'")= "\'";
  }
  string_parser.set_pairs (pairs);
  if (DEBUG_PARSER) debug_packrat << string_parser.to_string ();
}

void
prog_language_rep::customize_comment (tree config) {
  for (int i= 0; i < N (config); i++) {
    tree   feature= config[i];
    string label  = get_label (feature);
    if (label == "inline") {
      array<string> inline_comment_starts;
      for (int i= 0; i < N (feature); i++) {
        inline_comment_starts << get_label (feature[i]);
      }
      inline_comment_parser.set_starts (inline_comment_starts);
    }
  }
}

void
prog_language_rep::customize_preprocessor (tree config) {
  for (int i= 0; i < N (config); i++) {
    tree   feature= config[i];
    string name   = get_label (feature);
    if (name == "directives") {
      array<string> directives;
      for (int j= 0; j < N (feature); j++) {
        string key= get_label (feature[j]);
        directives << key;
      }
      preprocessor_parser.set_directives (directives);
    }
  }
  if (DEBUG_PARSER) debug_packrat << preprocessor_parser.to_string ();
}

text_property
prog_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos >= N (s)) return &tp_normal_rep;

  if (string_parser.unfinished ()) {
    if (string_parser.escaped () && string_parser.parse_escaped (s, pos)) {
      current_parser= escaped_char_parser.get_parser_name ();
      return &tp_normal_rep;
    }
    if (string_parser.parse (s, pos)) {
      current_parser= string_parser.get_parser_name ();
      return &tp_normal_rep;
    }
  }

  if (blanks_parser.parse (s, pos)) {
    current_parser= blanks_parser.get_parser_name ();
    return &tp_space_rep;
  }
  if (preprocessor_parser.parse (s, pos)) {
    current_parser= preprocessor_parser.get_parser_name ();
    return &tp_normal_rep;
  }
  if (string_parser.parse (s, pos)) {
    current_parser= string_parser.get_parser_name ();
    return &tp_normal_rep;
  }
  if (keyword_parser.parse (s, pos)) {
    current_parser= keyword_parser.get_parser_name ();
    return &tp_normal_rep;
  }
  if (number_parser.parse (s, pos)) {
    current_parser= number_parser.get_parser_name ();
    return &tp_normal_rep;
  }
  if (operator_parser.parse (s, pos)) {
    current_parser= operator_parser.get_parser_name ();
    return &tp_normal_rep;
  }
  if (identifier_parser.parse (s, pos)) {
    current_parser= identifier_parser.get_parser_name ();
    return &tp_normal_rep;
  }

  tm_char_forwards (s, pos);
  current_parser= "";

  return &tp_normal_rep;
}

array<int>
prog_language_rep::get_hyphens (string s) {
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
prog_language_rep::hyphenate (string s, int after, string& left,
                              string& right) {
  left = s (0, after);
  right= s (after, N (s));
}

string
prog_language_rep::get_color (tree t, int start, int end) {
  static string none= "";
  if (start >= end) return none;

  // Coloring as multi-line comment
  if (in_comment (start, t))
    return decode_color (lan_name, encode_color ("comment"));

  string type= none;
  string s   = t->label;

  // Coloring as inline comment
  int pos= 0;
  while (pos <= start) {
    if (inline_comment_parser.can_parse (s, pos)) {
      return decode_color (lan_name, encode_color ("comment"));
    }
    pos++;
  }

  if (current_parser == "string_parser") {
    type= "constant_string";
  }
  else if (current_parser == "escaped_char_parser") {
    type= "constant_char";
  }
  else if (current_parser == "number_parser") {
    type= "constant_number";
  }
  else if (current_parser == "operator_parser") {
    string oper= s (start, end);
    type       = operator_parser.get (oper);
  }
  else if (current_parser == "keyword_parser") {
    string keyword= s (start, end);
    type          = keyword_parser.get (keyword);
  }
  else if (current_parser == "preprocessor_parser") {
    type= "preprocessor_directive";
  }
  else {
    type= none;
  }

  if (type == none) return none;
  return decode_color (lan_name, encode_color (type));
}

bool
prog_lang_exists (string s) {
  return exists (url_system ("$TEXMACS_PATH/progs/prog/" * s * "-lang.scm")) ||
         exists (url_system ("$TEXMACS_PATH/plugins/" * s * "/progs/code/" * s *
                             "-lang.scm")) ||
         exists (url_system ("$TEXMACS_PATH/plugins/code/progs/code/" * s *
                             "-lang.scm")) ||
         exists (url_system ("$TEXMACS_HOME_PATH/plugins/" * s *
                             "/progs/code/" * s * "-lang.scm")) ||
         exists (url_system ("$TEXMACS_HOME_PATH/plugins/code/progs/code/" * s *
                             "-lang.scm"));
}

bool
ast_prog_lang_exists (string s) {
  return exists (
             url_system ("$TEXMACS_PATH/progs/prog/" * s * "-ast-lang.scm")) ||
         exists (url_system ("$TEXMACS_PATH/plugins/" * s * "/progs/code/" * s *
                             "-ast-lang.scm")) ||
         exists (url_system ("$TEXMACS_PATH/plugins/code/progs/code/" * s *
                             "-ast-lang.scm")) ||
         exists (url_system ("$TEXMACS_HOME_PATH/plugins/" * s *
                             "/progs/code/" * s * "-ast-lang.scm")) ||
         exists (url_system ("$TEXMACS_HOME_PATH/plugins/code/progs/code/" * s *
                             "-ast-lang.scm"));
}

/******************************************************************************
 * Interface
 ******************************************************************************/
language
prog_language (string s) {
  string use_ast= get_user_preference ("ast-syntax-highlighting");
  if (use_ast == "on") {
    // cout << "Load prog_language " << s << " use_ast: " << use_ast << "\n";
    if (language::instances->contains (s * "-ast"))
      return language (s * "-ast");
    if (format_exists (s) && ast_prog_lang_exists (s)) {
      return make (language, s * "-ast", tm_new<ast_language_rep> (s * "-ast"));
    }
  }

  if (language::instances->contains (s)) return language (s);

  if (s == "scheme") return make (language, s, tm_new<scheme_language_rep> (s));

  if (format_exists (s) && prog_lang_exists (s))
    return make (language, s, tm_new<prog_language_rep> (s));

  if (format_exists (s) && ast_prog_lang_exists (s))
    return make (language, s * "-ast", tm_new<ast_language_rep> (s * "-ast"));

  return make (language, s, tm_new<verb_language_rep> (s));
}
