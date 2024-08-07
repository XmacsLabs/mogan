/******************************************************************************
 * MODULE     : lang_parser.hpp
 * DESCRIPTION: Parser for Programming languages
 * COPYRIGHT  : (C) 2024 UnbSky
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "lang_parser.hpp"
#include "converter.hpp"
#include "list.hpp"
#include "observers.hpp"
#include "path.hpp"
#include "tm_timer.hpp"

lang_parser::lang_parser () {
  // TODO: Dynamic loading of shared lib and multilingual switching
  ast_parser= ts_parser_new ();
  ts_parser_set_language (ast_parser, tree_sitter_cpp ());
}

void
lang_parser::get_code_from_root (tree root, tree line, string& code,
                                 string_u8& code_u8, string& tag,
                                 int& start_index) {
  for (int i= 0; i < N (root); i++) {
    if (root[i]->op == 807) {
      tag= tag * as_string (N (obtain_ip (root[i])));
    }
    else {
      if (is_atomic (root[i])) {
        // cout << "Code Leaf: " << root[i] << " " << obtain_ip(root[i]) <<
        // "\n";
        tag    = tag * as_string (N (obtain_ip (root[i])));
        code   = code * root[i]->label;
        code_u8= code_u8 * cork_to_utf8 (root[i]->label);
        if (obtain_ip (root[i]) == obtain_ip (line)) {
          start_index= N (code) - N (root[i]->label);
        }
        // cout << "Line Change: " <<  N(code) << "\n";
        change_line_pos << N (code);
        if (root[i]->label == " " || N (root[i]->label) == 0) {
          code   = code * " ";
          code_u8= code_u8 * " ";
        }
        else {
          code   = code * "\n";
          code_u8= code_u8 * "\n";
        }
      }
      else {
        get_code_from_root (root[i], line, code, code_u8, tag, start_index);
      }
    }
  }
}

extern tree the_et;

string_u8
lang_parser::get_code_str (tree t, int& start_index, int& hash_code) {
  change_line_pos= list<int> ();
  path father    = obtain_ip (t)->next;
  while (N (father) > 3) {
    father= father->next;
  }
  tree& parent (subtree (the_et, reverse (father)));
  // cout << "Current Node " << t << " Current IP " << obtain_ip(t) << " Father
  // IP " << father << "\n";
  start_index        = 0;
  string    code_cork= "";
  string    tag      = "";
  string_u8 code     = "";
  get_code_from_root (parent, t, code_cork, code, tag, start_index);

  real_code_len= N (code_cork);
  if (N (code_cork) == 0 && N (t->label) >= 1) {
    code= cork_to_utf8 (t->label) * " ";
    change_line_pos << 1;
  }
  // for(int i = 0; i < N(code); i++){
  //   cout << code[i] << " " << (int)code[i] << "\n";
  // }
  hash_code= (hash (code) / 2) + (hash (tag) / 2);
  if (N (code) <= 1) hash_code= 0;
  // cout << "\nGen Hash Code " << hash_code << " C" << hash(code) / 2 << " T"
  // << tag << " " << current_code_hash << "\n";
  return code;
}

bool
lang_parser::check_to_compile (int hash_code) {
  if (hash_code != current_code_hash) {
    current_code_hash= hash_code;
    return true;
  }
  return false;
}

void
lang_parser::collect_leaf_nodes (TSNode node, list<TSNode>& tsnodes) {
  uint32_t child_count= ts_node_child_count (node);
  if (child_count == 0) {
    tsnodes << node;
  }
  else {
    for (uint32_t i= 0; i < child_count; ++i) {
      collect_leaf_nodes (ts_node_child (node, i), tsnodes);
    }
  }
}

void
lang_parser::is_change_line_between (int start, int end, int& cl_low,
                                     int& cl_high) {
  for (int i= 0; i < N (change_line_pos); i++) {
    if (change_line_pos[i] >= start && change_line_pos[i] < end) {
      if (change_line_pos[i] <= cl_low) cl_low= change_line_pos[i];
      if (change_line_pos[i] >= cl_high) cl_high= change_line_pos[i];
    }
  }
}

void
lang_parser::try_add_barckets_index (string& token_type) {
  if (token_type == "(") {
    small_bracket_depth+= 1;
    brackets_index << small_bracket_depth;
  }
  else if (token_type == ")") {
    brackets_index << small_bracket_depth;
    small_bracket_depth-= 1;
  }
  else if (token_type == "[") {
    mid_bracket_depth+= 1;
    brackets_index << mid_bracket_depth;
  }
  else if (token_type == "]") {
    brackets_index << mid_bracket_depth;
    mid_bracket_depth-= 1;
  }
  else if (token_type == "{") {
    large_bracket_depth+= 1;
    brackets_index << large_bracket_depth;
  }
  else if (token_type == "}") {
    brackets_index << large_bracket_depth;
    large_bracket_depth-= 1;
  }
  else {
    brackets_index << 0;
  }
}

void
lang_parser::add_single_token (string debug_tag, string token_type,
                               string token_literal, int start_pos, int end_pos,
                               int token_lang_pro) {
  if (start_pos == end_pos) {
    return;
  }
  if (token_type == "Space") {
    for (int i= start_pos; i < end_pos; i++) {
      token_starts << i;
      token_ends << i + 1;
      token_types << token_type;
      token_lang_pros << token_lang_pro;
      brackets_index << 0;
      // cout << debug_tag << token_type << ", Code: " << token_literal << " S "
      // << i << " E " << i+1 << "\n";
    }
  }
  else {
    // When a token contains spaces, it should be split into multiple space
    // tokens and normal tokens
    string token_now  = token_literal (0, end_pos - start_pos);
    string token_cache= "";
    int    start      = 0;
    for (int i= 0; i < N (token_now); i++) {
      if (token_now[i] != ' ' && token_now[i] != '\n') {
        token_cache= token_cache * token_now[i];
      }
      else {
        if (i > start) {
          token_starts << start_pos + start;
          token_ends << start_pos + i;
          token_types << token_type;
          token_lang_pros << token_lang_pro;
          try_add_barckets_index (token_type);
          // cout << debug_tag << token_type << ", Code: " << token_cache << " S
          // " << start_pos + start << " E " << start_pos + i << "\n";
        }
        // Add Space
        start      = i + 1;
        token_cache= "";
        token_starts << start_pos + i;
        token_ends << start_pos + start;
        token_types << token_type;
        token_lang_pros << 0;
        brackets_index << 0;
        // cout << debug_tag << "(Inner Space) " << token_type << ", Code: " <<
        // "<space>" << " S " << start_pos + i << " E " << start_pos + start <<
        // "\n";
      }
    }
    if (end_pos > start_pos) {
      token_starts << start_pos;
      token_ends << end_pos;
      token_types << token_type;
      token_lang_pros << token_lang_pro;
      try_add_barckets_index (token_type);
      // cout << debug_tag << token_type << ", Code: " << token_now << " S " <<
      // start_pos << " E " << end_pos << "\n";
    }
  }
}

void
lang_parser::add_token (string token_type, string token_literal, int start_pos,
                        int end_pos, int token_lang_pro) {
  // When a token spans multiple lines, it should be split into one token per
  // line
  int change_line_low = 99999;
  int change_line_high= -1;
  is_change_line_between (start_pos, end_pos, change_line_low,
                          change_line_high);
  if (change_line_high > 0) {
    int start_1= start_pos;
    int end_1  = change_line_low;
    int start_2= change_line_high + 1;
    int end_2  = end_pos;
    if (start_1 < end_1) {
      add_single_token ("(Before Cross)Node type: ", token_type, token_literal,
                        start_1, end_1, token_lang_pro);
    }
    if (end_1 + 1 < start_2) {
      // cout << "Mid Split\n";
      add_token (token_type,
                 token_literal (end_1 - start_1 + 1, N (token_literal)),
                 end_1 + 1, start_2, token_lang_pro);
    }
    if (start_2 < end_2) {
      add_single_token ("(After Cross)Node type: ", token_type,
                        token_literal (start_2 - start_1, N (token_literal)),
                        start_2, end_2, token_lang_pro);
    }
  }
  else {
    add_single_token ("Node type: ", token_type, token_literal, start_pos,
                      end_pos, token_lang_pro);
  }
}

void
lang_parser::do_ast_parse (string_u8 code) {
  fix_pos_moved      = 0;
  last_end_pos       = 0;
  inner_token_index  = 0;
  small_bracket_depth= 0;
  mid_bracket_depth  = 0;
  large_bracket_depth= 0;
  list<TSNode> tsnodes;

  token_starts   = list<int> ();
  token_ends     = list<int> ();
  token_types    = list<string> ();
  token_lang_pros= list<int> ();
  brackets_index = list<int> ();

  // Tree Parse
  // time_t t1= texmacs_time ();//Parse Time Start
  const char* source_code= as_charp (code); // code
  TSTree*     tstree=
      ts_parser_parse_string (ast_parser, NULL, source_code, N (code));
  TSNode root_node= ts_tree_root_node (tstree);
  // time_t t2= texmacs_time ();//Parse Time Start

  // Tree Process
  // time_t t3= texmacs_time ();//Process Time Start
  collect_leaf_nodes (root_node, tsnodes);
  for (int i= 0; i < N (tsnodes); i++) {
    const TSNode& leaf_node = tsnodes[i];
    const char*   node_type = ts_node_type (leaf_node);
    int           start_byte= ts_node_start_byte (leaf_node);
    int           end_byte  = ts_node_end_byte (leaf_node);
    string_u8 code_fragment (source_code + start_byte, end_byte - start_byte);
    string    code_fragment_cork  = utf8_to_cork (code_fragment);
    int       code_fragment_length= N (code_fragment_cork);
    int       real_start_byte     = start_byte + fix_pos_moved;
    fix_pos_moved+= code_fragment_length - (end_byte - start_byte);
    int real_end_byte= real_start_byte + code_fragment_length;

    if (code_fragment_length == 0) continue;

    // Add Front Space
    if (real_start_byte > last_end_pos && last_end_pos >= 0) {
      add_token (string ("Space"), string ("<space>"), last_end_pos,
                 real_start_byte, 0);
    }

    // Store Token Data
    add_token (string (node_type), code_fragment_cork, real_start_byte,
               real_end_byte, 1);
    last_end_pos= real_end_byte;
  }

  // Add End Space
  if (last_end_pos < real_code_len && last_end_pos >= 0) {
    add_token (string ("Space"), string ("<space>"), last_end_pos,
               real_code_len, 0);
  }
  // time_t t4= texmacs_time ();//Process Time Start
  // cout << "Parse took " << t2-t1 << "ms |Process took " << t4-t3 << "ms\n";

  ts_tree_delete (tstree);
}

int
lang_parser::current_token_property () {
  return token_lang_pros[inner_token_index];
}

int
lang_parser::current_token_end () {
  return token_ends[inner_token_index];
}

int
lang_parser::current_brackets_index () {
  return brackets_index[inner_token_index];
}

string
lang_parser::current_token_type () {
  return token_types[inner_token_index];
}

void
lang_parser::next_token () {
  inner_token_index+= 1;
}

int
lang_parser::get_token_num () {
  return N (token_types);
}

int
lang_parser::get_token_index () {
  return inner_token_index;
}

void
lang_parser::set_token_start (int start_index) {
  for (int i= 0; i < N (token_starts); i++) {
    if (start_index <= token_starts[i]) {
      inner_token_index= i;
      break;
    }
  }
}