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
#include <moebius/tree_label.hpp>

extern tree the_et;
using moebius::make_tree_label;

lang_parser::lang_parser (string lang) {
  // TODO: Dynamic loading of shared lib and multilingual switching
  ast_parser= ts_parser_new ();
  if (lang == "cpp") ts_lang= tree_sitter_cpp ();
  if (lang == "scheme") ts_lang= tree_sitter_scheme ();
  // cout << lang << " parser created\n";

  ts_parser_set_language (ast_parser, ts_lang);

  tree lang_tree (make_tree_label (lang));
  lang_op= lang_tree->op;

  tree lang_code_tree (make_tree_label (lang * "-code"));
  lang_code_op= lang_code_tree->op;
}

void
lang_parser::reset_brackets_pair () {
  brackets_pairs_amount= 0;
  bracket_symbol_list  = array<TSSymbol> ();
  brackets_depths_cache= array<uint32_t> ();
}

void
lang_parser::add_brackets_pair (string forward, string backward) {
  bracket_symbol_list << ts_language_symbol_for_name (
      ts_lang, c_string (forward), 1, false);
  bracket_symbol_list << ts_language_symbol_for_name (
      ts_lang, c_string (backward), 1, false);
  brackets_depths_cache << 0u;
  brackets_pairs_amount+= 1;
}

bool
lang_parser::check_line_changed (tree t) {
  // hash(t) does not take IP changes into account
  int line_hash= hash (obtain_ip (t)) + hash (t->label) / 2;
  // cout << "t->label: " << t->label << " obtain_ip (t): " << obtain_ip (t)
  // <<"\n";
  if (line_hash != current_line_hash) {
    current_line_hash= line_hash;
    return true;
  }
  return false;
}

tree
lang_parser::get_root_node (tree t, int& start_index, int& hash_code) {
  change_line_pos= array<int> ();
  leaf_tree_nodes= array<tree> ();

  tree root     = t;
  path father_ip= obtain_ip (t);
  // cout << "[Input]Current: " << father_ip << " Self:" << root << "\n";
  while (root->op != lang_op && root->op != lang_code_op && N (father_ip) > 1) {
    // cout << "Father: " << father_ip << " Root:" << root << " the_et " <<
    // the_et << LF;
    father_ip= father_ip->next;
    root     = tree (subtree (the_et, reverse (father_ip)));
  }
  // cout << "[Result]Father: " << father_ip <<
  // " Root: " << root <<
  // " N(the_et):  " << N(the_et) << LF;

  // if(N(the_et) > 1){
  //   cout << the_et << LF;
  // }

  hash_code= hash (root);
  get_data_from_root (root, t, start_index);

  // Temporary Fix
  // Sometimes copying to the image can result in not being able to obtain the
  // root node
  if (N (change_line_pos) == 1 && change_line_pos[0] == 0 && N (t->label) > 0) {
    cout << "Set Root To T\n";
    root= t;
  }
  return root;
}

void
lang_parser::get_data_from_root (tree root, tree line, int& start_index) {
  for (tree child_node : root) {
    if (is_atomic (child_node)) {
      leaf_tree_nodes << child_node;
      int local_start_index= 0;
      if (N (change_line_pos) > 0)
        local_start_index= change_line_pos[N (change_line_pos) - 1] + 1;
      // cout << "Child: " << obtain_ip (child_node)
      //      << " Line: " << obtain_ip (line) << " local_start_index "
      //      << local_start_index << "\n";

      if (obtain_ip (child_node) == obtain_ip (line)) {
        start_index= local_start_index;
      }

      int change_index= N (child_node->label) + local_start_index;
      // cout << "Line Change: " << change_index << "\n";
      change_line_pos << change_index;
    }
    else {
      get_data_from_root (child_node, line, start_index);
    }
  }
}

string_u8
lang_parser::get_code_str (tree root) {
  string    code_cork= "";
  string_u8 code     = "";
  if (is_atomic (root)) {
    code_cork << root->label;
    code << cork_to_utf8 (root->label);
  }
  else {
    get_code_from_root (root, code_cork, code);
  }

  //<ldots> error fix
  code         = replace (code, "…", "...");
  real_code_len= N (code_cork);
  return code;
}

void
lang_parser::get_code_from_root (tree root, string& code, string_u8& code_u8) {
  for (tree child_node : root) {
    if (is_atomic (child_node)) {
      code << child_node->label;
      code_u8 << cork_to_utf8 (child_node->label);
      if (child_node->label == " " || N (child_node->label) == 0) {
        code << " ";
        code_u8 << " ";
      }
      else {
        code << "\n";
        code_u8 << "\n";
      }
    }
    else {
      get_code_from_root (child_node, code, code_u8);
    }
  }
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
lang_parser::collect_leaf_nodes (TSNode node, array<TSNode>& tsnodes) {
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
  int change_line_pos_N= N (change_line_pos);
  for (int i= 0; i < change_line_pos_N; i++) {
    if (change_line_pos[i] >= start && change_line_pos[i] < end) {
      if (change_line_pos[i] <= cl_low) cl_low= change_line_pos[i];
      if (change_line_pos[i] >= cl_high) cl_high= change_line_pos[i];
    }
  }
}

void
lang_parser::try_add_brackets_index (TSSymbol& token_type) {
  for (int i= 0; i < brackets_pairs_amount; i++) {
    if (token_type == bracket_symbol_list[i << 1]) {
      brackets_depths_cache[i]+= 1;
      brackets_index << brackets_depths_cache[i];
      return;
    }
    if (token_type == bracket_symbol_list[(i << 1) + 1]) {
      brackets_index << brackets_depths_cache[i];
      brackets_depths_cache[i]-= 1;
      return;
    }
  }
  brackets_index << 0u;
}

void
lang_parser::add_single_token (string debug_tag, TSSymbol token_type,
                               string token_literal, int start_pos, int end_pos,
                               int token_lang_pro) {
  if (start_pos == end_pos) {
    return;
  }
  if (token_type == SpaceSymbol) {
    for (int i= start_pos; i < end_pos; i++) {
      token_starts << i;
      token_ends << i + 1;
      token_types << token_type;
      token_lang_pros << token_lang_pro;
      brackets_index << 0u;
      // cout << debug_tag << token_type << ", Code: " << token_literal << " S "
      //      << i << " E " << i + 1 << "\n";
    }
  }
  else {
    // When a token contains spaces, it should be split into multiple space
    // tokens and normal tokens
    string token_now  = token_literal (0, end_pos - start_pos);
    string token_cache= "";
    int    start      = 0;
    int    token_now_N= N (token_now);
    for (int i= 0; i < token_now_N; i++) {
      if (token_now[i] != ' ' && token_now[i] != '\n') {
        token_cache << token_now[i];
      }
      else {
        if (i > start) {
          token_starts << start_pos + start;
          token_ends << start_pos + i;
          token_types << token_type;
          token_lang_pros << token_lang_pro;
          try_add_brackets_index (token_type);

          // cout << debug_tag << token_type << ", Code: " << token_cache << "S"
          //      << start_pos + start << " E " << start_pos + i << "\n";
        }
        // Add Space
        start      = i + 1;
        token_cache= "";
        token_starts << start_pos + i;
        token_ends << start_pos + start;
        token_types << token_type;
        token_lang_pros << 0;
        brackets_index << 0u;

        // cout << debug_tag << "(Inner Space) " << token_type << ", Code: "
        //      << "<space>"
        //      << " S " << start_pos + i << " E " << start_pos + start << "\n";
      }
    }
    if (end_pos > start_pos) {
      token_starts << start_pos;
      token_ends << end_pos;
      token_types << token_type;
      token_lang_pros << token_lang_pro;
      try_add_brackets_index (token_type);

      // cout << debug_tag << token_type << ", Code: " << token_now << " S "
      //      << start_pos << " E " << end_pos << "\n";
    }
  }
}

void
lang_parser::add_token (TSSymbol token_type, string token_literal,
                        int start_pos, int end_pos, int token_lang_pro) {
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
lang_parser::do_ast_parse (tree code_root) {
  fix_pos_moved    = 0;
  last_end_pos     = 0;
  inner_token_index= 0;
  array<TSNode> tsnodes;

  for (int i= 0; i < brackets_pairs_amount; i++)
    brackets_depths_cache[i]= 0u;
  token_starts   = array<int> ();
  token_ends     = array<int> ();
  token_types    = array<TSSymbol> ();
  token_lang_pros= array<int> ();
  brackets_index = array<uint32_t> ();

  // Tree Parse
  // time_t t1  = texmacs_time (); // Parse Time Start
  code_string= get_code_str (code_root);
  c_string source_code (code_string);
  TSTree*  tstree=
      ts_parser_parse_string (ast_parser, NULL, source_code, N (code_string));
  TSNode root_node= ts_tree_root_node (tstree);
  // time_t t2       = texmacs_time (); // Parse Time End / Process Time Start

  // Tree Process
  collect_leaf_nodes (root_node, tsnodes);
  int tsnodes_len= N (tsnodes);
  for (int i= 0; i < tsnodes_len; i++) {
    const TSNode& leaf_node  = tsnodes[i];
    TSSymbol      node_symbol= ts_node_symbol (leaf_node);
    int           start_byte = ts_node_start_byte (leaf_node);
    int           end_byte   = ts_node_end_byte (leaf_node);
    string_u8 code_fragment (source_code + start_byte, end_byte - start_byte);
    string    code_fragment_cork  = utf8_to_cork (code_fragment);
    int       code_fragment_length= N (code_fragment_cork);
    int       real_start_byte     = start_byte + fix_pos_moved;
    fix_pos_moved+= code_fragment_length - (end_byte - start_byte);
    int real_end_byte= real_start_byte + code_fragment_length;

    if (code_fragment_length == 0) continue;

    // Add Front Space
    if (real_start_byte > last_end_pos && last_end_pos >= 0) {
      add_token (SpaceSymbol, string (""), last_end_pos, real_start_byte, 0);
    }

    // Store Token Data
    add_token (node_symbol, code_fragment_cork, real_start_byte, real_end_byte,
               1);
    last_end_pos= real_end_byte;
  }

  // Add End Space
  if (last_end_pos < real_code_len && last_end_pos >= 0) {
    add_token (SpaceSymbol, string (""), last_end_pos, real_code_len, 0);
  }
  // time_t t3= texmacs_time (); // Process Time End
  // cout << "Code Gen and Parse took " << t2 - t1 << "ms |Process took "
  //      << t3 - t2 << "ms\n";

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

uint32_t
lang_parser::current_brackets_index () {
  return brackets_index[inner_token_index];
}

string
lang_parser::current_token_type () {
  TSSymbol symbol= token_types[inner_token_index];
  if (symbol == SpaceSymbol) return "Space";
  return ts_language_symbol_name (ts_lang, symbol);
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
  int token_num= get_token_num ();
  for (int i= 0; i < token_num; i++) {
    if (start_index <= token_starts[i]) {
      inner_token_index= i;
      break;
    }
  }
}
