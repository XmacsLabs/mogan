/******************************************************************************
 * MODULE     : lang_parser.hpp
 * DESCRIPTION: Parser for Programming languages
 * COPYRIGHT  : (C) 2024 UnbSky
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef LANG_PARSER_H
#define LANG_PARSER_H

#include "array.hpp"
#include "hashset.hpp"
#include "tree.hpp"
#include <tree-sitter-cpp.h>
#include <tree-sitter-scheme.h>
#include <tree_sitter/api.h>
constexpr TSSymbol SpaceSymbol= 65534;

class lang_parser {
public:
  lang_parser (string lang, string lang_id);

  /**
   * @brief Check current line of code has changed or not.
   *
   * @return True if current line of code has changed, false otherwise
   */
  bool check_line_changed (tree t);

  /**
   * @brief Check if the code needs to be compiled based on its hash value.
   *
   * @param t The tree node
   * @param start_index The starting position of the node's code in the entire
   * code segment
   * @param hash_code The hash value of the code string
   * @return The root node of code to be parsed
   */
  tree get_root_node (tree t, int& start_index, int& hash_code);

  /**
   * @brief Check if the code needs to be compiled based on its hash value.
   *
   * @param code_hash The hash code of the code root node
   * @return True if the code needs to be compiled, false otherwise
   */
  bool check_to_compile (int code_hash);

  /**
   * @brief Perform AST parsing and processing on the given code string.
   *
   * @param code_root The root node of code to be parsed
   */
  void do_ast_parse (tree code_root);

  /**
   * @brief Set the starting position (inner_token_index) of the token based on
   * the starting position of the node's code in the entire code segment.
   *
   * @param start_index The starting position of the node's code in the entire
   * code segment
   */
  void set_token_start (int start_index);

  // Reset the brackets pair match data
  void reset_brackets_pair ();

  // Add brackets pair to the list of brackets pairs
  void add_brackets_pair (string forward, string backward);

  // Get the property of the current token
  int      current_token_property ();
  int      current_token_end ();
  uint32_t current_brackets_index ();
  string   current_token_type ();

  // Get the total number of tokens
  int get_token_num ();

  // Get the index of the current token (inner_token_index)
  int get_token_index ();

  // Move to the next token (inner_token_index++)
  void next_token ();

  string_u8 code_string;

private:
  TSParser*         ast_parser;
  const TSLanguage* ts_lang;
  int               current_code_hash= 0;
  int               current_line_hash= 0;

  int real_code_len    = 0;
  int fix_pos_moved    = 0;
  int last_end_pos     = -1;
  int inner_token_index= 0;

  array<TSSymbol> bracket_symbol_list;
  array<uint32_t> brackets_depths_cache;
  int             brackets_pairs_amount= 0;

  array<tree> leaf_tree_nodes;

  array<int>      change_line_pos;
  array<int>      token_starts;
  array<int>      token_ends;
  array<TSSymbol> token_types;
  array<int>      token_lang_pros;

  array<uint32_t> brackets_index;

  void      get_code_from_root (tree root, string& code, string_u8& code_u8);
  string_u8 get_code_str (tree root);
  void      collect_leaf_nodes (TSNode node, array<TSNode>& tsnodes);
  void      get_data_from_root (tree root, tree line, int& start_index);
  void is_change_line_between (int start, int end, int& cl_low, int& cl_high);
  void try_add_brackets_index (TSSymbol& token_type);
  void add_token (TSSymbol token_type, string token_literal, int start_pos,
                  int end_pos, int token_lang_pro);
  void add_single_token (string debug_tag, TSSymbol token_type,
                         string token_literal, int start_pos, int end_pos,
                         int token_lang_pro);
};

#endif // defined LANG_PARSER_H
