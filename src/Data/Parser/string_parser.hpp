
/******************************************************************************
 * MODULE     : string_parser.hpp
 * DESCRIPTION: shared string parsing routines for various programming languages
 * COPYRIGHT  : (C) 2020  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef STRING_PARSER_H
#define STRING_PARSER_H

#include "escaped_char_parser.hpp"
#include "hashmap.hpp"
#include "iterator.hpp"
#include "parser.hpp"

class string_parser_rep : public parser_rep {
public:
  string_parser_rep ();
  bool   can_parse (string s, int pos);
  string get_parser_name () { return "string_parser"; }
  string to_string ();

  void set_pairs (hashmap<string, string> p_pairs);

  bool unfinished ();
  void reset ();

  bool escaped ();
  void set_escaped_char_parser (escaped_char_parser_rep p_esc_parser);
  bool parse_escaped (string s, int& pos);
  void skip_escaped (bool skip);

private:
  /**
   * A hashmap consists of (start, end)
   */
  hashmap<string, string> m_pairs;

  /**
   * NOTE: If m_start_size is 0, we are not in a string and m_start is
   * meaningless.
   */
  string m_start;

  /**
   * By default, it is 0.
   * In the string, m_start_size is the size of m_start.
   * Out of the string, m_start_size is reset to 0.
   */
  int  m_start_size;
  bool m_finished;
  void do_finish ();

  escaped_char_parser_rep m_esc_parser;
  bool                    use_esc_parser;
  bool                    m_escaped;
  bool                    m_skip_escaped;

  void do_parse (string s, int& pos);
};

#endif
