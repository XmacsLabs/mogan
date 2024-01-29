
/******************************************************************************
 * MODULE     : identifier_parser.hpp
 * DESCRIPTION: shared identifier parsing routines
 * COPYRIGHT  : (C) 2020  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef IDENTIFIER_PARSER_H
#define IDENTIFIER_PARSER_H

#include "array.hpp"
#include "parser.hpp"

class identifier_parser_rep : public parser_rep {
public:
  identifier_parser_rep ();

  string get_parser_name () { return "identifier_parser"; }

  bool can_parse (string s, int pos);

  void set_start_chars (array<char> p_chars);
  void set_extra_chars (array<char> p_chars);

private:
  bool        start_with_alpha;
  array<char> start_chars;
  array<char> extra_chars;
  void        do_parse (string s, int& pos);
};

#endif
