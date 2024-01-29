
/******************************************************************************
 * MODULE     : blanks_parser.hpp
 * DESCRIPTION: shared blanks parsing routines
 * COPYRIGHT  : (C) 2019-2020  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef BLANKS_PARSER_H
#define BLANKS_PARSER_H

#include "parser.hpp"

class blanks_parser_rep : public parser_rep {
public:
  blanks_parser_rep (){};
  string get_parser_name () { return "blanks_parser"; }

  bool can_parse (string s, int pos) {
    return pos < N (s) && (s[pos] == ' ' || s[pos] == '\t');
  }

private:
  void do_parse (string s, int& pos) {
    (void) s;
    pos++;
  }
};

#endif // BLANKS_PARSER_H
