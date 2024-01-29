
/******************************************************************************
 * MODULE     : packrat.hpp
 * DESCRIPTION: efficient packrat parsing
 * COPYRIGHT  : (C) 2010  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef PACKRAT_H
#define PACKRAT_H
#include "scheme.hpp"
#include "tree.hpp"

void packrat_define (string lan, string s, tree t);
void packrat_property (string lan, string s, string var, string val);
void packrat_inherit (string lan, string from);
int  packrat_abbreviation (string lan, string s);

path   packrat_parse (string lan, string s, tree in);
bool   packrat_correct (string lan, string s, tree in);
bool   packrat_available_path (string lan, tree in, path in_p);
object packrat_context (string lan, string s, tree in, path in_pos);
bool   packrat_select (string lan, string s, tree in, path in_pos, path& p1,
                       path& p2, int mode);

void packrat_highlight (string lan, string s, tree t);
// array<int> packrat_colors (string lan, string s, tree t);

#endif // PACKRAT_H
