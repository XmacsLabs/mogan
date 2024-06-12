
/******************************************************************************
* MODULE     : xml.hpp
* DESCRIPTION: various conversion routines on XML/HTML/MathML
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef XML_HPP
#define XML_HPP

#include "string.hpp"
#include "tree.hpp"
#include "object.hpp"

string old_tm_to_xml_cdata (string s);
object tm_to_xml_cdata (string s);
string old_xml_cdata_to_tm (string s);
string tm_to_xml_name (string s);
string xml_name_to_tm (string s);
string xml_unspace (string s, bool first, bool last);

tree   parse_xml (string s);
tree   parse_plain_html (string s);
tree   parse_html (string s);
tree   clean_html (tree t);
tree   tmml_upgrade (scheme_tree t);
tree   upgrade_mathml (tree t);
tree   retrieve_mathjax (int id);

tree   find_first_element_by_name (tree t, string name);
string get_attr_from_element (tree t, string name, string default_value);
int    parse_xml_length (string length);

/*** Post corrections ***/
bool   seems_buggy_html_paste (string s);
string correct_buggy_html_paste (string s);
bool   seems_buggy_paste (string s);
string correct_buggy_paste (string s);
tree   default_with_simplify (tree t);

#endif
