
/******************************************************************************
 * MODULE     : xml.hpp
 * DESCRIPTION: various conversion routines on XML
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef XML_HPP
#define XML_HPP

#include "analyze.hpp"
#include "hashmap.hpp"
#include "object.hpp"
#include "parse_string.hpp"
#include "string.hpp"
#include "tree.hpp"

/******************************************************************************
 * The xml/html parser aims to parse a superset of the set of valid documents.
 * In other words, no attempts are made to signal error messages for
 * incorrect documents; in the case of Html we even attempt to correct
 * common mistakes, like badly structured documents. So correct documents
 * should be parsed correctly and incorrect documents are transformed into
 * correct documents in a heuristic way.
 *
 * The parser proceeds in three steps: the first pass does all parsing
 * except for the construction of a tree structure for nested tags.
 * The second stage takes care of the nesting, while heuristically
 * correcting improper nested trees, and while taking care of optional
 * closing tags in the case of Html. The last stage does some final
 * white space and entity cleanup.
 *
 * Present limitations: we do not fully parse <!DOCTYPE ...> constructs yet.
 * Entities which are present in the DOCTYPE definition of the document
 * will be expanded. However, external DTD's are not read. Notice also that
 * it is not yet possible to associate default xml:space attributes to tags.
 ******************************************************************************/

struct xml_html_parser {
  bool                    html;
  parse_string            s;
  hashmap<string, string> entities;
  array<tree>             a;
  int                     i, n;
  tree                    stack;

  xml_html_parser ();
  inline void skip_space () {
    while (s && is_space (s[0]))
      s+= 1;
  }
  inline bool is_name_char (char c) {
    return is_alpha (c) || is_digit (c) || (c == '_') || (c == ':') ||
           (c == '.') || (c == '-') || (((int) ((unsigned char) c)) >= 128);
  }

  string transcode (string s);

  string parse_until (string what);
  string parse_name ();
  string parse_quoted ();
  string expand_entity (string s);
  string expand_entities (string s);
  string parse_entity ();
  tree   parse_attribute ();
  tree   parse_opening ();
  tree   parse_closing ();
  tree   parse_pi ();
  tree   parse_comment ();
  tree   parse_cdata ();
  tree   parse_misc ();
  void   parse ();

  tree parse_system ();
  tree parse_public ();
  tree parse_element ();
  tree parse_attlist ();
  void parse_entity_decl ();
  tree parse_notation ();
  tree parse_doctype ();

  // NOTE: these routines should remain there even if they are not used
  bool   finalize_preserve_space (string tag);
  string finalize_space (string s, bool first, bool last);
  tree   finalize_space (tree t);
  // END NOTE
  bool build_valid_child (string parent, string child);
  bool build_must_close (string tag);
  bool build_can_close (string tag);
  void build (tree& r);

  tree finalize_sxml (tree t);
  tree parse (string s);
};

string old_tm_to_xml_cdata (string s);
object tm_to_xml_cdata (string s);
string old_xml_cdata_to_tm (string s);
string tm_to_xml_name (string s);
string xml_name_to_tm (string s);
string xml_unspace (string s, bool first, bool last);

tree parse_xml (string s);

tree   find_first_element_by_name (tree t, string name);
string get_attr_from_element (tree t, string name, string default_value);
int    parse_xml_length (string length);

#endif
