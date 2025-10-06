/** \file block_test.cpp
 *  \copyright GPLv3
 *  \details Unitests for scheme parser
 *  \author jingkaimori
 *  \date   2024
 */

#include "file.hpp"
#include "moe_doctests.hpp"
#include "string.hpp"
#include "tm_ostream.hpp"
#include "tree_helper.hpp"
#include <moebius/data/scheme.hpp>

using namespace moebius;

using moebius::data::block_to_scheme_tree;
using moebius::data::scheme_tree_to_block;
using moebius::data::scheme_tree_to_string;
using moebius::data::scm_quote;
using moebius::data::scm_unquote;
using moebius::data::string_to_scheme_tree;

TEST_CASE ("test scm quote") {
  CHECK_EQ (scm_quote ("a") == "\"a\"", true);
  CHECK_EQ (scm_quote ("") == "\"\"", true);
  CHECK_EQ (scm_quote ("\\") == "\"\\\\\"", true);
}

TEST_CASE ("test_scm_unquote") {
  CHECK_EQ (scm_unquote ("\"\"") == "", true);
  CHECK_EQ (scm_unquote ("\"abc\"") == "abc", true);
  CHECK_EQ (scm_unquote ("abc") == "abc", true);
  CHECK_EQ (scm_unquote ("") == "", true);
  CHECK_EQ (scm_unquote ("\"\\\\\"") == "\\", true);
}

TEST_CASE ("block_to_scheme_tree") {
  CHECK_EQ (block_to_scheme_tree ("(scheme parser)"),
            tree (TUPLE, tree (TUPLE, "scheme", "parser")));
  CHECK_EQ (block_to_scheme_tree ("(scheme 'parser)"),
            tree (TUPLE, tree (TUPLE, "scheme", tree (TUPLE, "'", "parser"))));
  CHECK_EQ (
      block_to_scheme_tree (
          "(scheme \"slashed \\r\\\\ characters \\n\\t\" \"\\0\")"),
      tree (TUPLE, tree (TUPLE, "scheme", "\"slashed r\\ characters \n\t\"",
                         string ("\"\0\"", 3))));
  CHECK_EQ (block_to_scheme_tree ("scheme parser"),
            tree (TUPLE, "scheme", "parser"));
  CHECK_EQ (block_to_scheme_tree ("(scheme (parser combinator))"),
            tree (TUPLE, tree (TUPLE, "scheme",
                               tree (TUPLE, "parser", "combinator"))));
  CHECK_EQ (block_to_scheme_tree ("(scheme (parser combinator))\n"
                                  "; some comment containing \"'\\;#\n"
                                  "(common-lisp gene_rator)"),
            tree (TUPLE,
                  tree (TUPLE, "scheme", tree (TUPLE, "parser", "combinator")),
                  tree (TUPLE, "common-lisp", "gene_rator")));
  CHECK_EQ (
      block_to_scheme_tree ("(chinese (utf-8 encoding) (汉语 条目))"),
      tree (TUPLE, tree (TUPLE, "chinese", tree (TUPLE, "utf-8", "encoding"),
                         tree (TUPLE, "汉语", "条目"))));
  CHECK_EQ (
      block_to_scheme_tree (
          "(empty-characters \t; an inline comment containing \"'\\;#\n\n"
          "  (entry))"),
      tree (TUPLE, tree (TUPLE, "empty-characters", tree (TUPLE, "entry"))));
}

TEST_CASE ("string_to_scheme_tree") {
  CHECK_EQ (string_to_scheme_tree ("(scheme parser)"),
            tree (TUPLE, "scheme", "parser"));
  CHECK_EQ (string_to_scheme_tree ("(scheme 'parser)"),
            tree (TUPLE, "scheme", tree (TUPLE, "'", "parser")));
  CHECK_EQ (string_to_scheme_tree (
                "(scheme \"slashed \\r\\\\ characters \\n\\t\" \"\\0\")"),
            tree (TUPLE, "scheme", "\"slashed r\\ characters \n\t\"",
                  string ("\"\0\"", 3)));
  CHECK_EQ (string_to_scheme_tree ("scheme parser"), tree ("scheme"));
  CHECK_EQ (string_to_scheme_tree ("(scheme (parser combinator))"),
            tree (TUPLE, "scheme", tree (TUPLE, "parser", "combinator")));
  CHECK_EQ (string_to_scheme_tree ("(chinese (utf-8 encoding) (汉语 条目))"),
            tree (TUPLE, "chinese", tree (TUPLE, "utf-8", "encoding"),
                  tree (TUPLE, "汉语", "条目")));
  CHECK_EQ (string_to_scheme_tree (
                "(empty-characters \t; an inline comment containing \"'\\;#\n\n"
                "  (entry))"),
            tree (TUPLE, "empty-characters", tree (TUPLE, "entry")));
}

TEST_CASE ("scheme_tree_to_string") {
  string_eq ("(scheme parser)",
             scheme_tree_to_string (tree (TUPLE, "scheme", "parser")));
  string_eq ("(scheme 'parser)",
             scheme_tree_to_string (
                 tree (TUPLE, "scheme", tree (TUPLE, "'", "parser"))));
  string_eq (string ("(scheme \"slashed \\\\ characters \n\t\" \"\0\")", 39),
             scheme_tree_to_string (tree (TUPLE, "scheme",
                                          "\"slashed \\ characters \n\t\"",
                                          string ("\"\0\"", 3))));
  string_eq ("scheme", scheme_tree_to_string (tree ("scheme")));
  string_eq ("(scheme (parser combinator))",
             scheme_tree_to_string (
                 tree (TUPLE, "scheme", tree (TUPLE, "parser", "combinator"))));
  string_eq ("(chinese (utf-8 encoding) (汉语 条目))",
             scheme_tree_to_string (tree (TUPLE, "chinese",
                                          tree (TUPLE, "utf-8", "encoding"),
                                          tree (TUPLE, "汉语", "条目"))));
}

TEST_CASE ("scheme_tree_to_block") {
  string_eq (
      "(scheme parser)\n",
      scheme_tree_to_block (tree (TUPLE, tree (TUPLE, "scheme", "parser"))));
  string_eq ("(scheme 'parser)\n",
             scheme_tree_to_block (tree (
                 TUPLE, tree (TUPLE, "scheme", tree (TUPLE, "'", "parser")))));
  string_eq (string ("(scheme \"slashed \\\\ characters \n\t\" \"\0\")\n", 40),
             scheme_tree_to_block (tree (
                 TUPLE, tree (TUPLE, "scheme", "\"slashed \\ characters \n\t\"",
                              string ("\"\0\"", 3)))));
  string_eq ("scheme\nparser\n", scheme_tree_to_block (tree (
                                     TUPLE, tree ("scheme"), tree ("parser"))));
  string_eq ("(scheme (parser combinator))\n",
             scheme_tree_to_block (
                 tree (TUPLE, tree (TUPLE, "scheme",
                                    tree (TUPLE, "parser", "combinator")))));
  string_eq (
      "(chinese (utf-8 encoding) (汉语 条目))\n",
      scheme_tree_to_block (tree (
          TUPLE, tree (TUPLE, "chinese", tree (TUPLE, "utf-8", "encoding"),
                       tree (TUPLE, "汉语", "条目")))));
  string_eq (
      "(scheme (parser combinator))\n"
      "(common-lisp gene_rator)\n",
      scheme_tree_to_block (tree (
          TUPLE, tree (TUPLE, "scheme", tree (TUPLE, "parser", "combinator")),
          tree (TUPLE, "common-lisp", "gene_rator"))));
}
