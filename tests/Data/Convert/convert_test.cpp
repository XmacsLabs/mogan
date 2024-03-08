/******************************************************************************
 * MODULE     : converter_test.cpp
 * DESCRIPTION: Properties of characters and strings
 * COPYRIGHT  : (C) 2023 jingkaimori
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include <QtTest/QtTest>

#include "base.hpp"
#include "convert.hpp"
#include "tree_helper.hpp"
Q_DECLARE_METATYPE (tree)
Q_DECLARE_METATYPE (string)

class TestConverter : public QObject {
  Q_OBJECT

private slots:
  void initTestCase () { init_lolly (); }
  void test_search_metadata_data ();
  void test_search_metadata ();
  void test_texmacs_to_tree_data ();
  void test_texmacs_to_tree ();
};

void
TestConverter::test_search_metadata_data () {
  QTest::addColumn<tree> ("input_tree");
  QTest::addColumn<string> ("title");
  QTest::addColumn<string> ("author");
  QTest::addColumn<string> ("keyword");
  QTest::addColumn<string> ("invalid");

  string empty ("");
  QTest::newRow ("regular document")
      << tree (DOCUMENT,
               compound (
                   "doc-data", compound ("doc-title", "Test of Metadata"),
                   compound ("doc-author",
                             compound ("author-data",
                                       compound ("author-name", "author1"))),
                   compound ("doc-author",
                             compound ("author-data",
                                       compound ("author-name", "author2"))),
                   compound ("doc-author",
                             compound ("author-data",
                                       compound ("author-name", "author3")))),
               compound ("abstract-data",
                         compound ("abstract-keywords", "keyword 1",
                                   "keyword 2", "keyword 3")))
      << string ("Test of Metadata") << string ("author1, author2, author3")
      << string ("keyword 1, keyword 2, keyword 3") << empty;
  QTest::newRow ("texmacs document")
      << tree (DOCUMENT, compound ("tmdoc-title", "Test of manual title"))
      << string ("Test of manual title") << empty << empty << empty;
}

void
TestConverter::test_search_metadata () {
  QFETCH (tree, input_tree);
  QFETCH (string, title);
  QFETCH (string, author);
  QFETCH (string, keyword);
  QFETCH (string, invalid);
  qcompare (search_metadata (input_tree, "title"), title);
  qcompare (search_metadata (input_tree, "author"), author);
  qcompare (search_metadata (input_tree, "keyword"), keyword);
  qcompare (search_metadata (input_tree, "invalid"), invalid);
}

void
TestConverter::test_texmacs_to_tree_data () {
  QTest::addColumn<string> ("input_string");
  QTest::addColumn<tree> ("output");

  QTest::newRow ("pure string") << string ("text") << tree (DOCUMENT, "text");
  QTest::newRow ("escaped string")
      << string ("tex\\\\t") << tree (DOCUMENT, "tex\\t");
  QTest::newRow ("non escaped trailing slash")
      << string ("text\\") << tree (DOCUMENT, "text\\");
}
void
TestConverter::test_texmacs_to_tree () {
  QFETCH (string, input_string);
  QFETCH (tree, output);
  QCOMPARE (texmacs_to_tree (input_string), output);
}

QTEST_MAIN (TestConverter)
#include "convert_test.moc"
