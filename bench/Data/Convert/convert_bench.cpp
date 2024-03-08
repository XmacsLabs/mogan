/******************************************************************************
 * MODULE     : converter_test.cpp
 * DESCRIPTION: Properties of characters and strings
 * COPYRIGHT  : (C) 2024 jingkaimori
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include <QtTest/QtTest>

#include "base.hpp"
#include "convert.hpp"
#include "file.hpp"
Q_DECLARE_METATYPE (url)

class TestConverter : public QObject {
  Q_OBJECT

private slots:
  void initTestCase () { init_lolly (); }
  void bench_texmacs_to_tree_data ();
  void bench_texmacs_to_tree ();
};

void
TestConverter::bench_texmacs_to_tree_data () {
  QTest::addColumn<url> ("file_name");
  url tm_base ("$TEXMACS_PATH/tests/tm/");
  QTest::newRow ("29_1_1.tm") << tm_base * "29_1_1.tm";
  QTest::newRow ("46_3.tm") << tm_base * "46_3.tm";
  QTest::newRow ("64_1.tm") << tm_base * "64_1.tm";
}
void
TestConverter::bench_texmacs_to_tree () {
  QFETCH (url, file_name);
  string file_content;
  load_string (file_name, file_content, true);
  QBENCHMARK { texmacs_to_tree (file_content); };
}

QTEST_MAIN (TestConverter)
#include "convert_bench.moc"
