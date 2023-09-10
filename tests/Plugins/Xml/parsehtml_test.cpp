/******************************************************************************
 * MODULE     : parsehtml_test.cpp
 * DESCRIPTION: parse html
 * COPYRIGHT  : (C) 2023 charonxin
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include <QtTest/QtTest>

#include "Xml/xml.hpp"
#include "convert.hpp"
#include "drd_std.hpp"
#include "sys_utils.hpp"
#include "tree_helper.hpp"

class TestParseHTML : public QObject {
  Q_OBJECT

private slots:
  void init () { lolly::init_tbox (); }
  void test_parse_html ();
};

void
TestParseHTML::test_parse_html () {
   string a = string("<p>hello</p><p>hello</p>");
  // print_tree (parse_html (a));
//   245 (*TOP*, 245 (p, "hello"), 245 (p, "hello"))
  QVERIFY (parse_html (a) == tuple (tree ("*TOP*"), tuple ("p","\"hello\""), tuple ("p","\"hello\""))); 

}


QTEST_MAIN (TestParseHTML)
#include "parsehtml_test.moc"