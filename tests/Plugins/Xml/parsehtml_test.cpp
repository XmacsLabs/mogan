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
    string a =string("<table style=\"margin-bottom: 2em\" class=\"title-block\"><tr><td><table style=\"margin-top: 0.5em; margin-bottom: 0.5em\" class=\"title-block\"><tr><td><font style=\"font-size: 168.2%\"><strong>hello</strong></font></td></tr></table></td></tr></table><p></p>");
    tree b = tree("*TOP*", 245 ,tuple("table", 245 ,tuple("@", 245 ,tuple("style", "margin-bottom: 2em"), 245 ,tuple("class", "title-block")), "
  ", 245 ,tree("tr", "
    ", 245 ,tree("td", 245 ,tuple("table", 245 ,tuple("@", 245 ,tuple("style", "margin-top: 0.5em; margin-bottom: 0.5em"), 245 ,tuple(class, "title-block")), "
      ", 245 ,tree("tr", "
        ", 245 ,tree("td", 245 ,tuple("font", 245 ,tuple("@", 245 tuple("style", "font-size: 168.2%")), 245 ,tuple("strong", "hello"))), "
      "), "
    ")), "
  "), "
"), 245 ,tree("p", "

"));
    //std::cout << "parse html: "<< a << LF; 
    cout << "parse html: "<< a << LF;
    QVERIFY (a,b);

}


QTEST_MAIN (TestParseHTML)
#include "parsehtml_test.moc"