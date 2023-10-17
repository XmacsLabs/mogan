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

#include "Html/html.hpp"
#include "base.hpp"
#include "convert.hpp"
#include "drd_std.hpp"
#include "sys_utils.hpp"
#include "tm_ostream.hpp"
#include "tree_helper.hpp"

class TestParseHTML : public QObject {
  Q_OBJECT

private slots:
  void init () { init_lolly (); }
  void test_html_p ();
  void test_html_title ();
};

void
TestParseHTML::test_html_p () {
  string a= string ("<p>hello</p><p>hello</p>");
  QVERIFY (parse_html (a) == tuple (tree ("*TOP*"), tuple ("p", "\"hello\""),
                                    tuple ("p", "\"hello\"")));
}

void
TestParseHTML::test_html_title () {
  string a= string ("\
<table style=\"margin-bottom: 2em\" class=\"title-block\">\
  <tr>\
    <td><table style=\"margin-top: 0.5em; margin-bottom: 0.5em\" class=\"title-block\">\
      <tr>\
        <td><font style=\"font-size: 168.2%\"><strong>hello</strong></font></td>\
      </tr>\
    </table></td>\
  </tr>\
</table>");

  tree b= tuple (
      "*TOP*",
      tuple (
          "table",
          tuple ("@", tuple ("style, \"margin-bottom: 2em\""),
                 tuple ("class, \"title-block\"")),
          "\"  \"",
          tuple (
              "tr", "\"    \"",
              tuple (
                  "td",
                  tuple (
                      "table",
                      tuple ("@",
                             tuple ("style, \"margin-top: 0.5em; "
                                    "margin-bottom: 0.5em\""),
                             tuple ("class, \"title-block\"")),
                      "\"      \"",
                      tuple (
                          "tr", "\"        \"",
                          tuple (
                              "td",
                              tuple (
                                  "font",
                                  tuple (
                                      "@",
                                      tuple ("style, \"font-size: 168.2%\"")),
                                  tuple ("strong, \"hello\""))),
                          "\"      \""),
                      "\"    \"")),
              "\"  \"")));

  tree c= parse_html (a);

  cout << "[DEBUG] a: " << a << LF;

  cout << "[DEBUG] b: " << b << LF;

  cout << "[DEBUG] c: " << c << LF;

  tm_ostream b_out, c_out;
  b_out.buffer ();
  c_out.buffer ();
  b_out << b;
  c_out << c;

  QVERIFY (b_out.unbuffer () == c_out.unbuffer ());
}

QTEST_MAIN (TestParseHTML)
#include "parsehtml_test.moc"
