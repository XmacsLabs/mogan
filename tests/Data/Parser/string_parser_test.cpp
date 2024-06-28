
/******************************************************************************
 * MODULE     : string_parser_test.cpp
 * DESCRIPTION: Properties of Keyword Parser
 * COPYRIGHT  : (C) 2024  UnbSky
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "analyze.hpp"
#include "iterator.hpp"
#include "string_parser.hpp"

#include "converter.hpp"
#include <QtTest/QtTest>

class TestStringParser : public QObject {
  Q_OBJECT

private slots:
  void test_can_parse ();
  void test_parse ();
};

void
TestStringParser::test_can_parse () {
  string_parser_rep       string_parser= string_parser_rep ();
  int                     pos;
  hashmap<string, string> pairs;
  pairs ("\"")= "\"";
  pairs ("\'")= "\'";
  string_parser.set_pairs (pairs);

  pos= 4;
  QVERIFY (string_parser.can_parse ("a = \"string\";", pos));
}

void
TestStringParser::test_parse () {
  string_parser_rep       string_parser= string_parser_rep ();
  int                     pos;
  hashmap<string, string> pairs;
  pairs ("\"")= "\"";
  pairs ("\'")= "\'";
  string_parser.set_pairs (pairs);

  pos= 4;
  string_parser.parse ("a = \"string\";", pos);
  QCOMPARE (pos, 12);

  // The string parser cannot correctly handle quotation marks in strings
  // pos = 4;
  // string_parser.parse ("a = \"stri\\\"ng\";", pos);
  // QCOMPARE (pos, 14);
}

QTEST_MAIN (TestStringParser)
#include "string_parser_test.moc"
