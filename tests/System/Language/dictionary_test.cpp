
/******************************************************************************
 * MODULE     : dictionary_test.cpp
 * COPYRIGHT  : (C) 2024  jingkaimori
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "base.hpp"
#include "converter.hpp"
#include "dictionary.hpp"
#include "tree_helper.hpp"
#include <QtTest/QtTest>

class TestDictionary : public QObject {
  Q_OBJECT

private slots:
  void init () { init_lolly (); }
  void test_string_translate ();
};

void
TestDictionary::test_string_translate () {
  qcompare (translate (string ("yes"), "english", "chinese"),
            utf8_to_cork ("是"));
  qcompare (translate (string ("!yes??"), "english", "chinese"),
            utf8_to_cork ("!是??"));

  set_output_language ("chinese");
  qcompare (get_output_language (), "chinese");
  qcompare (translate ("yes"), utf8_to_cork ("是"));
  qcompare (translate (string ("yes")), utf8_to_cork ("是"));

  qcompare (translate (string ("!yes??")), utf8_to_cork ("!是??"));
  qcompare (translate_as_is (string ("!yes??")), utf8_to_cork ("!yes??"));
  qcompare (translate (string ("yes::no_such_disambiguation")),
            utf8_to_cork ("是"));
  qcompare (translate_as_is (string ("yes::no_such_disambiguation")),
            utf8_to_cork ("是"));
  qcompare (translate (string ("block")), utf8_to_cork ("有框表格"));
  qcompare (translate (string ("block::version")), utf8_to_cork ("段落"));
}

QTEST_MAIN (TestDictionary)
#include "dictionary_test.moc"
