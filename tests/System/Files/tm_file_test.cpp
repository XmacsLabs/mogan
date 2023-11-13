
/******************************************************************************
 * MODULE     : tm_file_test.cpp
 * COPYRIGHT  : (C) 2019  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "base.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "url.hpp"
#include <QtTest/QtTest>

class TestTMFile : public QObject {
  Q_OBJECT

private slots:
  void init () { init_lolly (); }
  void test_load_ramdisc ();
};

void
TestTMFile::test_load_ramdisc () {
  url u1= url_ramdisc ("content") * url ("sample_file.txt");
  qcompare (tm_string_load (u1), "content");
}

QTEST_MAIN (TestTMFile)
#include "tm_file_test.moc"
