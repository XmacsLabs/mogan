
/******************************************************************************
 * MODULE     : tm_url_test.cpp
 * COPYRIGHT  : (C) 2024  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "base.hpp"
#include "sys_utils.hpp"
#include "tm_sys_utils.hpp"
#include "tm_url.hpp"
#include "url.hpp"
#include <QtTest/QtTest>

class TestTMURL : public QObject {
  Q_OBJECT

private slots:
  void init () { init_lolly (); }
  void test_complete ();
};

void
TestTMURL::test_complete () {
  // url u= url_system ("/tmp") | url_system ("/usr");
  // cout << complete (u, "dr") << LF;
  if (!os_win ()) {
    QVERIFY (url_system ("/tmp") == complete (url_system ("/tmp"), "dr"));
  }
}

QTEST_MAIN (TestTMURL)
#include "tm_url_test.moc"
