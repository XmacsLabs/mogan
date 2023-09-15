
/******************************************************************************
 * MODULE     : url_test.cpp
 * COPYRIGHT  : (C) 2019-2021  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "config.h"
#include "file.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tmfs_url.hpp"

#include <QtTest/QtTest>

class TestURL : public QObject {
  Q_OBJECT

private:
  url root_tmp;
  url root_no_such_tmp;

private slots:

  void init () {
    lolly::init_tbox ();
    if (os_mingw () || os_win()) {
      root_tmp= url ("$TEMP");
    }
    else {
      root_tmp= url ("/tmp");
    }
    root_no_such_tmp= url ("/no_such_tmp");
  }
  void test_exists ();
  void test_suffix ();

  // operations
  void test_descends ();
};

void
TestURL::test_exists () {
  // two cases: root directory
  QVERIFY (exists (root_tmp));
  QVERIFY (!exists (root_no_such_tmp));
}

void
TestURL::test_suffix () {
  // empty suffix should work
  url no_suffix= url ("/a/b/c/d/no_suffix");
  QCOMPARE (suffix (no_suffix), string (""));
  url no_suffix2= url ("/a/b.c/d/no_suffix");
  QCOMPARE (suffix (no_suffix2), string (""));

  // normal suffix should work
  url png= url ("/a/b/c/d.png");
  QCOMPARE (suffix (png), string ("png"));
  url png2= url ("/a/b.c/d.png");
  QCOMPARE (suffix (png2), string ("png"));
}

void
TestURL::test_descends () {
#if defined(OS_MINGW) || defined(OS_WIN)
  QVERIFY (descends (url_system ("$TEMP/a.txt"), root_tmp));
#else
  QVERIFY (descends (url_system ("/tmp/a.txt"), root_tmp));
#endif
  QVERIFY (descends (url_system ("$TEXMACS_PATH/doc/main/man-manual.en.tm"),
                     url_system ("$TEXMACS_PATH")));
  QVERIFY (!descends (root_no_such_tmp, root_tmp));
}

QTEST_MAIN (TestURL)
#include "url_test.moc"
