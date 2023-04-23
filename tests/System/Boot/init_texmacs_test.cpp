
/******************************************************************************
* MODULE     : nit_texmacs_test.cpp
* COPYRIGHT  : (C) 2023  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boot.hpp"
#include "url.hpp"

#include <QtTest/QtTest>

class TestInitTeXmacs: public QObject {
  Q_OBJECT

private slots:
  void test_cache_path ();
};

void TestInitTeXmacs::test_cache_path () {
  url cache_url= cache_path ();
#ifdef OS_GNU_LINUX
  QVERIFY (as_system_string (cache_url) == as_system_string (url_system ("$HOME/.cache/app.mogan.editor")));
#endif
}

QTEST_MAIN(TestInitTeXmacs)
#include "init_texmacs_test.moc"
