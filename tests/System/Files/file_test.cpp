
/******************************************************************************
* MODULE     : file_test.cpp
* COPYRIGHT  : (C) 2022  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "file.hpp"


class TestFile: public QObject {
  Q_OBJECT

private slots:
  void test_read_directory ();
};

void TestFile::test_read_directory () {
  auto misc= url_system("$TEXMACS_PATH/styles");
  bool err= false;
  auto dirs= read_directory(misc, err);
  QCOMPARE (N(dirs), 17);

  auto no_dirs= read_directory (url ("/not_exist"), err);
  QCOMPARE (N(no_dirs), 0);
}

QTEST_MAIN(TestFile)
#include "file_test.moc"
