/******************************************************************************
* MODULE     : converter_test.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 2019 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>

#include "converter.hpp"
#include "base.hpp"
#include "Qt/qt_utilities.hpp"

class TestConverter: public QObject {
  Q_OBJECT

private slots:
  void test_utf8_to_cork();
};

void TestConverter::test_utf8_to_cork() {
  qcompare (utf8_to_cork ("中"), "<#4E2D>");
  qcompare (utf8_to_cork ("“"), "\x10");
  qcompare (utf8_to_cork("”"), "\x11");

  qDebug() << "test_utf8_to_cork" << "\n";

  auto full_width= list<string>()
    * string(u8"。") * string(u8"，") * string(u8"：") * string(u8"；")
    * string(u8"！") * string(u8"？") * string(u8"、") * string(u8"～")
    * string(u8"”") * string(u8"‘") * string(u8"』") * string(u8"」")
    * string(u8"）") * string(u8"】") * string(u8"》") * string(u8"〉")
    * string(u8"—") * string(u8"·");
  for (int i=0; i<N(full_width); i++) {
    qDebug() << utf8_to_qstring(utf8_to_cork(full_width[i])) << "\n";
  }
}

QTEST_MAIN(TestConverter)
#include "converter_test.moc"
