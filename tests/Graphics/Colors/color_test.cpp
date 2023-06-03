/*****************************************************************************
 * MODULE     : color_test.cpp
 * DESCRIPTION: Tests on color
 * COPYRIGHT  : (C) 2019-2023  zxc
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "colors.hpp"
#include <QtTest/QtTest>

class TestColor : public QObject {
  Q_OBJECT

private slots:
  void test_get_rgb_color ();
  void test_rgb_color();
  void test_cmyk_color ();
};

void 
TestColor::test_rgb_color(){
  array<int> myArray = {255,255,0};
  color c = 0xFFFFFF00;
  QVERIFY(rgb_color(255,255,0) == c);
}

void 
TestColor::test_get_rgb_color(){
  array<int> myArray = {255,255,0,255};
  color c = 0xFFFFFF00;
  array<int> col_array(4);
  get_rgb_color(c, col_array[0], col_array[1], col_array[2], col_array[3]);
  QVERIFY(col_array == myArray);
}

void 
TestColor::test_cmyk_color(){
  color c = 0xFFFFFF00;
  QVERIFY(cmyk_color(0,0,255,0) == c);
}

QTEST_MAIN (TestColor)
#include "color_test.moc"