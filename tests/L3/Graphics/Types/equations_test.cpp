/*****************************************************************************
 * MODULE     : equations_test.cpp
 * DESCRIPTION: Tests on equations
 * COPYRIGHT  : (C) 2024 Zhenjun Guo
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "equations.hpp"
#include "tm_ostream.hpp"
#include <QtTest/QtTest>

class TestEquations : public QObject {
  Q_OBJECT

private slots:
  void test_tridiag_solve ();
  void test_quasitridiag_solve ();
  void test_xtridiag_solve ();
};

/*
         A       *  x   =   y
  ┌ 1  8        ┐ ┌ 1 ┐   ┌ 9  ┐
  | 5  2  9     |*| 1 | = | 16 |
  |    6  3  10 | | 1 |   | 19 |
  └       7  3  ┘ └ 1 ┘   └ 11 ┘
*/
void
TestEquations::test_tridiag_solve () {
  const int     n= 4;
  array<double> a= {0, 5, 6, 7}, b= {1, 2, 3, 4}, c= {8, 9, 10, 0};
  array<point>  x (n), y (n), expected_x (n);

  int i= 0;
  for (const int& value : {9, 16, 19, 11})
    y[i++]= as_point (value);

  for (i= 0; i < n; ++i)
    expected_x[i]= as_point (1);

  tridiag_solve (a, b, c, x, y, n);
  for (i= 0; i < n; ++i)
    QVERIFY (x[i] == expected_x[i]);
}

void
TestEquations::test_quasitridiag_solve () {
  const int     n= 4;
  array<double> a= {0, 5, 6, 7}, b= {1, 2, 3, 4}, c= {8, 9, 10, 0},
                u= {1, 2, 3, 4}, v= {5, 6, 7, 8};
  array<point> x (n), y (n), expected_x (n);

  int i= 0;
  for (const int& value : {35, 68, 97, 115})
    y[i++]= as_point (value);

  for (i= 0; i < n; ++i)
    expected_x[i]= as_point (1);

  quasitridiag_solve (a, b, c, u, v, x, y, n);
  for (i= 0; i < n; ++i)
    QVERIFY (x[i] == expected_x[i]);
}

void
TestEquations::test_xtridiag_solve () {
  const int     n= 4;
  array<double> a= {0, 5, 6, 7}, b= {1, 2, 3, 4}, c= {8, 9, 10, 0};
  double        a0= 0, a1= 1;
  array<point>  x (n), y (n), expected_x (n);

  int i= 0;
  for (const int& value : {10, 16, 19, 11})
    y[i++]= as_point (value);

  for (i= 0; i < n; ++i)
    expected_x[i]= as_point (1);

  xtridiag_solve (a, b, c, a0, a1, x, y, n);
  for (i= 0; i < n; ++i)
    QVERIFY (x[i] == expected_x[i]);
}

QTEST_MAIN (TestEquations)
#include "equations_test.moc"