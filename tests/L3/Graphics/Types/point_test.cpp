/******************************************************************************
 * MODULE     : point_test.cpp
 * DESCRIPTION: Add tests for point.cpp.
 * COPYRIGHT  : (C) 2023  charonxin
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "point.hpp"
#include <QtTest/QtTest>

class TestPoint : public QObject {
  Q_OBJECT

private slots:
  void test_operator ();
  void test_inner ();
  void test_norm ();
};

void
TestPoint::test_operator () {
  QCOMPARE (point (2, 3) + point (2, 4) == point (4, 7), true);
  QCOMPARE (point (4, 5) - point (2, 3) == point (2, 2), true);
  QCOMPARE (point (2, 3) * 2.0 == point (4, 6), true);
  QCOMPARE (point (4, 6) / point (2, 3) == point (2, 2), true);
  QCOMPARE (point (4, 6) / 2 == point (2, 3), true);

  QCOMPARE (abs (point (-2, 3)) == point (2, 3), true);
}

/**
 * function inner calculate the inner product of vectors
 * function slanted move the [x] of the point by p[1] * slant
 * function norm return euclidean distance of a point from the [0,0]
 */
void
TestPoint::test_inner () {
  QCOMPARE (inner (point (3, 4), point (2, 5)) == 26.0, true);
  QCOMPARE (slanted (point (10, 20), 1.5) == point (40, 20), true);
  QCOMPARE (norm (point (3, 4)) == 5.0, true);
}

/**
 * function arg computes the polar angle of a point p with respect to the origin
 * function proj calculates the projection point of a point to a line
 * function dist return the distance of a point to a line, but failed to test
 */
void
TestPoint::test_norm () {
  QVERIFY (norm (point (4, 8)) - norm (point (-4, -8)) < 1e-6);
  double tm_PI= 3.1415926535897932384626433832795029L;
  QVERIFY (arg (point (3, 4)) - (2 * tm_PI - arg (point (3, -4))) < 1e-6);
}

QTEST_MAIN (TestPoint)
#include "point_test.moc"
