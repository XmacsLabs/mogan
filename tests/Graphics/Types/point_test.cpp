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
  void test_operator();
  // void test_is_point();
  void test_inner ();
};

void
TestPoint::test_operator() {
  QCOMPARE (point (2, 3) + point (2, 4) == point (4, 7), true);
  QCOMPARE (point (4, 5) - point (2, 3) == point (2, 2), true);
  QCOMPARE (point (2, 3) * 2.0 == point (4, 6), true);
  QCOMPARE (point (4, 6) / point (2, 3) == point (2, 2), true);
  QCOMPARE (point (4, 6) / 2 == point (2, 3), true);

  QCOMPARE (abs (point (-2, 3)) == point (2, 3), true);
}

void
TestPoint::test_inner () {
  QCOMPARE (inner (point (3, 4), point (2, 5)) == 26.0, true);
  QCOMPARE (slanted (point (10, 20), 1.5) == point (40, 20), true);
  QCOMPARE (norm (point (3, 4)) == 5.0, true);
}

QTEST_MAIN (TestPoint)
#include "point_test.moc"
