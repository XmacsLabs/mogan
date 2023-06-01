
/******************************************************************************
 * MODULE     : tree_test.cpp
 * DESCRIPTION: Tests on tree
 * COPYRIGHT  : (C) 2019-2021  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qtestcase.h"
#include "rectangles.hpp"
#include <QtTest/QtTest>

class TestRectangle : public QObject {
  Q_OBJECT

private slots:
  void test_operators ();
  void test_intersect ();
  void test_translate ();
  void test_thicken ();
  void test_area ();
};

void
TestRectangle::test_operators () {
  QVERIFY (rectangle (0, 0, 1, 1) == rectangle (0, 0, 1, 1));
  QVERIFY (rectangle (0, 0, 1, 1) != rectangle (0, 0, 2, 1));
  QVERIFY (rectangle (0, 0, 1, 1) <= rectangle (-1, -1, 2, 2));
  QVERIFY (rectangle (0, 0, 1, 1) * 2 == rectangle (0, 0, 2, 2));
  QVERIFY (rectangle (0, 0, 1, 1) / 2 == rectangle (0, 0, 0, 0));
  QVERIFY (rectangle (0, 0, 1, 1) / 2.0 == rectangle (0, 0, 1, 1));
}

void
TestRectangle::test_intersect () {
  QVERIFY (intersect (rectangle (0, 0, 2, 1), rectangle (1, 0, 2, 2)));
}

void
TestRectangle::test_translate () {
  QVERIFY (translate (rectangle (0, 0, 1, 1), 1, 5) == rectangle (1, 5, 2, 6));
}

void
TestRectangle::test_thicken () {
  QVERIFY (thicken (rectangle (0, 0, 2, 2), 1, 5) == rectangle (-1, -5, 3, 7));
}

void
TestRectangle::test_area () {
  QVERIFY (area (rectangle (0, 0, 2, 2)) == 4);
}

QTEST_MAIN (TestRectangle)
#include "rectangle_test.moc"
