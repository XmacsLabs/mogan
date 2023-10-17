/******************************************************************************
 * MODULE     : curve_test.cpp
 * DESCRIPTION: Add tests for curve.cpp.
 * COPYRIGHT  : (C) 2023  charonxin Oyyko
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "curve.hpp"
#include "path.hpp"
#include "point.hpp"
#include "qtestcase.h"
#include <QtTest/QtTest>
#include <list>

// using std::list;

class TestCurve : public QObject {
  Q_OBJECT
private slots:
  void text_linearly_dependent ();
  void test_segment ();
  void test_poly_segment ();
  void test_ellipse ();
};

void
TestCurve::text_linearly_dependent () {
  QVERIFY (linearly_dependent (point (1, 1), point (2, 2), point (3, 3)) ==
           true);
  QVERIFY (linearly_dependent (point (1, 1), point (1, 1), point (3, 3)) ==
           true);
  QVERIFY (linearly_dependent (point (1, 1), point (1, 1), point (1, 1)) ==
           true);
  QVERIFY (linearly_dependent (point (1, 1), point (1, 2), point (3, 3)) ==
           false);
}

void
TestCurve::test_segment () {
  curve a= segment (point (2, 3), point (4, 5));
  curve b= segment (point (4, 5), point (2, 3));
  bool  e;
  point a_p= a->grad (1.0, e);
  point b_p= b->grad (1.0, e);
  QCOMPARE (norm (a_p + b_p) < 1e-6, true);
  array<double> abs;
  array<point>  pts;
  array<path>   rcip (3);
  a->get_control_points (abs, pts, rcip);
  QVERIFY (norm (point (2, 3) - pts[0]) < 1e-6 &&
           norm (point (4, 5) - pts[1]) < 1e-6);
}

/**
 * nr_components(): Returns the number n of line segments in the curve.
 * evaluate(): returns the point coordinates on the curve whose parameter value
 * is t. Among them, according to the value of t, find the corresponding line
 * segment on the curve, and calculate the corresponding point coordinates on
 * the line segment. rectify_cumul(): Decompose the curve into segments and add
 * the end point of each segment to the point array cum. bound(): Calculates the
 * distance of the closest point on the curve to a point with parameter value t.
 * curvature(): Calculates the curvature on the curve from a point
 *          with parameter value t1 to a point with parameter value t2.
 *          In this class, this function is implemented to return positive
 * infinity.
 */
void
TestCurve::test_poly_segment () {
  array<point> a (point (1, 1), point (2, 2), point (3, 3));
  curve        b= poly_segment (a, array<path> ());
  QVERIFY (b->nr_components () == 2);
  point c= b->evaluate (1.5);
  QVERIFY (norm (c - point (4, 4)) < 1e-6);
  array<point> cum;
  b->rectify_cumul (cum, 0);
  for (int i= 0; i < 2; i++) {
    QVERIFY (norm (cum[i] - point (i + 2, i + 2)) < 1e-6);
  }
  bool  _d;
  point d= b->grad (1.5, _d);
  QVERIFY (norm (d - point (2, 2)) < 1e-6);
}

void
TestCurve::test_ellipse () {
  auto points=
      array<point> (point (0.0, 0.0), point (0.0, 2.0), point (1.0, 1.0));
  auto  obj= ellipse (points, array<path> (), true);
  point p1 = obj->evaluate (0);
  point p2 = obj->evaluate (0.5);
  point p3 = obj->evaluate (0.25);
  point p4 = obj->evaluate (0.75);
  QVERIFY (norm (p1 - point (0, -sqrt (2) + 1)) < 1e-6);
  QVERIFY (norm (p2 - point (0, 1 + sqrt (2))) < 1e-6);
  QVERIFY (norm (p3 - point (1.0, 1.0)) < 1e-6);
  QVERIFY (norm (p4 - point (-1.0, 1.0)) < 1e-6);
}

QTEST_MAIN (TestCurve)
#include "curve_test.moc"
