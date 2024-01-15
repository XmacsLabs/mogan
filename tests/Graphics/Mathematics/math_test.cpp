
/******************************************************************************
 * MODULE     : test_math.cpp
 * DESCRIPTION: Test mathematical functions
 * COPYRIGHT  : (C) 2006  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "base.hpp"
#include "math_tree.hpp"
#include "matrix.hpp"
#include "polynomial.hpp"
#include "vector.hpp"
#include <QtTest/QtTest>
Q_DECLARE_METATYPE (tree)

class TestMath : public QObject {
  Q_OBJECT

private slots:
  void test_math_tree ();
  void test_vector ();
  void test_matrix ();
  void test_polynomial ();
};

void
TestMath::test_math_tree () {
  tree t= add ("x", mul (pow ("y", "2"), "z"));
  qcompare (as_math_string (t), "x + y^2 * z");
  qcompare (as_math_string (mul (t, t)), "(x + y^2 * z) * (x + y^2 * z)");
  qcompare (as_math_string (as_tree (vector<tree> (t, t))),
            "[ x + y^2 * z, x + y^2 * z ]");
}

void
TestMath::test_vector () {
  vector<double> v (1.0, 2.0, 3.0);
  QCOMPARE (as_tree (v), tree (TUPLE, "1", "2", "3"));
  QCOMPARE (as_tree (exp (v)), tree (TUPLE, as_tree (exp (1)),
                                     as_tree (exp (2)), as_tree (exp (3))));
  QCOMPARE (norm (v), 3.74165738677394);
}

void
TestMath::test_matrix () {
  vector<double> v (1.0, 2.0);
  matrix<double> m (1.0, 2, 2);
  m (0, 1)= 4;
  QCOMPARE (as_tree (m),
            tree (TUPLE, tree (TUPLE, "1", "4"), tree (TUPLE, "0", "1")));
  qcompare (as_math_string (as_tree (m)), "[ [ 1, 4 ], [ 0, 1 ] ]");
  QCOMPARE (as_tree (v), tree (TUPLE, "1", "2"));
  QCOMPARE (as_tree (m * m),
            tree (TUPLE, tree (TUPLE, "1", "8"), tree (TUPLE, "0", "1")));
  QCOMPARE (as_tree (m * v), tree (TUPLE, "9", "2"));
  qcompare (as_math_string (as_tree (m * m)), "[ [ 1, 8 ], [ 0, 1 ] ]");
  qcompare (as_math_string (as_tree (m * v)), "[ 9, 2 ]");
}

void
TestMath::test_polynomial () {
  polynomial<double> p (1.0, 2.0, 3.0);
  polynomial<double> q= p * p;
  QCOMPARE (as_tree (q),
            tree (PLUS,
                  tree (PLUS,
                        tree (PLUS, tree (PLUS, "1", tree (TIMES, "4", "x")),
                              tree (TIMES, "10", tree (POW, "x", "2"))),
                        tree (TIMES, "12", tree (POW, "x", "3"))),
                  tree (TIMES, "9", tree (POW, "x", "4"))));
  qcompare (as_math_string (as_tree (p)), "1 + 2 * x + 3 * x^2");
  qcompare (as_math_string (as_tree (q)),
            "1 + 4 * x + 10 * x^2 + 12 * x^3 + 9 * x^4");
  qcompare (as_math_string (as_tree (p - p)), "0");
  qcompare (as_math_string (as_tree (p * p)),
            "1 + 4 * x + 10 * x^2 + 12 * x^3 + 9 * x^4");
  qcompare (as_math_string (as_tree (p * p + p)),
            "2 + 6 * x + 13 * x^2 + 12 * x^3 + 9 * x^4");
  qcompare (as_math_string (as_tree (p * p * p)),
            "1 + 6 * x + 21 * x^2 + 44 * x^3 + 63 * x^4 + 54 * x^5 + 27 * x^6");
  QCOMPARE (p (3.0), 34);
  QCOMPARE (q (3.0, 0), 1156);
  QCOMPARE (q (3.0, 1), 1360);
  QCOMPARE (q (3.0, 2), 1208);
}

QTEST_MAIN (TestMath)
#include "math_test.moc"
