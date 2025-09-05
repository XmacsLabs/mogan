/******************************************************************************
 * MODULE     : rectangles_test.cpp
 * DESCRIPTION: Test rectangles disjoint_union function
 * COPYRIGHT  : (C) 2025
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "base.hpp"
#include "rectangles.hpp"
#include <QtTest/QtTest>

class TestRectangles : public QObject {
  Q_OBJECT

private slots:
  void test_disjoint_union_empty_list ();
  void test_disjoint_union_non_adjacent ();
  void test_disjoint_union_adjacent_horizontal ();
  void test_disjoint_union_adjacent_vertical ();
  void test_disjoint_union_multiple_adjacent ();
  void test_disjoint_union_overlapping ();
  void test_disjoint_union_complex_case ();
};

void
TestRectangles::test_disjoint_union_empty_list () {
  rectangles empty_list;
  rectangle  r (10, 10, 20, 20);
  rectangles result= disjoint_union (empty_list, r);

  QVERIFY (!is_nil (result));
  QVERIFY (is_nil (result->next));
  QVERIFY (result->item == r);
}

void
TestRectangles::test_disjoint_union_non_adjacent () {
  rectangle  r1 (0, 0, 10, 10);
  rectangle  r2 (20, 20, 30, 30);
  rectangles list (r1);
  rectangles result= disjoint_union (list, r2);

  int count= 0;
  for (rectangles p= result; !is_nil (p); p= p->next) {
    count++;
  }
  QVERIFY (count == 2);

  bool found_r1= false, found_r2= false;
  for (rectangles p= result; !is_nil (p); p= p->next) {
    if (p->item == r1) found_r1= true;
    if (p->item == r2) found_r2= true;
  }
  QVERIFY (found_r1 && found_r2);
}

void
TestRectangles::test_disjoint_union_adjacent_horizontal () {
  rectangle  r1 (0, 0, 10, 10);
  rectangle  r2 (10, 0, 20, 10);
  rectangles list (r1);
  rectangles result= disjoint_union (list, r2);

  QVERIFY (!is_nil (result));
  QVERIFY (is_nil (result->next));

  rectangle merged= result->item;
  QVERIFY (merged->x1 == 0);
  QVERIFY (merged->y1 == 0);
  QVERIFY (merged->x2 == 20);
  QVERIFY (merged->y2 == 10);
}

void
TestRectangles::test_disjoint_union_adjacent_vertical () {
  rectangle  r1 (0, 0, 10, 10);
  rectangle  r2 (0, 10, 10, 20);
  rectangles list (r1);
  rectangles result= disjoint_union (list, r2);

  QVERIFY (!is_nil (result));
  QVERIFY (is_nil (result->next));

  rectangle merged= result->item;
  QVERIFY (merged->x1 == 0);
  QVERIFY (merged->y1 == 0);
  QVERIFY (merged->x2 == 10);
  QVERIFY (merged->y2 == 20);
}

void
TestRectangles::test_disjoint_union_multiple_adjacent () {
  rectangle  r1 (0, 0, 10, 10);
  rectangle  r2 (10, 0, 20, 10);
  rectangle  r3 (20, 0, 30, 10);
  rectangles list  = rectangles (r2, rectangles (r1));
  rectangles result= disjoint_union (list, r3);

  QVERIFY (!is_nil (result));
  QVERIFY (is_nil (result->next));

  rectangle merged= result->item;
  QVERIFY (merged->x1 == 0);
  QVERIFY (merged->y1 == 0);
  QVERIFY (merged->x2 == 30);
  QVERIFY (merged->y2 == 10);
}

void
TestRectangles::test_disjoint_union_overlapping () {
  rectangle  r1 (0, 0, 15, 10);
  rectangle  r2 (10, 0, 20, 10);
  rectangles list (r1);
  rectangles result= disjoint_union (list, r2);

  // disjoint_union only merges adjacent rectangles, not overlapping ones
  // So overlapping rectangles should remain separate
  int count= 0;
  for (rectangles p= result; !is_nil (p); p= p->next) {
    count++;
  }
  QVERIFY (count == 2);

  bool found_r1= false, found_r2= false;
  for (rectangles p= result; !is_nil (p); p= p->next) {
    if (p->item == r1) found_r1= true;
    if (p->item == r2) found_r2= true;
  }
  QVERIFY (found_r1 && found_r2);
}

void
TestRectangles::test_disjoint_union_complex_case () {
  rectangle  r1 (0, 0, 10, 10);
  rectangle  r2 (15, 15, 25, 25);
  rectangle  r3 (10, 0, 20, 10);
  rectangles list  = rectangles (r2, rectangles (r1));
  rectangles result= disjoint_union (list, r3);

  // 应该有两个：一个是 [0,0]-[20,10] 的合并矩形，另一个是 r2 原样保留
  int count= 0;
  for (rectangles p= result; !is_nil (p); p= p->next)
    count++;
  QVERIFY (count == 2);

  bool has_merged= false, has_r2= false;
  for (rectangles p= result; !is_nil (p); p= p->next) {
    rectangle rc= p->item;
    if (rc->x1 == 0 && rc->y1 == 0 && rc->x2 == 20 && rc->y2 == 10)
      has_merged= true;
    if (p->item == r2) has_r2= true; // r2 未被改写时可用指针判断
  }
  QVERIFY (has_merged && has_r2);
}

QTEST_MAIN (TestRectangles)
#include "rectangles_test.moc"