/******************************************************************************
 * MODULE     : invalidate_test.cpp
 * DESCRIPTION: Test edit_interface_rep::invalidate (rectangles rs) function
 * COPYRIGHT  : (C) 2025
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "base.hpp"
#include "rectangles.hpp"
#include <QtTest/QtTest>

// Test helper functions to simulate the invalidate logic
void
test_invalidate_rectangles (rectangles rs, SI pixel, SI& out_x1, SI& out_y1,
                            SI& out_x2, SI& out_y2, bool& called) {
  called= false;
  if (!is_nil (rs)) {
    called               = true;
    rectangle  first_rect= rs->item;
    SI         min_x1= first_rect->x1 - pixel, min_y1= first_rect->y1 - pixel;
    SI         max_x2= first_rect->x2 + pixel, max_y2= first_rect->y2 + pixel;
    rectangles current= rs->next;
    while (!is_nil (current)) {
      rectangle& r= current->item;
      min_x1      = min (min_x1, r->x1 - pixel);
      min_y1      = min (min_y1, r->y1 - pixel);
      max_x2      = max (max_x2, r->x2 + pixel);
      max_y2      = max (max_y2, r->y2 + pixel);
      current     = current->next;
    }
    out_x1= min_x1;
    out_y1= min_y1;
    out_x2= max_x2;
    out_y2= max_y2;
  }
}

class TestInvalidate : public QObject {
  Q_OBJECT

private slots:
  void test_invalidate_empty_rectangles ();
  void test_invalidate_single_rectangle ();
  void test_invalidate_multiple_rectangles ();
  void test_invalidate_pixel_expansion ();
};

void
TestInvalidate::test_invalidate_empty_rectangles () {
  rectangles empty_list;
  SI         x1, y1, x2, y2;
  bool       called;

  test_invalidate_rectangles (empty_list, 1, x1, y1, x2, y2, called);

  QVERIFY (!called);
}

void
TestInvalidate::test_invalidate_single_rectangle () {
  rectangle  r (10, 20, 30, 40);
  rectangles list (r);
  SI         x1, y1, x2, y2;
  bool       called;

  test_invalidate_rectangles (list, 1, x1, y1, x2, y2, called);

  QVERIFY (called);
  QVERIFY (x1 == 9);  // 10 - pixel(1)
  QVERIFY (y1 == 19); // 20 - pixel(1)
  QVERIFY (x2 == 31); // 30 + pixel(1)
  QVERIFY (y2 == 41); // 40 + pixel(1)
}

void
TestInvalidate::test_invalidate_multiple_rectangles () {
  rectangle r1 (10, 20, 30, 40);
  rectangle r2 (50, 60, 70, 80);
  rectangle r3 (15, 25, 35, 45);

  rectangles list= rectangles (r1, rectangles (r2, rectangles (r3)));
  SI         x1, y1, x2, y2;
  bool       called;

  test_invalidate_rectangles (list, 1, x1, y1, x2, y2, called);

  QVERIFY (called);

  // Should find the bounding box of all rectangles expanded by pixel
  // r1: [10,20] - [30,40] -> [9,19] - [31,41]
  // r2: [50,60] - [70,80] -> [49,59] - [71,81]
  // r3: [15,25] - [35,45] -> [14,24] - [36,46]
  // Bounding box: [9,19] - [71,81]
  QVERIFY (x1 == 9);  // min(9, 49, 14)
  QVERIFY (y1 == 19); // min(19, 59, 24)
  QVERIFY (x2 == 71); // max(31, 71, 36)
  QVERIFY (y2 == 81); // max(41, 81, 46)
}

void
TestInvalidate::test_invalidate_pixel_expansion () {
  rectangle  r (100, 200, 300, 400);
  rectangles list (r);
  SI         x1, y1, x2, y2;
  bool       called;

  test_invalidate_rectangles (list, 5, x1, y1, x2, y2, called);

  QVERIFY (called);
  QVERIFY (x1 == 95);  // 100 - 5
  QVERIFY (y1 == 195); // 200 - 5
  QVERIFY (x2 == 305); // 300 + 5
  QVERIFY (y2 == 405); // 400 + 5
}

QTEST_MAIN (TestInvalidate)
#include "invalidate_test.moc"