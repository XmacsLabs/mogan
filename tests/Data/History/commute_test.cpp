/******************************************************************************
 * MODULE     : commute_test.cpp
 * DESCRIPTION: Consistency checks of modification
 * COPYRIGHT  : (C) 2009-2023  Joris van der Hoeven
 *                       2023  jingkaimori
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "modification.hpp"
#include "patch.hpp"
#include "tree_label.hpp"
#include <QtTest/QtTest>

#include <iostream>

class TestCommute : public QObject {
  Q_OBJECT

private slots:
  void test_commute ();
  void test_invert ();
};

static modification
test_modification (int i) {
  switch (i) {
  case 0:
    return mod_assign (path (), "Hi");
  case 1:
    return mod_insert (path (), 0, tree (TUPLE, "a", "b"));
  case 2:
    return mod_remove (path (), 0, 2);
  case 3:
    return mod_split (path (), 0, 1);
  case 4:
    return mod_join (path (), 0);
  case 5:
    return mod_assign_node (path (), TUPLE);
  case 6:
    return mod_insert_node (path (), 1, tree (TUPLE, "a", "b"));
  case 7:
    return mod_remove_node (path (), 0);

  case 8:
    return mod_insert (path (), 1, tree (TUPLE, "a", "b"));
  case 9:
    return mod_insert (path (), 2, tree (TUPLE, "a", "b"));
  case 10:
    return mod_remove (path (), 1, 2);
  case 11:
    return mod_remove (path (), 2, 2);
  case 12:
    return mod_split (path (), 1, 2);
  case 13:
    return mod_split (path (), 2, 1);
  case 14:
    return mod_join (path (), 1);
  case 15:
    return mod_join (path (), 2);
  case 16:
    return mod_remove_node (path (), 1);
  case 17:
    return mod_remove_node (path (), 2);

  case 18:
  case 19:
  case 20:
  case 21:
  case 22:
  case 23:
  case 24:
  case 25:
    return path (0) * test_modification (i - 18);
  case 26:
  case 27:
  case 28:
  case 29:
  case 30:
  case 31:
  case 32:
  case 33:
    return path (1) * test_modification (i - 26);
  case 34:
  case 35:
  case 36:
  case 37:
  case 38:
  case 39:
  case 40:
  case 41:
    return path (2) * test_modification (i - 34);
  default:
    TM_FAILED ("not implemented");
    return mod_assign (path (), "");
  }
}

static tree
test_tree (int i= 0, int d= 3) {
  // cout << "i= " << i << ", d= " << d << "\n";
  if (d == 0) return tree (as_string (i));
  else {
    int  n= 6 + ((int) (2 * sin (1.0 * i * d)));
    tree t (TUPLE, n);
    for (int j= 0; j < n; i++, j++)
      t[j]= test_tree (i, d - 1);
    return t;
  }
}

void
TestCommute::test_commute () {
  tree tt= test_tree ();
  for (int i= 0; i < 42; i++)
    for (int j= 0; j < 42; j++) {
      modification m1= test_modification (i);
      modification m2= test_modification (j);
      modification t1= m1;
      modification t2= m2;
      cout << "m1  = " << m1 << "\n";
      cout << "m2  = " << m2 << "\n";
      bool         r = swap (m1, m2);
      modification u1= m1;
      modification u2= m2;
      if (!r) cout << "  Modifications do not commute\n\n";
      else {
        cout << "m1' = " << m1 << "\n";
        cout << "m2' = " << m2 << "\n";
        QCOMPARE (clean_apply (clean_apply (tt, t1), t2),
                  clean_apply (clean_apply (tt, m1), m2));
        r= swap (m1, m2);
        if (!r) cout << "r   = " << r << "\n";
        else if (m1 != t1 || m2 != t2) {
          cout << "m1''= " << m1 << "\n";
          cout << "m2''= " << m2 << "\n";
          r= swap (m1, m2);
          if (!r) cout << "r   = " << r << "\n";
          else if (m1 != u1 || m2 != u2) {
            cout << "m1* = " << m1 << "\n";
            cout << "m2* = " << m2 << "\n";
            r= false;
          }
        }
        // Consistency check
        QVERIFY (r);
      }
    }
}

void
TestCommute::test_invert () {
  tree t1= test_tree ();
  for (int i= 0; i < 42; i++) {
    modification m1= test_modification (i);
    tree         t2= clean_apply (t1, m1);
    modification m2= invert (m1, t1);
    tree         t3= clean_apply (t2, m2);
    modification m3= invert (m2, t2);
    QCOMPARE (m1, m3);
    QCOMPARE (t1, t3);
  }
}
QTEST_MAIN (TestCommute)
#include "commute_test.moc"
