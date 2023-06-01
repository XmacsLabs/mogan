
/******************************************************************************
 * MODULE     : tree_test.cpp
 * DESCRIPTION: Tests on tree
 * COPYRIGHT  : (C) 2019-2021  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tree.hpp"
#include <QtTest/QtTest>

class TestTree : public QObject {
  Q_OBJECT

private slots:
  void test_is_atomic ();
  void test_is_tuple ();
  void test_is_concat ();
  void test_is_compound ();
  void test_is_bool ();
  void test_is_int ();
  void test_is_func ();
  void test_is_double ();
  void test_is_string ();
  void test_is_generic ();
  void test_get_label ();
  void test_arity ();
};

void
TestTree::test_is_atomic () {
  QVERIFY (is_atomic (tree ()));
}

void
TestTree::test_is_tuple () {
  QVERIFY (is_tuple (tuple ()));
  QVERIFY (is_tuple (tuple (tree ())));
  QVERIFY (is_tuple (tuple (tree (), tree ())));
  QVERIFY (is_tuple (tuple (tree (), tree (), tree ())));
  QVERIFY (is_tuple (tuple (tree (), tree (), tree (), tree ())));
  QVERIFY (is_tuple (tuple (tree (), tree (), tree (), tree (), tree ())));
}

void
TestTree::test_is_concat () {
  QVERIFY (is_concat (concat ()));
  QVERIFY (is_concat (concat (tree ())));
  QVERIFY (is_concat (concat (tree (), tree ())));
  QVERIFY (is_concat (concat (tree (), tree (), tree ())));
  QVERIFY (is_concat (concat (tree (), tree (), tree (), tree ())));
  QVERIFY (is_concat (concat (tree (), tree (), tree (), tree (), tree ())));
}

void
TestTree::test_is_compound () {
  QVERIFY (is_compound (tree (FRAC, 3)));
  QVERIFY (!is_compound (tree ()));
}

void
TestTree::test_is_bool () {
  QVERIFY (is_bool (tree ("true")));
  QVERIFY (is_bool (tree ("false")));
  QVERIFY (!is_bool (tree ("other")));
}

void
TestTree::test_is_int () {
  QVERIFY (!is_int (tree ()));
  QVERIFY (is_int (tree ("+12")));
  QVERIFY (is_int (tree ("-12")));
  QVERIFY (!is_int (tree ("-+12")));
  QVERIFY (!is_int (tree ("one")));
}

void
TestTree::test_is_func () {
  QVERIFY (!is_func (tree (FRAC, 3),tree_label(DOCUMENT)));
  QVERIFY (!is_func (tree (),tree_label()));
  QVERIFY (is_func (tree (LSUP,4),tree_label(LSUP)));
}

void
TestTree::test_is_double () {
  QVERIFY (!is_double (tree ()));
  QVERIFY (is_double (tree ("3.15")));
  QVERIFY (is_double (tree ("0")));
  QVERIFY (is_double (tree ("0.0")));
  QVERIFY (is_double (tree ("3.1415926")));
  QVERIFY (is_double (tree ("-3.1415926")));
  QVERIFY (!is_double (tree ("nothing")));
}

void
TestTree::test_is_string () {
  QVERIFY (is_string (tree ()));
  QVERIFY (is_string (tree ("0")));
  QVERIFY (is_string (tree ("+121234")));
  QVERIFY (is_string (tree ("seigj")));
  QVERIFY (!is_string (tree ("!@#$$!")));
  QVERIFY (!is_string (tree (FRAC, 3)));
}

void
TestTree::test_is_generic () {
  QVERIFY (!is_generic (tree (RIGID,0)));
  QVERIFY (!is_generic (tree (PARA,-1)));
  QVERIFY (!is_generic (tree (RIGID,1234)));
  QVERIFY (is_generic (tree (IF_PAGE_BREAK,-45)));
  QVERIFY (!is_generic (tree (RIGID,2)));
}

void
TestTree::test_get_label () {
  QVERIFY (get_label (tree (RIGID,1)) != string("RIGID"));
  QVERIFY (get_label (tree (ARC,0)) != string("RIGID"));
  QVERIFY (get_label (tree (ARC,-1)) == string("ARC"));
}

void
TestTree::test_arity () {
  QVERIFY (!arity (tree (STRING, -1)));
  QVERIFY (!arity (tree (ARC, 0)));
  QVERIFY (arity (tree (RIGID,3)));
  QVERIFY (!arity (tree (STRING,5)));
}

QTEST_MAIN (TestTree)
#include "tree_test.moc"
