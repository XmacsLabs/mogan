/******************************************************************************
 * MODULE     : database_basic_function_test.cpp
 * COPYRIGHT  : (C) paradisuman
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Database/database.hpp"
#include "sys_utils.hpp"
#include "tm_timer.hpp"
#include "tree_helper.hpp"

#include <QtTest/QtTest>

extern hashmap<tree, int> db_index;

class TestDatabaseBasicFunciton : public QObject {
  Q_OBJECT
private slots:
  void init () {}
  void test_set_field_and_get_field ();
  void test_remove_field ();
  void test_set_entry_and_get_entry ();
  void test_remove_entry ();
  void test_get_attributes ();
  void test_query ();
};

void
TestDatabaseBasicFunciton::test_set_field_and_get_field () {
  url test_db= url_system ("db1");
  // test single string
  set_field (test_db, "single", "", array<string> ("", 1),
             (double) get_sec_time ());
  set_field (test_db, "single", "a", array<string> ("b", 1),
             (double) get_sec_time ());
  set_field (test_db, "single", "1", array<string> ("2b", 1),
             (double) get_sec_time ());
  set_field (test_db, "single", "cd", array<string> ("3a", 1),
             (double) get_sec_time ());

  array<string> s1=
      get_field (test_db, "single", "a", (double) get_sec_time ());
  array<string> s2=
      get_field (test_db, "single", "1", (double) get_sec_time ());
  array<string> s3=
      get_field (test_db, "single", "cd", (double) get_sec_time ());
  array<string> s4= get_field (test_db, "single", "", (double) get_sec_time ());

  QVERIFY (s1[0] == "b");
  QVERIFY (s2[0] == "2b");
  QVERIFY (s3[0] == "3a");
  QVERIFY (s4[0] == "");

  // test multiple string
  set_field (test_db, "multiple", "a1", array<string> ("b", "c"),
             (double) get_sec_time ());
  set_field (test_db, "multiple", "1b", array<string> ("2b", "23bb"),
             (double) get_sec_time ());
  set_field (test_db, "multiple", "345asd",
             array<string> ("3a", "213fd", "wffwafw"),
             (double) get_sec_time ());

  array<string> s5=
      get_field (test_db, "multiple", "a1", (double) get_sec_time ());
  array<string> s6=
      get_field (test_db, "multiple", "1b", (double) get_sec_time ());
  array<string> s7=
      get_field (test_db, "multiple", "345asd", (double) get_sec_time ());

  QVERIFY (s5 == array<string> ("b", "c"));
  QVERIFY (s6 == array<string> ("2b", "23bb"));
  QVERIFY (s7 == array<string> ("3a", "213fd", "wffwafw"));
}

void
TestDatabaseBasicFunciton::test_remove_field () {
  url test_db= url_system ("db2");

  set_field (test_db, "remove", "a", array<string> ("b", 1),
             (double) get_sec_time ());
  set_field (test_db, "remove", "1", array<string> ("2b", 1),
             (double) get_sec_time ());
  set_field (test_db, "remove", "cd", array<string> ("3a", 1),
             (double) get_sec_time ());
  set_field (test_db, "remove", "a1", array<string> ("b", "c"),
             (double) get_sec_time ());

  remove_field (test_db, "remove", "a", (double) get_sec_time ());
  remove_field (test_db, "remove", "cd", (double) get_sec_time ());
  remove_field (test_db, "remove", "1", (double) get_sec_time ());

  // clean and write to disk
  keep_history (test_db, true);
  keep_history (test_db, false);
  db_index->reset (url (test_db)->t);

  array<string> s1=
      get_field (test_db, "remove", "a", (double) get_sec_time ());
  array<string> s2=
      get_field (test_db, "remove", "1", (double) get_sec_time ());
  array<string> s3=
      get_field (test_db, "remove", "cd", (double) get_sec_time ());
  array<string> s4=
      get_field (test_db, "remove", "a1", (double) get_sec_time ());

  QVERIFY (N (s1) == 0);
  QVERIFY (N (s2) == 0);
  QVERIFY (N (s3) == 0);
  QVERIFY (s4 == array<string> ("b", "c"));
}

void
TestDatabaseBasicFunciton::test_set_entry_and_get_entry () {
  url test_db= url_system ("db3");

  tree t1 (TUPLE, tree (TUPLE, "a", "a1"));
  tree t2 (TUPLE, tree (TUPLE, "b1", "bb11"), tree (TUPLE, "b2", "bb22"));
  tree t3 (TUPLE, tree (TUPLE, "c1", "cc11"), tree (TUPLE, "c2", "cc22"),
           tree (TUPLE, "c3", "cc33"));

  set_entry (test_db, "entry1", t1, (double) get_sec_time ());
  set_entry (test_db, "entry2", t2, (double) get_sec_time ());
  set_entry (test_db, "entry3", t3, (double) get_sec_time ());

  // from 245 (245 (a, a1)) to 245 (245 ("a", "a1"))
  tree t4= get_entry (test_db, "entry1", (double) get_sec_time ());
  tree t5= get_entry (test_db, "entry2", (double) get_sec_time ());
  tree t6= get_entry (test_db, "entry3", (double) get_sec_time ());

  QVERIFY (t4 == tree (TUPLE, tree (TUPLE, "\"a\"", "\"a1\"")));
  QVERIFY (t5 == tree (TUPLE, tree (TUPLE, "\"b1\"", "\"bb11\""),
                       tree (TUPLE, "\"b2\"", "\"bb22\"")));
  QVERIFY (t6 == tree (TUPLE, tree (TUPLE, "\"c1\"", "\"cc11\""),
                       tree (TUPLE, "\"c2\"", "\"cc22\""),
                       tree (TUPLE, "\"c3\"", "\"cc33\"")));
}

void
TestDatabaseBasicFunciton::test_remove_entry () {
  url test_db= url_system ("db4");

  tree t1 (TUPLE, tree (TUPLE, "a", "a1"));
  tree t2 (TUPLE, tree (TUPLE, "b1", "bb11"), tree (TUPLE, "b2", "bb22"));
  tree t3 (TUPLE, tree (TUPLE, "c1", "cc11"), tree (TUPLE, "c2", "cc22"),
           tree (TUPLE, "c3", "cc33"));

  set_entry (test_db, "entryre1", t1, (double) get_sec_time ());
  set_entry (test_db, "entryre2", t2, (double) get_sec_time ());
  set_entry (test_db, "entryre3", t3, (double) get_sec_time ());

  // clean and write to disk
  keep_history (test_db, true);
  keep_history (test_db, false);
  db_index->reset (url (test_db)->t);

  remove_entry (test_db, "entryre1", (double) get_sec_time ());
  remove_entry (test_db, "entryre3", (double) get_sec_time ());

  tree t4= get_entry (test_db, "entryre1", (double) get_sec_time ());
  tree t5= get_entry (test_db, "entryre2", (double) get_sec_time ());
  tree t6= get_entry (test_db, "entryre3", (double) get_sec_time ());

  QVERIFY (N (t4) == 0);
  QVERIFY (t5 == tree (TUPLE, tree (TUPLE, "\"b1\"", "\"bb11\""),
                       tree (TUPLE, "\"b2\"", "\"bb22\"")));
  QVERIFY (N (t6) == 0);
}

void
TestDatabaseBasicFunciton::test_get_attributes () {
  url test_db= url_system ("db5");

  set_field (test_db, "attributes", "a1", array<string> ("b", "c"),
             (double) get_sec_time ());
  set_field (test_db, "attributes", "1b", array<string> ("2b", "23bb"),
             (double) get_sec_time ());
  set_field (test_db, "attributes", "345asd",
             array<string> ("3a", "213fd", "wffwafw"),
             (double) get_sec_time ());

  array<string> ans=
      get_attributes (test_db, "attributes", (double) get_sec_time ());

  QVERIFY (ans == array<string> ("a1", "1b", "345asd"));
}

void
TestDatabaseBasicFunciton::test_query () {
  url test_db= url_system ("db6");

  set_field (test_db, "query1", "a", array<string> ("b", 1),
             (double) get_usec_time ());
  set_field (test_db, "query1", "23", array<string> ("2b", 1),
             (double) get_usec_time ());
  set_field (test_db, "query1", "11cd", array<string> ("3a", 1),
             (double) get_usec_time ());

  set_field (test_db, "query2", "a", array<string> ("bv", 1),
             (double) get_usec_time ());
  set_field (test_db, "query2", "23a", array<string> ("2b", 1),
             (double) get_usec_time ());
  set_field (test_db, "query2", "11cda", array<string> ("3a", 1),
             (double) get_usec_time ());

  tree q1 (TUPLE, tree (TUPLE, "order", "a", "#t"));
  auto a1= query (test_db, q1, (double) get_usec_time (), 1000000);

  // sort ascending order
  QVERIFY (a1 == array<string> ("query1", "query2"));
}

QTEST_MAIN (TestDatabaseBasicFunciton)
#include "database_basic_function_test.moc"
