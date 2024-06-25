/******************************************************************************
 * MODULE     : database_basic_function_test.cpp
 * COPYRIGHT  : (C) paradisuman
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Database/database.hpp"
#include "analyze.hpp"
#include "base.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "tm_timer.hpp"
#include "tree_helper.hpp"

#include <QtTest/QtTest>
#include <moebius/data/scheme.hpp>

using moebius::data::scm_quote;

extern hashmap<tree, int> db_index;

class TestDatabaseBasicFunciton : public QObject {
  Q_OBJECT
private slots:
  void init () { init_lolly (); }
  void test_set_field_and_get_field ();
  void test_remove_field ();
  void test_set_entry_and_get_entry ();
  void test_remove_entry ();
  void test_get_attributes ();
  void test_query ();
  void test_get_completions ();
};

void
TestDatabaseBasicFunciton::test_set_field_and_get_field () {
  url test_db= url_temp ("db1");
  // test single string
  string single_val1[1]= {"defg"};
  string single_val2[1]= {"5678"};
  string single_val3[1]= {"defg5678"};
  string single_val4[1]= {""};

  set_field (test_db, "single", "abc", array<string> (single_val1, 1),
             (double) get_sec_time ());
  set_field (test_db, "single", "1234", array<string> (single_val2, 1),
             (double) get_sec_time ());
  set_field (test_db, "single", "abc1234", array<string> (single_val3, 1),
             (double) get_sec_time ());
  set_field (test_db, "single", "", array<string> (single_val4, 1),
             (double) get_sec_time ());

  array<string> s1=
      get_field (test_db, "single", "abc", (double) get_sec_time ());
  array<string> s2=
      get_field (test_db, "single", "1234", (double) get_sec_time ());
  array<string> s3=
      get_field (test_db, "single", "abc1234", (double) get_sec_time ());
  array<string> s4= get_field (test_db, "single", "", (double) get_sec_time ());

  QVERIFY (s1[0] == "defg");
  QVERIFY (s2[0] == "5678");
  QVERIFY (s3[0] == "defg5678");
  QVERIFY (s4[0] == "");

  // test multiple string
  set_field (test_db, "multiple", "teacher", array<string> ("Bob", "Alice"),
             (double) get_sec_time ());
  set_field (test_db, "multiple", "number", array<string> ("123", "456"),
             (double) get_sec_time ());
  set_field (test_db, "multiple", "room", array<string> ("A1", "B2", "C3"),
             (double) get_sec_time ());

  array<string> s5=
      get_field (test_db, "multiple", "teacher", (double) get_sec_time ());
  array<string> s6=
      get_field (test_db, "multiple", "number", (double) get_sec_time ());
  array<string> s7=
      get_field (test_db, "multiple", "room", (double) get_sec_time ());

  QVERIFY (s5 == array<string> ("Bob", "Alice"));
  QVERIFY (s6 == array<string> ("123", "456"));
  QVERIFY (s7 == array<string> ("A1", "B2", "C3"));
}

void
TestDatabaseBasicFunciton::test_remove_field () {
  url test_db= url_temp ("db2");

  set_field (test_db, "remove", "no1", array<string> ("no1_name", "1"),
             (double) get_sec_time ());
  set_field (test_db, "remove", "no2", array<string> ("no2_name", "2"),
             (double) get_sec_time ());
  set_field (test_db, "remove", "no3", array<string> ("no3_name", "3"),
             (double) get_sec_time ());
  set_field (test_db, "remove", "no4", array<string> ("no4_name", "4"),
             (double) get_sec_time ());

  remove_field (test_db, "remove", "no1", (double) get_sec_time ());
  remove_field (test_db, "remove", "no3", (double) get_sec_time ());
  remove_field (test_db, "remove", "no4", (double) get_sec_time ());

  // clean and write to disk
  keep_history (test_db, true);
  keep_history (test_db, false);
  db_index->reset (as_tree (url (test_db)));

  array<string> s1=
      get_field (test_db, "remove", "no1", (double) get_sec_time ());
  array<string> s2=
      get_field (test_db, "remove", "no2", (double) get_sec_time ());
  array<string> s3=
      get_field (test_db, "remove", "no3", (double) get_sec_time ());
  array<string> s4=
      get_field (test_db, "remove", "no4", (double) get_sec_time ());

  QVERIFY (N (s1) == 0);
  QVERIFY (N (s3) == 0);
  QVERIFY (N (s4) == 0);
  QVERIFY (s2 == array<string> ("no2_name", "2"));
}

void
TestDatabaseBasicFunciton::test_set_entry_and_get_entry () {
  url test_db= url_temp ("db3");

  tree t1= tuple (tuple ("camera", "Sony"));
  tree t2= tuple (tuple ("phone", "huawei"), tuple ("car", "BYD"));
  tree t3= tuple (tuple ("phone", "apple"), tuple ("mall", "Sam"));

  set_entry (test_db, "Japan", t1, (double) get_sec_time ());
  set_entry (test_db, "China", t2, (double) get_sec_time ());
  set_entry (test_db, "USA", t3, (double) get_sec_time ());

  // from 245 (245 (a, a1)) to 245 (245 ("a", "a1"))
  tree t4= get_entry (test_db, "Japan", (double) get_sec_time ());
  tree t5= get_entry (test_db, "China", (double) get_sec_time ());
  tree t6= get_entry (test_db, "USA", (double) get_sec_time ());

  QVERIFY (t4 == tuple (tuple ("\"camera\"", "\"Sony\"")));
  QVERIFY (t5 == tuple (tuple ("\"phone\"", "\"huawei\""),
                        tuple ("\"car\"", "\"BYD\"")));
  QVERIFY (t6 == tuple (tuple ("\"phone\"", "\"apple\""),
                        tuple ("\"mall\"", "\"Sam\"")));
}

void
TestDatabaseBasicFunciton::test_remove_entry () {
  url test_db= url_temp ("db4");

  tree t1= tuple (tuple ("camera", "Sony"));
  tree t2= tuple (tuple ("phone", "huawei"), tuple ("car", "BYD"));
  tree t3= tuple (tuple ("phone", "apple"), tuple ("mall", "Sam"));

  set_entry (test_db, "Japan", t1, (double) get_sec_time ());
  set_entry (test_db, "China", t2, (double) get_sec_time ());
  set_entry (test_db, "USA", t3, (double) get_sec_time ());

  // clean and write to disk
  keep_history (test_db, true);
  keep_history (test_db, false);
  db_index->reset (as_tree (url (test_db)));

  remove_entry (test_db, "Japan", (double) get_sec_time ());
  remove_entry (test_db, "USA", (double) get_sec_time ());

  // from 245 (245 (a, a1)) to 245 (245 ("a", "a1"))
  tree t4= get_entry (test_db, "Japan", (double) get_sec_time ());
  tree t5= get_entry (test_db, "China", (double) get_sec_time ());
  tree t6= get_entry (test_db, "USA", (double) get_sec_time ());

  QVERIFY (N (t4) == 0);
  QVERIFY (t5 == tuple (tuple ("\"phone\"", "\"huawei\""),
                        tuple ("\"car\"", "\"BYD\"")));
  QVERIFY (N (t6) == 0);
}

void
TestDatabaseBasicFunciton::test_get_attributes () {
  url test_db= url_system ("db5");

  set_field (test_db, "attributes", "age", array<string> ("1", "18"),
             (double) get_sec_time ());
  set_field (test_db, "attributes", "name", array<string> ("bob", "black"),
             (double) get_sec_time ());
  set_field (test_db, "attributes", "id",
             array<string> ("33dsa", "213fd", "wf3fw"),
             (double) get_sec_time ());

  array<string> ans=
      get_attributes (test_db, "attributes", (double) get_sec_time ());

  QVERIFY (ans == array<string> ("age", "name", "id"));
}

void
TestDatabaseBasicFunciton::test_query () {
  url test_db= url_temp ("db6");

  // use % 1000 to avoid sec_time too big to convert to an inaccurate number
  set_field (test_db, "query1", "no1", array<string> ("no1_name", "1"),
             (double) (get_sec_time () % 1000));
  set_field (test_db, "query1", "no2", array<string> ("no2_name", "2"),
             (double) (get_sec_time () % 1000));
  set_field (test_db, "query1", "no3", array<string> ("no3_name", "3"),
             (double) (get_sec_time () % 1000));

  double t1= (double) (get_sec_time () % 1000);

  while ((double) (get_sec_time () % 1000) <= t1)
    ;
  t1= (double) (get_sec_time () % 1000);

  set_field (test_db, "query2", "no1", array<string> ("no1_name", "1"),
             (double) (get_sec_time () % 1000));
  set_field (test_db, "query2", "no4", array<string> ("no4_name", "4"),
             (double) (get_sec_time () % 1000));
  set_field (test_db, "query2", "no5", array<string> ("no4_name", "4"),
             (double) (get_sec_time () % 1000));

  // order to sort
  // sort ascending order
  tree q1= tuple (tuple ("order", "\"no1_name\"", "#t"));
  auto a1= query (test_db, q1, (double) (get_sec_time () % 1000), 1000000);
  // sort decending order
  tree q2= tuple (tuple ("order", "\"no1_name\"", "#f"));
  auto a2= query (test_db, q2, (double) (get_sec_time () % 1000), 1000000);

  QVERIFY (a1 == array<string> ("query1", "query2"));
  QVERIFY (a2 == array<string> ("query2", "query1"));

  // modified: the modified id between t_begin and t_end
  double t2= (double) (get_sec_time () % 1000);
  while ((double) (get_sec_time () % 1000) <= t2)
    ;
  t2= (double) (get_sec_time () % 1000);

  // The end needs to be strictly greater than the modification time, while the
  // begin does not strictly less.
  string t_begin (scm_quote (as_string (t1)));
  string t_end (scm_quote (as_string (t2)));

  tree   q3= tuple (tuple ("modified", t_begin, t_end));
  auto   a3= query (test_db, q3, (double) (get_sec_time () % 1000), 1000000);
  string ans[1]= {"query2"};
  QVERIFY (a3 == array<string> (ans, 1));

  // test contains
  tree q4= tuple (tuple ("contains", "\"no1_name\""));
  auto a4= query (test_db, q4, (double) get_sec_time (), 1000000);
  tree q5= tuple (tuple ("contains", "\"no2_name\""));
  auto a5= query (test_db, q5, (double) get_sec_time (), 1000000);

  QVERIFY (a4 == array<string> ("query1", "query2"));
  string ans1[1]= {"query1"};
  QVERIFY (a5 == array<string> (ans1, 1));
}

void
TestDatabaseBasicFunciton::test_get_completions () {
  url test_db= url_temp ("db7");

  string val1[1]= {"abcd"};
  string val2[1]= {"5678"};
  string val3[1]= {"abcd5678"};

  set_field (test_db, "completion", "sample1", array<string> (val1, 1),
             (double) get_sec_time ());
  set_field (test_db, "completion", "sample2", array<string> (val2, 1),
             (double) get_sec_time ());
  set_field (test_db, "completion", "sample3", array<string> (val3, 1),
             (double) get_sec_time ());

  strings completions_for_val1            = get_completions (test_db, "abcd");
  string  expected_completions_for_val1[2]= {"abcd", "abcd5678"};
  QVERIFY (completions_for_val1 ==
           array<string> (expected_completions_for_val1, 2));

  strings completions_for_val2= get_completions (test_db, "5");
  QVERIFY (completions_for_val2 == array<string> (val2, 1));

  strings completions_for_val3= get_completions (test_db, "abcd5");
  QVERIFY (completions_for_val3 == array<string> (val3, 1));
}

QTEST_MAIN (TestDatabaseBasicFunciton)
#include "database_basic_function_test.moc"
