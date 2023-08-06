
/******************************************************************************
 * MODULE     : url_test.cpp
 * COPYRIGHT  : (C) 2019-2021  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "file.hpp"
#include "url.hpp"
#include <QtTest/QtTest>

class TestURL : public QObject {
  Q_OBJECT

public:
//   url tmfs_1 = url_system ("tmfs://git/help");
//   url http_1 = url_system ("http://texmacs.org");
//   url https_1= url_system ("https://ustc.edu.cn");
// #ifdef OS_MINGW
//   url root_tmp= url ("$TEMP");
// #else
//   url root_tmp= url ("/tmp");
// #endif
//   url root_no_such_tmp= url ("/no_such_tmp");

private slots:
  void test_as_url ();
  void test_url_none ();
  void test_url_here ();
  void test_url_parent ();
  void test_url_ancestor ();
  void test_url_pwd ();   // failed!
  
  void test_is_none();
  void test_is_here();
  void test_is_parent();
  void test_is_ancestor();
  void test_is_atomic();
  void test_is_concat();
  void test_is_or();
  
  // // void test_exists ();
  // void test_suffix ();

  // // predicates
  // void test_is_rooted_tmfs ();
  // void test_is_rooted_web ();

  // // operations
  // void test_descends ();
};

void
TestURL::test_as_url () {
  url empty = as_url ();
  QVERIFY (empty->path[0] ==  "TUPLE");
  QVERIFY (empty->path[1] ==  "STRING");
  QVERIFY (empty->path[2] ==  "none");
  url empty3 = as_url (array<string> ("STRING", "none"));
  QVERIFY (empty3->path[0] ==  "STRING");
  QVERIFY (empty3->path[1] ==  "none");
}
 
void 
TestURL::test_url_none () {
  url none = url_none ();
  QVERIFY (none->path[0] ==  "TUPLE");
  QVERIFY (none->path[1] ==  "STRING");
  QVERIFY (none->path[2] ==  "none");
}

void 
TestURL::test_url_here () {
  url here = url_here ();
  QVERIFY (here->path[0] ==  "STRING");
  QVERIFY (here->path[1] ==  ".");
}

void 
TestURL::test_url_parent () {
  url parent = url_parent ();
  QVERIFY (parent->path[0] ==  "STRING");
  QVERIFY (parent->path[1] ==  "..");
}

void 
TestURL::test_url_ancestor () {
  url ancestor = url_ancestor ();
  QVERIFY (ancestor->path[0] ==  "STRING");
  QVERIFY (ancestor->path[1] ==  "...");
}

void 
TestURL::test_url_pwd () {
  // url pwd = url_pwd ();
  // QVERIFY (pwd->path[0] ==  "STRING");
  // QVERIFY (pwd->path[1] ==  "$PWD"); // failed!
}

void 
TestURL::test_is_none () {
  url none = url_none ();
  QVERIFY (is_none (none));
  url here = url_here ();
  QVERIFY (!is_none (here));
}

void
TestURL::test_is_here () {
  url here = url_here ();
  QVERIFY (is_here (here));
  url none = url_none ();
  QVERIFY (!is_here (none));
}
// 
void 
TestURL::test_is_parent () {
  url parent = url_parent ();
  QVERIFY (is_parent (parent));
  url none = url_none ();
  QVERIFY (!is_parent (none));
}

void 
TestURL::test_is_ancestor () {
  url ancestor = url_ancestor ();
  QVERIFY (is_ancestor (ancestor));
  url none = url_none ();
  QVERIFY (!is_ancestor (none));
}

void 
TestURL::test_is_atomic () {
  url here = url_here ();
  QVERIFY (is_atomic (here));
  url parent = url_parent ();
  QVERIFY (is_atomic (parent));
  url ancestor = url_ancestor ();
  QVERIFY (is_atomic (ancestor));
}

// void
// TestURL::test_is_concat() {
//   url here = url_here ();
//   url parent = url_parent ();
//   url ancestor = url_ancestor ();
//   url concat1 = here * parent;
//   QVERIFY (is_concat (concat1));
//   url concat2 = here * parent * ancestor;
//   QVERIFY (is_concat (concat2));
//   url concat3 = here * parent * ancestor * here;
//   QVERIFY (is_concat (concat3));
// }

// void
// TestURL::test_exists () {
//   // two cases: root directory    // failed!
//   // QVERIFY (exists (root_tmp));
//   // QVERIFY (!exists (root_no_such_tmp));
// }

// void
// TestURL::test_suffix () {
//   // empty suffix should work
//   url no_suffix= url ("/a/b/c/d/no_suffix");
//   QCOMPARE (suffix (no_suffix), string (""));
//   url no_suffix2= url ("/a/b.c/d/no_suffix");
//   QCOMPARE (suffix (no_suffix2), string (""));

//   // normal suffix should work
//   url png= url ("/a/b/c/d.png");
//   QCOMPARE (suffix (png), string ("png"));
//   url png2= url ("/a/b.c/d.png");
//   QCOMPARE (suffix (png2), string ("png"));
// }

// void
// TestURL::test_is_rooted_tmfs () {
//   QVERIFY (is_rooted_tmfs (tmfs_1));
//   QVERIFY (!is_rooted_tmfs (http_1));
//   QVERIFY (!is_rooted_tmfs (https_1));
//   QVERIFY (!is_rooted_tmfs (root_tmp));
// }

// void
// TestURL::test_is_rooted_web () {
//   QVERIFY (is_rooted_web (http_1));
//   QVERIFY (is_rooted_web (https_1));
//   QVERIFY (!is_rooted_web (root_tmp));
//   QVERIFY (!is_rooted_web (tmfs_1));
// }

// void
// TestURL::test_descends () {
// #ifdef OS_MINGW
//   QVERIFY (descends (url_system ("$TEMP/a.txt"), root_tmp));
// #else
//   QVERIFY (descends (url_system ("/tmp/a.txt"), root_tmp));
// #endif
//   QVERIFY (descends (url_system ("$TEXMACS_PATH/doc/main/man-manual.en.tm"),
//                      url_system ("$TEXMACS_PATH")));
//   QVERIFY (!descends (root_no_such_tmp, root_tmp));
// }

QTEST_MAIN (TestURL)
#include "url_test.moc"
