#include "a_lolly_test.hpp"
#include "tree.hpp"

TEST_CASE ("test_is_atomic") {
  CHECK (is_atomic (tree ()));
  CHECK (is_atomic (tree (0, tree ())));
  CHECK (!is_atomic (tree (1, tree (), tree ())));
  CHECK (!is_atomic (tree (2, tree (), tree ())));
}

TEST_CASE ("test_is_compound") {
  CHECK (is_compound (tree (70, 3)));
  CHECK (is_compound (tree (1, tree ())));
  CHECK (!is_compound (tree ()));
  CHECK (!is_compound (tree (0, tree ())));
}

TEST_CASE ("test_is_bool") {
  CHECK (is_bool (tree ("true")));
  CHECK (is_bool (tree ("false")));
  CHECK (!is_bool (tree ("other")));
}

TEST_CASE ("test_is_int") {
  CHECK (!is_int (tree ()));
  CHECK (is_int (tree ("+12")));
  CHECK (is_int (tree ("-12")));
  CHECK (!is_int (tree ("-+12")));
  CHECK (!is_int (tree ("one")));
}

TEST_CASE ("test_is_func") {
  CHECK (!is_func (tree (70, 3), 5));
  CHECK (!is_func (tree (), 0));
  CHECK (is_func (tree (67, 4), 67));
}

TEST_CASE ("test_is_double") {
  CHECK (!is_double (tree ()));
  CHECK (is_double (tree ("3.15")));
  CHECK (is_double (tree ("0")));
  CHECK (is_double (tree ("0.0")));
  CHECK (is_double (tree ("3.1415926")));
  CHECK (is_double (tree ("-3.1415926")));
  CHECK (!is_double (tree ("nothing")));
}

TEST_CASE ("test_is_string") {
  CHECK (is_string (tree ()));
  CHECK (is_string (tree ("0")));
  CHECK (is_string (tree ("+121234")));
  CHECK (is_string (tree ("seigj")));
  CHECK (is_string (tree ("!@#$$!")));
  CHECK (!is_string (tree (70, 3)));
}

TEST_CASE ("test_is_generic") {
  CHECK (!is_generic (tree (9, 0)));
  CHECK (!is_generic (tree (10, -1)));
  CHECK (!is_generic (tree (11, 1234)));
  CHECK (!is_generic (tree (11, 4)));
  CHECK (!is_generic (tree (12, 2)));
  CHECK (is_generic (tree (-1, 1)));
  CHECK (is_generic (tree (-2, 1)));
  CHECK (is_generic (tree (-3, 1)));
}

TEST_CASE ("test N()") {
  CHECK (N (tree ()) == 0);
  CHECK (N (tree (0, tree ())) == 1);
  CHECK (N (tree (0, tree (), tree ())) == 2);
  CHECK (N (tree (0, tree (), tree (), tree ())) == 3);
}

TEST_CASE ("test_arity") {
  CHECK (!arity (tree (0, -1)));
  CHECK (!arity (tree (280, 0)));
  CHECK (arity (tree (9, 3)));
  CHECK (!arity (tree (0, 5)));
}

TEST_CASE ("test right_index") {
  CHECK (right_index (tree (0)) == 0);
  CHECK (right_index (tree ("string")) == 6);
  CHECK (right_index (tree (280, tree ())) == 1);
  CHECK (right_index (tree (9, tree ())) == 1);
  CHECK (right_index (tree (0, 5)) == 5);
}

TEST_CASE ("test A()") {
  CHECK (A (tree (0, 1, 2))[0] == 1);
  CHECK (A (tree (0, 1, 2))[1] == 2);
  CHECK (A (tree (0, 1, 2, 3))[0] == 1);
  CHECK (A (tree (0, 1, 2, 3))[1] == 2);
  CHECK (A (tree (0, 1, 2, 3))[2] == 3);
}

TEST_CASE ("test AR()") {
  tree a   = tree (2, tree (3), tree (4));
  AR (a)[0]= tree (5);
  CHECK (A (a)[0] == tree (5));

  CHECK (N (A (a)) == 2);
  AR (a)= append (tree (5), AR (a));
  CHECK (N (A (a)) == 3);
}

TEST_CASE ("test operator==") {
  CHECK (tree (0, 1, 2) != 1);
  CHECK (tree (0, 1, 2) != 2);
  CHECK (tree (2) == 2);
}

TEST_CASE ("test inside") {
  CHECK (inside (tree (0))->op == 0);
  CHECK (inside (tree (0, tree (), tree ()))->op == 0);
  CHECK (inside (tree (3, tree (), tree ()))->op == 3);
}

TEST_CASE ("test strong_equal") {
  auto a= tree (0, tree ());
  auto b= &a;
  CHECK (strong_equal (a, *b));
  CHECK (!strong_equal (tree (0, 1, 2), tree (3, tree ())));
  CHECK (!strong_equal (tree (0, 1, 2), tree (4, tree ())));
}

TEST_CASE ("test replace") {
  tree la= tree (1, tree (2, tree (3), tree (3)));
  tree ra= tree (1, tree (2, tree (4), tree (5)));
  tree a = tree (1, la, ra);

  tree lb= tree (1, tree (2, tree (6), tree (6)));
  tree rb= tree (1, tree (2, tree (4), tree (5)));
  tree b = tree (1, lb, rb);

  a= replace (a, tree (3), tree (6));
  CHECK (a == b);
  b= replace (b, tree (4), tree (7));
  CHECK (a != b);
  b= replace (b, tree (7), tree (4));
  CHECK (a == b);
}

TEST_CASE ("test operator=") {
  tree a= tree (3, tree (2));

  tree b= a;
  CHECK (b == tree (3, tree (2)));

  CHECK (a[0] == tree (2));
  b[0]= tree (4);
  CHECK (a[0] == tree (4));
}

TEST_CASE ("tree (const tree& x)") {
  tree a= tree (3, tree (2));

  tree b (a);
  CHECK (b == tree (3, tree (2)));

  CHECK (a[0] == tree (2));
  b[0]= tree (4);
  CHECK (a[0] == tree (4));
}

TEST_CASE ("copy") {
  tree a= tree (3, tree (2));

  tree b= copy (a);
  CHECK (b == tree (3, tree (2)));

  CHECK (a[0] == tree (2));
  b[0]= tree (4);
  CHECK (a[0] == tree (2));
}
