#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest/doctest.h"
#include "generic_tree.hpp"
#include "tree.hpp"

TEST_CASE ("test as") {
  CHECK_EQ (as<double, int> (1), 1.0);
  CHECK_EQ (as<int, double> (1.0), 1);
  CHECK_EQ (as<int, double> (1.5), 1);
}
