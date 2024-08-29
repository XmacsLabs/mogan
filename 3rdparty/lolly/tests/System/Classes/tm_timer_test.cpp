/** \file tm_timer_test.cpp
 *  \copyright GPLv3
 *  \details A unitest for tm_timer.
 *  \author Paradisuman
 *  \date   2023
 */
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest/doctest.h"
#include "tm_timer.hpp"

#include <cstring>
#include <iostream>
#include <sstream>
#include <string>

int
get_timing_cumul (string rep) {
  auto              out= as_charp (rep);
  std::stringstream ss (out);
  std::string       token;

  while (ss >> token) {
    if (std::isdigit (token[0]) ||
        (token[0] == '-' && std::isdigit (token[1]))) {
      return std::stoi (token);
    }
  }
}

int
get_timing_nr (string rep) {
  auto              out= as_charp (rep);
  std::stringstream ss (out);
  std::string       token;

  while (ss >> token) {
    if (token[0] == '(') {
      std::string token1= token.substr (1);
      return std::stoi (token1);
    }
  }
  return -1;
}

bool
test_same (tm_ostream& a, string b) {
  string sa= a.unbuffer ();

  return sa == b;
}

string
to_zero (tm_ostream& out) {
  string s1     = out.unbuffer ();
  char*  ans    = as_charp (s1);
  char*  current= ans;
  while ((current= strstr (current, "took"))) {
    current+= strlen ("took");
    while (*current && (*current == '(')) {
      current+= 2;
    }
    while (*current && (*current < '0' || *current > '9')) {
      current++;
    }
    while (*current && *current >= '0' && *current <= '9') {
      *current= '0';
      current++;
    }
  }
  return ans;
}

TEST_CASE ("function raw_time") {
  time_t startTime= raw_time ();

  // CHECK (startTime >= 0);

  time_t endTime= raw_time ();

  CHECK (endTime >= startTime);
}

TEST_CASE ("function texmacs_time") {
  long t1= texmacs_time ();

  CHECK (t1 >= 0);

  long t2= texmacs_time ();

  CHECK (t2 >= t1);
}

TEST_CASE ("function bench_start and bench_cumul") {
  debug_set ("bench", true);
  tm_ostream ostream;
  ostream.buffer ();

  SUBCASE ("start once") {
    bench_start ("task1");
    bench_print (ostream, "task1");
    string out= ostream.unbuffer ();
    int    t1 = get_timing_cumul (out);

    CHECK (t1 == 0);

    bench_cumul ("task1");
    ostream.buffer ();
    bench_print (ostream, "task1");
    out   = ostream.unbuffer ();
    int t2= get_timing_cumul (out);

    CHECK (t2 >= 0);
  }

  SUBCASE ("start multiple times") {
    bench_start ("task2");

    bench_cumul ("task2");
    bench_print (ostream, "task1");
    string out= ostream.unbuffer ();
    int    t1 = get_timing_cumul (out);

    CHECK (t1 >= 0);
    CHECK (get_timing_nr (out) == -1);

    bench_start ("task2");

    bench_cumul ("task2");
    ostream.buffer ();
    bench_print (ostream, "task2");
    out   = ostream.unbuffer ();
    int t2= get_timing_cumul (out);
    int nr= get_timing_nr (out);

    CHECK (t2 >= 0);
    CHECK (nr == 2);
  }
}

TEST_CASE ("function bench_end") {
  debug_set ("bench", true);
  tm_ostream ostream;
  ostream.buffer ();

  bench_start ("task");
  bench_end (ostream, "task");
  string out= ostream.unbuffer ();

  CHECK (get_timing_nr (out) == -1);

  bench_start ("task");
  ostream.buffer ();
  bench_end (ostream, "task");
  out= ostream.unbuffer ();

  CHECK (get_timing_nr (out) == -1);
}

TEST_CASE ("function bench_reset") {
  debug_set ("bench", true);
  tm_ostream ostream;
  ostream.buffer ();

  SUBCASE ("if task empty") {
    bench_reset ("task");
    bench_print (ostream, "task");
    string out= ostream.unbuffer ();

    CHECK (get_timing_nr (out) == -1);
  }

  SUBCASE ("if task not empty") {
    bench_start ("task");
    bench_cumul ("task");
    bench_start ("task");
    bench_cumul ("task");

    bench_reset ("task");
    bench_print (ostream, "task");
    string out= ostream.unbuffer ();

    CHECK (get_timing_nr (out) == -1);
  }
}

TEST_CASE ("function bench_print") {
  debug_set ("bench", true);
  tm_ostream ostream;
  ostream.buffer ();

  SUBCASE ("print one task") {
    bench_print (ostream, "task");
    string b= "Task 'task' took 0 ms\n";

    CHECK (test_same (ostream, b));

    ostream.buffer ();
    bench_reset ("task1");
    bench_start ("task1");
    bench_cumul ("task1");
    bench_start ("task1");
    bench_cumul ("task1");
    bench_print (ostream, "task1");

    string ans= "Task 'task1' took 0 ms (2 invocations)\n";
    string out= to_zero (ostream);

    CHECK (out == ans);
  }

  SUBCASE ("print multiple task") {
    bench_reset ("task1");
    bench_reset ("task2");

    bench_start ("task1");
    bench_cumul ("task1");
    bench_start ("task2");
    bench_cumul ("task2");
    bench_print (ostream);

    string out= to_zero (ostream);
    string b  = "Task 'task1' took 0 ms\nTask 'task2' took 0 ms\n";

    CHECK (out == b);
  }
}
