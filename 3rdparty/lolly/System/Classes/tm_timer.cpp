
/******************************************************************************
 * MODULE     : timer.cpp
 * DESCRIPTION: timers
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_timer.hpp"
#include "basic.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#include "tm_ostream.hpp"

#include "tbox/tbox.h"

static hashmap<string, long> timing_level (0);
static hashmap<string, long> timing_nr (0);
static hashmap<string, long> timing_cumul (0);
static hashmap<string, long> timing_last (0);

/******************************************************************************
 * Getting the time
 ******************************************************************************/
time_t
get_sec_time () {
  tb_timeval_t tp= {0};
  tb_gettimeofday (&tp, tb_null);
  return (time_t) tp.tv_sec;
}

time_t
get_usec_time () {
  tb_timeval_t tp= {0};
  tb_gettimeofday (&tp, tb_null);
  return (time_t) tp.tv_usec;
}

time_t
raw_time () {
  tb_timeval_t tp= {0};
  tb_gettimeofday (&tp, tb_null);
  return (time_t) ((tp.tv_sec * 1000) + (tp.tv_usec / 1000));
}

static time_t start_time= raw_time ();

time_t
texmacs_time () {
  tb_timeval_t tp= {0};
  tb_gettimeofday (&tp, tb_null);
  return ((time_t) ((tp.tv_sec * 1000) + (tp.tv_usec / 1000))) - start_time;
}

/******************************************************************************
 * Routines for benchmarking
 ******************************************************************************/

void
bench_start (string task) {
  // start timer for a given type of task
  if (timing_level[task] == 0) timing_last (task)= (long) texmacs_time ();
  timing_level (task)++;
}

void
bench_cumul (string task) {
  // end timer for a given type of task, but don't reset timer
  timing_level (task)--;
  if (timing_level[task] == 0) {
    long ms= ((long) texmacs_time ()) - timing_last (task);
    timing_nr (task)++;
    timing_cumul (task)+= ms;
    timing_last->reset (task);
  }
}

void
bench_end (tm_ostream& ostream, string task) {
  // end timer for a given type of task, print result and reset timer
  bench_cumul (task);
  bench_print (ostream, task);
  bench_reset (task);
}

void
bench_reset (string task) {
  // reset timer for a given type of task
  timing_level->reset (task);
  timing_nr->reset (task);
  timing_cumul->reset (task);
  timing_last->reset (task);
}

void
bench_print (tm_ostream& ostream, string task) {
  // print timing for a given type of task
  if (DEBUG_BENCH) {
    long nr= timing_nr[task];
    ostream << "Task '" << task << "' took " << timing_cumul[task] << " ms";
    if (nr > 1) ostream << " (" << nr << " invocations)";
    ostream << LF;
  }
}

static array<string>
collect (hashmap<string, long> h) {
  array<string>    a;
  iterator<string> it= iterate (h);
  while (it->busy ())
    a << it->next ();
  merge_sort (a);
  return a;
}

void
bench_print (tm_ostream& ostream) {
  // print timings for all types of tasks
  array<string> a= collect (timing_cumul);
  int           i, n= N (a);
  for (i= 0; i < n; i++)
    bench_print (ostream, a[i]);
}
