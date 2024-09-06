
/******************************************************************************
 * MODULE     : tm_timer.hpp
 * DESCRIPTION: timers
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TIMER_H
#define TIMER_H

#include <time.h>

#include "string.hpp"
#include "tm_ostream.hpp"

time_t get_sec_time ();
time_t get_usec_time ();
time_t raw_time ();
time_t texmacs_time ();

void bench_start (string task);
void bench_cumul (string task);
void bench_end (tm_ostream& ostream, string task);
void bench_reset (string task);
void bench_print (tm_ostream& ostream, string task);
void bench_print (tm_ostream& ostream);

#endif // defined TIMER_H
