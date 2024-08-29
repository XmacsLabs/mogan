
/******************************************************************************
 * MODULE     : basic.cpp
 * DESCRIPTION: fast global new and delete
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "basic.hpp"
#include "analyze.hpp"
#include "string.hpp"

int
hash (pointer ptr) {
  return ((int) ((intptr_t) ptr)) + (((int) ((intptr_t) ptr)) % 19);
}

/******************************************************************************
 * debugging
 ******************************************************************************/

static long int debug_status= 0;

bool
debug (int which, bool write_flag) {
  if (write_flag) {
    debug_status= debug_status | (((long int) 1) << which);
    return 0;
  }
  else return (debug_status & (((long int) 1) << which)) > 0;
}

int
debug_off () {
  int status  = debug_status;
  debug_status= 0;
  return status;
}

void
debug_on (int status) {
  debug_status= status;
}

static void
debug_set (int which, bool on) {
  if (on) debug_status= debug_status | (((long int) 1) << which);
  else debug_status= debug_status & (~(((long int) 1) << which));
}

void
debug_set (string s, bool on) {
  if (s == "auto") debug_set (DEBUG_FLAG_AUTO, on);
  else if (s == "verbose") debug_set (DEBUG_FLAG_VERBOSE, on);
  else if (s == "events") debug_set (DEBUG_FLAG_EVENTS, on);
  else if (s == "std") debug_set (DEBUG_FLAG_STD, on);
  else if (s == "io") debug_set (DEBUG_FLAG_IO, on);
  else if (s == "bench") debug_set (DEBUG_FLAG_BENCH, on);
  else if (s == "history") debug_set (DEBUG_FLAG_HISTORY, on);
  else if (s == "qt") debug_set (DEBUG_FLAG_QT, on);
  else if (s == "qt-widgets") debug_set (DEBUG_FLAG_QT_WIDGETS, on);
  else if (s == "keyboard") debug_set (DEBUG_FLAG_KEYBOARD, on);
  else if (s == "packrat") debug_set (DEBUG_FLAG_PACKRAT, on);
  else if (s == "flatten") debug_set (DEBUG_FLAG_FLATTEN, on);
  else if (s == "parser") debug_set (DEBUG_FLAG_PARSER, on);
  else if (s == "correct") debug_set (DEBUG_FLAG_CORRECT, on);
  else if (s == "convert") debug_set (DEBUG_FLAG_CONVERT, on);
  else if (s == "remote") debug_set (DEBUG_FLAG_REMOTE, on);
  else if (s == "live") debug_set (DEBUG_FLAG_LIVE, on);
}

static bool
debug_get (int which) {
  return (debug_status & (((long int) 1) << which)) != 0;
}

bool
debug_get (string s) {
  if (s == "auto") return debug_get (DEBUG_FLAG_AUTO);
  else if (s == "verbose") return debug_get (DEBUG_FLAG_VERBOSE);
  else if (s == "events") return debug_get (DEBUG_FLAG_EVENTS);
  else if (s == "std") return debug_get (DEBUG_FLAG_STD);
  else if (s == "io") return debug_get (DEBUG_FLAG_IO);
  else if (s == "bench") return debug_get (DEBUG_FLAG_BENCH);
  else if (s == "history") return debug_get (DEBUG_FLAG_HISTORY);
  else if (s == "qt") return debug_get (DEBUG_FLAG_QT);
  else if (s == "qt-widgets") return debug_get (DEBUG_FLAG_QT_WIDGETS);
  else if (s == "keyboard") return debug_get (DEBUG_FLAG_KEYBOARD);
  else if (s == "packrat") return debug_get (DEBUG_FLAG_PACKRAT);
  else if (s == "flatten") return debug_get (DEBUG_FLAG_FLATTEN);
  else if (s == "correct") return debug_get (DEBUG_FLAG_CORRECT);
  else if (s == "convert") return debug_get (DEBUG_FLAG_CONVERT);
  else if (s == "remote") return debug_get (DEBUG_FLAG_REMOTE);
  else if (s == "live") return debug_get (DEBUG_FLAG_LIVE);
  else return false;
}

#ifdef USE_EXCEPTIONS
string the_exception;
string the_report;
// string get_crash_report (const char* msg);

void
tm_throw (const char* msg) {
  the_exception= msg;
  // the_report   = get_crash_report (msg);
  cout << "Throwing " << msg << LF;
  cout << "-------------------------------------------------\n";
  cout << the_report << LF;
  cout << "-------------------------------------------------\n";
  throw string (msg);
}

void
handle_exceptions () {
  if (N (the_exception) != 0) {
    // formatted arg (verbatim_to_tree (the_report, false, "utf-8"));
    // failed_error << "Exception, " << the_exception << arg << LF;
    the_exception= "";
    the_report   = "";
  }
}
#endif

/******************************************************************************
 * miscellaneous routines
 ******************************************************************************/

int
new_type_identifier () {
  static int id= 0;
  id--;
  return id;
}

static int current_indent= 0;

tm_ostream&
operator<< (tm_ostream& out, display_control ctrl) {
  int i;
  switch (ctrl) {
  case INDENT:
    out << "  ";
    current_indent+= 2;
    break;
  case UNINDENT:
    out << "\b\b";
    current_indent-= 2;
    break;
  case HRULE:
    for (i= current_indent; i < 78; i++)
      out << "-";
  case LF:
    out << "\n";
    for (i= 0; i < current_indent; i++)
      out << " ";
    break;
  }
  return out;
}
