
/******************************************************************************
 * MODULE     : ispell.cpp
 * DESCRIPTION: interface with the ispell spell checker
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Ispell/ispell.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "file.hpp"
#include "locale.hpp"
#include "resource.hpp"
#include "scheme.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tm_link.hpp"

#include <moebius/tree_label.hpp>

using namespace moebius;

static url
find_binary_aspell () {
  eval ("(use-modules (binary aspell))");
  return as_url (eval ("(find-binary-aspell)"));
}

static url
find_binary_hunspell () {
  eval ("(use-modules (binary hunspell))");
  return as_url (eval ("(find-binary-hunspell)"));
}

string ispell_encode (string lan, string s);
string ispell_decode (string lan, string s);

/******************************************************************************
 * The connection resource
 ******************************************************************************/

RESOURCE (ispeller);
struct ispeller_rep : rep<ispeller> {
  string  lan; // name of the session
  tm_link ln;  // the pipe
  bool    unavailable;

public:
  ispeller_rep (string lan);
  string start ();
  string retrieve ();
  void   send (string cmd);

private:
  bool connect_spellchecker (string cmd);
};
RESOURCE_CODE (ispeller);

/******************************************************************************
 * Routines for ispellers
 ******************************************************************************/

ispeller_rep::ispeller_rep (string lan2) : rep<ispeller> (lan2), lan (lan2) {}

// connect to spell checker with the desired dictionnary
string
ispeller_rep::start () {
  if (!is_nil (ln)) {
    if (ln->alive) return "ok";
    if (unavailable) return "Error: not available";
  }
  string cmd, err;
  string name   = "";
  string locale = language_to_locale (lan);
  bool   testdic= false;

  // Try hunspell first
  url binary_hunspell= find_binary_hunspell ();
  if (!is_none (binary_hunspell)) {
    cmd= sys_concretize (binary_hunspell);
    cmd << " -a -i utf-8";
    name= "Hunspell";
    if (!is_empty (locale)) {
      cmd << " -d " << locale;
    }
    testdic= connect_spellchecker (cmd);
  }

  // And then try aspell
  if (is_empty (name) || (!testdic)) {
    url binary_aspell= find_binary_aspell ();
    if (!is_none (binary_aspell)) {
      cmd = sys_concretize (binary_aspell);
      name= "Aspell";
      cmd = cmd * " -a --encoding=utf-8";
      if (!is_empty (locale)) cmd= cmd * " --language-tag=" * locale;
      testdic= connect_spellchecker (cmd);
    }
  }

  if (is_empty (name)) {
    err= "Error: spellchecker not found in PATH (neither Aspell nor Hunspell) ";
    std_error << err << "\nCannot spellcheck\n";
    unavailable= true;
    return err;
  }
  if (!testdic) {
    err= "Error: no dictionary installed for " * lan * " (" * locale * ")";
    std_error << err << "\nThe corresponding text is not checked\n";
    unavailable= true;
    return err;
  }
  debug_spell << "running " << name << " with " << locale << " dictionary for "
              << lan << "\n";
  unavailable= false;
  return "ok";
}

bool
ispeller_rep::connect_spellchecker (string cmd) {
  // establishes connection (absence of error means the required dictionnary is
  // available) returns true when the spellchecker is up and running
  ln            = make_pipe_link (cmd);
  string message= ln->start ();
  if (DEBUG_IO) debug_spell << "Received " << message << "\n";
  if (starts (message, "Error: ")) {
    if (ln->alive) ln->stop ();
    return false;
  }
  message= retrieve ();
  if (DEBUG_IO) debug_spell << "Received " << message << "\n";
  if (starts (message, "@(#)")) return true;
  else {
    if (ln->alive) ln->stop ();
    return false;
  }
}

string
ispeller_rep::retrieve () {
  string ret;
#if defined(OS_MINGW) || defined(OS_WIN)
  while ((ret != "\r\n") && (!ends (ret, "\r\n\r\n")) &&
         ((!ends (ret, "\r\n")) || (!starts (ret, "@(#)"))))
#else
  while ((ret != "\n") && (!ends (ret, "\n\n")) &&
         ((!ends (ret, "\n")) || (!starts (ret, "@(#)"))))
#endif
  {
    ln->listen (10000);
    string mess = ln->read (LINK_ERR);
    string extra= ln->read (LINK_OUT);
    if (mess != "") io_error << "Spellchecker error: " << mess << "\n";
    if (extra == "") {
      ln->stop ();
      return "Error: spellchecker does not respond";
    }
    ret << extra;
  }
  return ispell_decode (lan, ret);
}

void
ispeller_rep::send (string cmd) {
  ln->write (ispell_encode (lan, cmd) * "\n", LINK_IN);
}

/******************************************************************************
 * Internationalization
 ******************************************************************************/

string
ispell_encode (string lan, string s) {
  (void) lan;
  return cork_to_utf8 (s);
}

string
ispell_decode (string lan, string s) {
  (void) lan;
  return utf8_to_cork (s);
}

/******************************************************************************
 * Subroutines
 ******************************************************************************/

static tree
parse_ispell (string s) {
#if defined(OS_MINGW) || defined(OS_WIN)
  while (ends (s, "\r\n"))
    s= s (0, N (s) - 2);
#else
  while (ends (s, "\n"))
    s= s (0, N (s) - 1);
#endif
  bool flag= true;
  int  i, j;
  tree t (TUPLE);
  for (i= 0, j= 0; j < N (s); j++)
    if (s[j] == ':') flag= false;
    else if (((s[j] == ' ') && (flag || (j == i) || (s[j - 1] == ':'))) ||
             (s[j] == ',')) {
      if (j > i) t << s (i, j);
      i= j + 1;
    }
  t << s (i, j);

  if (N (t) == 0) return tree (TUPLE, "0");
  if ((t[0] == "+") || (t[0] == "*") || (t[0] == "-")) return "ok";
  if ((N (t) >= 4) && ((t[0] == "&") || (t[0] == "?"))) {
    tree u (TUPLE, t[2]);
    u << A (t (4, N (t)));
    return u;
  }
  return tree (TUPLE, "0");
}

static void
ispell_send (string lan, string s) {
  ispeller sc= ispeller (lan);
  if (!is_nil (sc) && !is_nil (sc->ln) && sc->ln->alive) {
    sc->send (s);
  }
}

static string
ispell_eval (string lan, string s) {
  ispeller sc= ispeller (lan);
  if (!is_nil (sc) && !is_nil (sc->ln) && sc->ln->alive) {
    sc->send (s);
    return sc->retrieve ();
  }
  return "";
}

/******************************************************************************
 * Spell checking interface
 ******************************************************************************/

string
ispell_start (string lan) {
  if (DEBUG_IO) debug_spell << "Start " << lan << "\n";
  ispeller sc= ispeller (lan);
  if (is_nil (sc)) sc= tm_new<ispeller_rep> (lan);
  return sc->start ();
}

tree
ispell_check (string lan, string s) {
  if (DEBUG_IO) debug_spell << "Check " << s << "\n";
  ispeller sc= ispeller (lan);
  if (is_nil (sc) || (!sc->ln->alive)) {
    string message= ispell_start (lan);
    if (starts (message, "Error: ")) return message;
  }
  if (sc->unavailable) return "Error: unavailable";
  string ret_s= ispell_eval (lan, "^" * s);
  if (starts (ret_s, "Error: ")) return ret_s;
  return parse_ispell (ret_s);
}

void
ispell_accept (string lan, string s) {
  if (DEBUG_IO) debug_spell << "Accept " << s << "\n";
  ispell_send (lan, "@" * s);
}

void
ispell_insert (string lan, string s) {
  if (DEBUG_IO) debug_spell << "Insert " << s << "\n";
  ispell_send (lan, "*" * s);
}

void
ispell_done (string lan) {
  if (DEBUG_IO) debug_spell << "End " << lan << "\n";
  ispell_send (lan, "#");
}
