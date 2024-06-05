/* 0-clause BSD */

/* Adapted from repl.c in the s7 official repo */

#include <iostream>
#include <sstream>
#include <unordered_set>
#include <vector>

using std::cout;
using std::flush;
using std::string;

#include "s7.h"

#define DATA_BEGIN ((char) 2)
#define DATA_END ((char) 5)
#define DATA_ESCAPE ((char) 27)

void
data_begin () {
  cout << DATA_BEGIN;
}

void
data_end () {
  cout << DATA_END << flush;
}

void
flush_scheme (string msg) {
  data_begin ();
  cout << "scheme:" << msg;
  data_end ();
}

void
flush_verbatim (string msg) {
  data_begin ();
  cout << "verbatim:" << msg;
  data_end ();
}

void
flush_prompt (string prompt) {
  data_begin ();
  cout << "prompt#" << prompt;
  data_end ();
}

string
getBeforeSpace (const string& str) {
  size_t pos= str.find (' ');
  if (pos == string::npos) {
    return str;
  }
  return str.substr (0, pos);
}

std::string::size_type
findBegin (const std::string& s) {
  std::string::size_type pos= s.find_first_not_of (" \t\n\r\f\v");
  return (pos == std::string::npos) ? s.length () : pos;
}

std::string::size_type
findEnd (const std::string& s) {
  std::string::size_type pos= s.find_last_not_of (" \t\n\r\f\v");
  return (pos == std::string::npos) ? 0 : pos + 1;
}

std::string
trim (const std::string& s) {
  std::string::size_type left = findBegin (s);
  std::string::size_type right= findEnd (s);
  return s.substr (left, right - left);
}

int
main (int argc, char** argv) {
  std::unordered_set<std::string> scheme_headers= {
      "(document", "(math", "(equation*", "(align", "(with", "(graphics"};

  std::stringstream welcome;
  welcome << "S7 Scheme " << S7_VERSION << " (" << S7_DATE << ")\n";
  flush_verbatim (welcome.str ());
  flush_prompt ("> ");

  s7_scheme* sc;
  sc= s7_init ();
#ifdef S7_LOAD_PATH
  s7_add_to_load_path (sc, S7_LOAD_PATH);
#endif

  while (true) {
    string first_line;
    std::getline (std::cin, first_line);
    std::vector<string> lines;
    lines.push_back (first_line);
    std::stringstream ss;
    string            line= first_line;
    while (line != "<EOF>") {
      ss << line << "\n";
      line= "";
      std::getline (std::cin, line);
    }
    s7_pointer x     = s7_eval_c_string (sc, ss.str ().c_str ());
    string     result= s7_object_to_c_string (sc, x);
    if (result.size () == 0) {
      flush_verbatim ("");
    }
    else {
      string trimmed= trim (result);
      string head   = getBeforeSpace (trimmed);
      if (trimmed[trimmed.size () - 1] == ')' &&
          scheme_headers.find (head) != scheme_headers.end ()) {
        flush_scheme (trimmed);
      }
      else {
        flush_verbatim (result);
      }
    }
  }

  return 0;
}
