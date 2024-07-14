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

string str_r7rs_define_library=
    "(define-macro (define-library libname . body) ; |(lib name)| -> "
    "environment\n"
    "  `(define ,(symbol (object->string libname))\n"
    "     (with-let (sublet (unlet)\n"
    "                         (cons 'import import)\n"
    "                         (cons '*export* ())\n"
    "                         (cons 'export (define-macro (,(gensym) . names)\n"
    "                                         `(set! *export* (append ',names "
    "*export*)))))\n"
    "       ,@body\n"
    "       (apply inlet\n"
    "              (map (lambda (entry)\n"
    "                     (if (or (member (car entry) '(*export* export "
    "import))\n"
    "                             (and (pair? *export*)\n"
    "                                  (not (member (car entry) *export*))))\n"
    "                         (values)\n"
    "                         entry))\n"
    "                   (curlet))))))\n";
string str_r7rs_library_filename=
    "(unless (defined? 'r7rs-import-library-filename)\n"
    "  (define (r7rs-import-library-filename libs)\n"
    "    (when (pair? libs)\n"
    "      (let ((lib-filename (let loop ((lib (if (memq (caar libs) '(only "
    "except prefix rename))\n"
    "                                              (cadar libs)\n"
    "                                              (car libs)))\n"
    "                                     (name \"\"))\n"
    "                            (set! name (string-append name "
    "(symbol->string (car lib))))\n"
    "                            (if (null? (cdr lib))\n"
    "                                (string-append name \".scm\")\n"
    "                                (begin\n"
    "                                  (set! name (string-append name \"/\"))\n"
    "                                  (loop (cdr lib) name))))))\n"
    "        (unless (member lib-filename (*s7* 'file-names))\n"
    "          (load lib-filename)))\n"
    "      (r7rs-import-library-filename (cdr libs)))))\n";
string str_r7rs_import=
    "(define-macro (import . libs)\n"
    "  `(begin\n"
    "     (r7rs-import-library-filename ',libs)\n"
    "     (varlet (curlet)\n"
    "       ,@(map (lambda (lib)\n"
    "                (case (car lib)\n"
    "                  ((only)\n"
    "                   `((lambda (e names)\n"
    "                       (apply inlet\n"
    "                              (map (lambda (name)\n"
    "                                     (cons name (e name)))\n"
    "                                   names)))\n"
    "                     (symbol->value (symbol (object->string (cadr "
    "',lib))))\n"
    "                     (cddr ',lib)))\n"
    "                  ((except)\n"
    "                   `((lambda (e names)\n"
    "                       (apply inlet\n"
    "                              (map (lambda (entry)\n"
    "                                     (if (member (car entry) names)\n"
    "                                         (values)\n"
    "                                         entry))\n"
    "                                   e)))\n"
    "                     (symbol->value (symbol (object->string (cadr "
    "',lib))))\n"
    "                     (cddr ',lib)))\n"
    "                  ((prefix)\n"
    "                   `((lambda (e prefx)\n"
    "                       (apply inlet\n"
    "                              (map (lambda (entry)\n"
    "                                     (cons (string->symbol \n"
    "                                            (string-append "
    "(symbol->string prefx) \n"
    "                                                           "
    "(symbol->string (car entry)))) \n"
    "                                           (cdr entry)))\n"
    "                                   e)))\n"
    "                     (symbol->value (symbol (object->string (cadr "
    "',lib))))\n"
    "                     (caddr ',lib)))\n"
    "                  ((rename)\n"
    "                   `((lambda (e names)\n"
    "                       (apply inlet\n"
    "                              (map (lambda (entry)\n"
    "                                     (let ((info (assoc (car entry) "
    "names)))\n"
    "                                       (if info\n"
    "                                           (cons (cadr info) (cdr "
    "entry))\n"
    "                                           entry))) \n"
    "                                   e)))\n"
    "                     (symbol->value (symbol (object->string (cadr "
    "',lib))))\n"
    "                     (cddr ',lib)))\n"
    "                  (else\n"
    "                   `(let ((sym (symbol (object->string ',lib))))\n"
    "                      (if (not (defined? sym))\n"
    "                          (format () \"~A not loaded~%\" sym)\n"
    "                          (symbol->value sym))))))\n"
    "              libs))))\n";

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

  const char*       env_key  = "TEXMACS_PATH";
  const char*       env_value= getenv (env_key);
  std::stringstream load_path;
  load_path << env_value << "/plugins/s7/";

  s7_scheme* sc;
  sc= s7_init ();
  s7_eval_c_string (sc, str_r7rs_define_library.c_str ());
  s7_eval_c_string (sc, str_r7rs_library_filename.c_str ());
  s7_eval_c_string (sc, str_r7rs_import.c_str ());

  s7_add_to_load_path (sc, load_path.str ().c_str ());

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
