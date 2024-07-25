/* 0-clause BSD */

/* gcc -o repl repl.c goldfish.o -Wl,-export-dynamic -lm -I. -ldl */

#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
#include <errno.h>
#include <unistd.h>
#endif

#include "s7.h"

char* str_r7rs_define_library=
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
char* str_r7rs_library_filename=
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
    "        (unless (member lib-filename (*goldfish* 'file-names))\n"
    "          (load lib-filename)))\n"
    "      (r7rs-import-library-filename (cdr libs)))))\n";
char* str_r7rs_import=
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

int
main (int argc, char** argv) {
  goldfish_scheme* sc;
  sc= goldfish_init ();

  goldfish_eval_c_string (sc, str_r7rs_define_library);
  goldfish_eval_c_string (sc, str_r7rs_library_filename);
  goldfish_eval_c_string (sc, str_r7rs_import);
  if (argc >= 2) {
    if (strcmp (argv[1], "-e") == 0) /* repl -e '(+ 1 2)' */
    {
      goldfish_pointer x;
      x= goldfish_eval_c_string (sc, argv[2]);
      fprintf (stdout, "%s\n", goldfish_object_to_c_string (sc, x));
      return (0);
    }
    if (strcmp (argv[1], "--version") == 0) {
      fprintf (stdout, "goldfish: %s, %s\n", S7_VERSION, S7_DATE);
      return (0);
    }
    errno= 0;
    if (strcmp (argv[1], "--texmacs") == 0) {
      const char*       env_key  = "TEXMACS_PATH";
      const char*       env_value= getenv (env_key);
      std::stringstream load_path;
      load_path << env_value << "/plugins/goldfish/s7/";
      goldfish_add_to_load_path (sc, load_path.str ().c_str ());
      if (!goldfish_load (sc, argv[2])) {
        fprintf (stderr, "%s: %s\n", strerror (errno), argv[2]);
        return (2);
      }
    }
    else {
      if (!goldfish_load (sc, argv[1])) {
        fprintf (stderr, "%s: %s\n", strerror (errno), argv[1]);
        return (2);
      }
    }
  }
  return (0);
}
