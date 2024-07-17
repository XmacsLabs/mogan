/* 0-clause BSD */

/* gcc -o repl repl.c s7.o -Wl,-export-dynamic -lm -I. -ldl */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
#include <errno.h>
#include <unistd.h>
#endif

#include "s7.h"

char* str_r7rs_define_library=
"(define-macro (define-library libname . body) ; |(lib name)| -> environment\n"
"  `(define ,(symbol (object->string libname))\n"
"     (with-let (sublet (unlet)\n"
"                         (cons 'import import)\n"
"                         (cons '*export* ())\n"
"                         (cons 'export (define-macro (,(gensym) . names)\n"
"                                         `(set! *export* (append ',names *export*)))))\n"
"       ,@body\n"
"       (apply inlet\n"
"              (map (lambda (entry)\n"
"                     (if (or (member (car entry) '(*export* export import))\n"
"                             (and (pair? *export*)\n"
"                                  (not (member (car entry) *export*))))\n"
"                         (values)\n"
"                         entry))\n"
"                   (curlet))))))\n";
char* str_r7rs_library_filename=
"(unless (defined? 'r7rs-import-library-filename)\n"
"  (define (r7rs-import-library-filename libs)\n"
"    (when (pair? libs)\n"
"      (let ((lib-filename (let loop ((lib (if (memq (caar libs) '(only except prefix rename))\n"
"                                              (cadar libs)\n"
"                                              (car libs)))\n"
"                                     (name \"\"))\n"
"                            (set! name (string-append name (symbol->string (car lib))))\n"
"                            (if (null? (cdr lib))\n"
"                                (string-append name \".scm\")\n"
"                                (begin\n"
"                                  (set! name (string-append name \"/\"))\n"
"                                  (loop (cdr lib) name))))))\n"
"        (unless (member lib-filename (*s7* 'file-names))\n"
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
"                     (symbol->value (symbol (object->string (cadr ',lib))))\n"
"                     (cddr ',lib)))\n"
"                  ((except)\n"
"                   `((lambda (e names)\n"
"                       (apply inlet\n"
"                              (map (lambda (entry)\n"
"                                     (if (member (car entry) names)\n"
"                                         (values)\n"
"                                         entry))\n"
"                                   e)))\n"
"                     (symbol->value (symbol (object->string (cadr ',lib))))\n"
"                     (cddr ',lib)))\n"
"                  ((prefix)\n"
"                   `((lambda (e prefx)\n"
"                       (apply inlet\n"
"                              (map (lambda (entry)\n"
"                                     (cons (string->symbol \n"
"                                            (string-append (symbol->string prefx) \n"
"                                                           (symbol->string (car entry)))) \n"
"                                           (cdr entry)))\n"
"                                   e)))\n"
"                     (symbol->value (symbol (object->string (cadr ',lib))))\n"
"                     (caddr ',lib)))\n"
"                  ((rename)\n"
"                   `((lambda (e names)\n"
"                       (apply inlet\n"
"                              (map (lambda (entry)\n"
"                                     (let ((info (assoc (car entry) names)))\n"
"                                       (if info\n"
"                                           (cons (cadr info) (cdr entry))\n"
"                                           entry))) \n"
"                                   e)))\n"
"                     (symbol->value (symbol (object->string (cadr ',lib))))\n"
"                     (cddr ',lib)))\n"
"                  (else\n"
"                   `(let ((sym (symbol (object->string ',lib))))\n"
"                      (if (not (defined? sym))\n"
"                          (format () \"~A not loaded~%\" sym)\n"
"                          (symbol->value sym))))))\n"
"              libs))))\n";


#ifndef _MSC_VER
static char*
realdir (s7_scheme* sc, const char* filename) {
  char* path;
  char* p;

  if (!strchr (filename, '/')) {
    if (access ("libc_s7.so", F_OK) != 0) {
      if ((access ("libc.scm", F_OK) == 0) &&
          (access ("cload.scm", F_OK) == 0)) {
        s7_load (sc, "cload.scm");
        s7_load (sc, "libc.scm");
        return (NULL);
      }
      fprintf (stderr,
               "%s needs libc_s7.so (give the explicit repl pathname or build "
               "it by running: repl libc.scm)\n",
               filename); /* env PATH=/home/bil/cl repl */
      exit (2);
    }
    return (
        NULL); /* we're in the libc_s7.so directory, I hope (user could start a
                  version of s7 that does not match the local libc_s7.so...) */
  }
  if (!(path= realpath (filename, NULL))) {
    fprintf (stderr, "%s: %s\n", strerror (errno), filename);
    exit (2);
  }
  if (!(p= strrchr (path, '/'))) {
    free (path);
    fprintf (stderr, "please provide the full pathname for %s\n", filename);
    exit (2);
  }
  if (p > path) *p= '\0';
  else p[1]= 0;
  return (path);
}
#endif

int
main (int argc, char** argv) {
  s7_scheme* sc;

  sc= s7_init ();
  s7_eval_c_string (sc, str_r7rs_define_library);
  s7_eval_c_string (sc, str_r7rs_library_filename);
  s7_eval_c_string (sc, str_r7rs_import);
  if (argc >= 2) {
    if (strcmp (argv[1], "-e") == 0) /* repl -e '(+ 1 2)' */
    {
      s7_pointer x;
      x= s7_eval_c_string (sc, argv[2]);
      fprintf (stdout, "%s\n", s7_object_to_c_string (sc, x));
      return (0);
    }
    if (strcmp (argv[1], "--version") == 0) {
      fprintf (stdout, "s7: %s, %s\n", S7_VERSION, S7_DATE);
      return (0);
    }
    fprintf (stderr, "load %s\n", argv[1]); /* repl test.scm */
    errno= 0;
    if (!s7_load (sc, argv[1])) {
      fprintf (stderr, "%s: %s\n", strerror (errno), argv[1]);
      return (2);
    }
  }
  return (0);
}
