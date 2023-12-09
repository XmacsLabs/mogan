(define (export-as-latex-and-load path)
  (with path (string-append "$TEXMACS_PATH/tests/tm/" path)
    (with tmpfile (url-temp)
      (load-buffer path)
      (buffer-export path tmpfile "latex")
      (string-load tmpfile))))
  
(define (load-latex path)
  (with path (string-append "$TEXMACS_PATH/tests/tex/" path)
    (string-replace (string-load path)  "\r\n" "\n")))

(define (test-export-as-latex)
  (regression-test-group
   "export to latex and load as string" "load as string"
   export-as-latex-and-load load-latex
   (test "test tm" "43_2.tm" "43_2.tex")))

(tm-define (test_43_2)
  (let ((n (+ (test-export-as-latex))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 43_2: ok\n")))
