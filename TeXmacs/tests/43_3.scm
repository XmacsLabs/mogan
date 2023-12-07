(define (export-as-latex-and-load path)
  (with tmpfile (url-temp)
    (load-buffer path)
    (buffer-export path tmpfile "latex")
    (string-load tmpfile)))

(define (test-export-empty-doc-as-latex)
  (regression-test-group
   "export to latex and load as string" "load as string"
   export-as-latex-and-load string-load
   (test "tm file with only a" "$TEXMACS_PATH/tests/43_3_a.tm" "$TEXMACS_PATH/tests/43_3_a.tex")))

(define (test-export-a-doc-as-latex)
        1)

(tm-define (test_43_3)
  (let ((n (+ (test-export-empty-doc-as-latex)
              (test-export-a-doc-as-latex))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 9_1: ok\n")))
