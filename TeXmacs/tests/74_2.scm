(define (export-as-pdf filename)
  (let ((path (string-append "$TEXMACS_PATH/tests/tm/" filename))
        (tmpfile (url-temp)))
      (load-buffer path)
      (buffer-export path tmpfile "latex")))

(define (test-export-as-latex)
  (regression-test-group
   "export large table to pdf, testing pagination" "export-pdf-pagination"
   export-as-pdf :none
   (test "test tm" "74_2.tm" #f)))

(tm-define (test_74_2)
  (let ((n (+ (test-export-as-latex))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 74_2: ok\n")))
