(use-modules (data scheme))

(define (test-url-format)
  (regression-test-group
   "url-format" "format name"
   url-format :none
   (test "format of init-texmacs.scm"
    "$TEXMACS_PATH/progs/init-texmacs.scm"
    "scheme")))

(tm-define (test_15_3_7)
  (let ((n (+ (test-url-format))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 15_3_7: ok\n")))
