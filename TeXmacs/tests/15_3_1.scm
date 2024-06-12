(define (test-cpp-string-number?)
  (regression-test-group
   "cpp-string-number?" "bool"
   cpp-string-number? :none
   (test "normal floating number" "1.1" #t)
   (test "integer" "1" #t)
   (test "empty string" "" #f)))

(tm-define (test_15_3_1)
  (let ((n (+ (test-cpp-string-number?))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 15_3_1: ok\n")))
