(define (test-string-alpha?)
  (regression-test-group
     "string-alpha?" "bool"
     string-alpha? :none
     (test "string-alpha? of a is true" "a" #t)
     (test "string-alpha? of b is true" "b" #t)
     (test "string-alpha? of Z is true" "Z" #t)
     (test "string-alpha? of ? is false" "?" #f)
     (test "string-alpha? of empty string is false" "" #f)))

(define (test-string-occurs?)
  (define (test-string-1 x)
    (string-occurs? x "This is a love letter to S7 scheme."))
  (regression-test-group
   "string_occurs case 1" "bool"
   test-string-1 :none
   (test "leading word" "This" #t)
   (test "word in middle" "S7" #t)
   (test "string contains space" "ve l" #t)
   (test "should not contain" "notcontain" #f)
   (test "empty string" "" #t)))

(tm-define (test_15_3_3)
  (let ((n (+ (test-string-alpha?)
              (test-string-occurs?))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 15_3_3: ok\n")))
