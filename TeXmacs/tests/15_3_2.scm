(define (test-tree-atomic?)
  (regression-test-group
   "tree-atomic?" "bool"
   tree-atomic? :none
   (test "The result of string->tree is atomic" (string->tree "a string") #t)))

(tm-define (test_15_3_2)
  (let ((n (+ (test-tree-atomic?))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 15_3_2: ok\n")))
