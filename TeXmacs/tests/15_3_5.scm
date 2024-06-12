(define (test-url-none?)
  (regression-test-group
   "url-none" "bool"
   url-none? :none
   (test "(url-none? (url-none)) is true" (url-none) #t)
   (test "url-none? of /tmp is false" (system->url "/tmp") #f)))

(tm-define (test_15_3_5)
  (let ((n (+ (test-url-none?))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 15_3_5: ok\n")))
