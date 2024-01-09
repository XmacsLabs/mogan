
(define (test-libgit2-version)
  (if (> (length (libgit2-version)) 0)
    1
    (display* "Result: " (libgit2-version))))

(tm-define (test_44_5)
  (let ((n (+ (test-libgit2-version))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 44_5: ok\n")))
