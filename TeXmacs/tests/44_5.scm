
(define (test-libgit2-version)
  (when (!= (libgit2-version) "1.7.1")
    (display* "Result: " (libgit2-version))
    (error "libgit2-version does not match"))
  1)

(tm-define (test_44_5)
  (let ((n (+ (test-libgit2-version))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 44_5: ok\n")))
