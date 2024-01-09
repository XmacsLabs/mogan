
(define (test-show-license)
  (when (!= (git-load-blob "HEAD" "LICENSE" (url-pwd))
            (string-load (url-append (url-pwd) "LICENSE")))
    (error "git show HEAD:LICENSE does not match"))
  1)

(define (test-empty-dir)
  (when (!= (length (git-load-blob "HEAD" "" ""))
            0)
    (error "should return empty string for empty git root"))
  1)

(define (test-not-exist-dir)
  (when (!= (length (git-load-blob "HEAD" "" "/not-exist"))
            0)
    (error "should return empty string for not-exist git root"))
  1)

(define (test-invalid-git-root)
  (if (os-win32?)
    0
    (if (== (length (git-load-blob "HEAD" "hosts" "/etc")) 0)
      1
      (error "should return empty string for exist but invalid git root"))))

(define (test-invalid-blob)
  (if (== (length (git-load-blob "HEAD" "not-exist" (url-pwd))) 0)
    1
    (error "should return empty string for not exist blob")))

(define (test-invalid-rev)
  (if (== (length (git-load-blob "not-exist" "LICENSE" (url-pwd))) 0)
    1
    (error "should return empty string for not exist rev")))

(tm-define (test_44_6)
  (let ((n (+ (test-show-license)
              (test-empty-dir)
              (test-not-exist-dir)
              (test-invalid-git-root)
              (test-invalid-blob)
              (test-invalid-rev))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 44_6: ok\n")))
