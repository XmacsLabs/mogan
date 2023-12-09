(inherit-modules (texmacs texmacs tm-tools))

(define (test-count-characters)
  (load-buffer "$TEXMACS_PATH/tests/tm/17_1.tm")
  (regression-test-group
   "count-characters" "result"
   count-characters :none
   (test "character count" (buffer-tree) 41)))

(tm-define (test_17_1)
  (let ((n (+ (test-count-characters)
              0)))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 17_1: ok\n")))
