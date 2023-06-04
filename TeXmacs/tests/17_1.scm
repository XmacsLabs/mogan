(define (test-count-characters)
  (load-buffer "$TEXMACS_PATH/tests/17_1.tm")
  (regression-test-group
   "count-characters" "result"
   count-characters :none
   (test "character count" (buffer-tree) 41)))

(tm-define (test_9_1)
  (let ((n (+ (test-no-title)
              0)))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 9_1: ok\n")))
