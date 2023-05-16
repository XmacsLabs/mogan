(define (test-no-title)
  (load-buffer "$TEXMACS_PATH/tests/9_1_无标题.tm")
  (regression-test-group
   "get-metadata" "result"
   get-metadata :none
   (test "no title" "title" "9_1_无标题.tm")))

(define (test-title)
  (load-buffer "$TEXMACS_PATH/tests/9_1_with_title.tm")
  (regression-test-group
   "get-metadata" "result"
   get-metadata :none
   (test "with title" "title" "<#6807><#9898>")))

(tm-define (test_9_1)
  (let ((n (+ (test-no-title)
              (test-title))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 9_1: ok\n")))
