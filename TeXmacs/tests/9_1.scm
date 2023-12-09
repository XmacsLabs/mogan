(define (test-no-title)
  (load-buffer "$TEXMACS_PATH/tests/tm/9_1_无标题.tm")
  (regression-test-group
   "get-metadata" "result"
   get-metadata :none
   (test "no title" "title" "9_1_无标题.tm")))

(define (test-title)
  (load-buffer "$TEXMACS_PATH/tests/tm/9_1_with_title.tm")
  (regression-test-group
   "get-metadata" "result"
   get-metadata :none
   (test "with title" "title" "标题")))

(define (test-metadata)
  (load-buffer "$TEXMACS_PATH/tests/tm/9_1_with_metadata.tm")
  (regression-test-group
   "get-metadata" "result"
   get-metadata :none
   (test "get author" "author" "作者")
   (test "get subject" "subject" "主题")
   (test "get title" "title" "标题")))

(tm-define (test_9_1)
  (let ((n (+ (test-no-title)
              (test-title)
              (test-metadata))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 9_1: ok\n")))
