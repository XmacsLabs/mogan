(define (tm->html tm-file html-file)
  (load-buffer tm-file)
  (export-buffer-main (current-buffer) (html-file) "html" ()))

(define (test_24_14)
  (tm->html "$TEXMACS_PATH/tests/tm/24_14.tm" "/tmp/test.html"))
