(define (tm->html tm-file html-file)
  (display* "load-buffer: " tm-file "\n")
  (load-buffer tm-file)
  (display* "buffer-loaded: " tm-file "\n")
  (export-buffer-main (current-buffer) html-file "html" ()))

(define (test_24_14)
  (debug-set "qt" #t)
  (tm->html "$TEXMACS_PATH/tests/tm/24_14.tm" "/tmp/test.html"))
