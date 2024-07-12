(define (export-as-html-and-load path)
  (with path (string-append "$TEXMACS_PATH/tests/tmu/" path)
    (with tmpfile (url-temp)
      (load-buffer path)
      (buffer-export path tmpfile "html")
      (string-load tmpfile))))
  
(define (load-html path)
  (with path (string-append "$TEXMACS_PATH/tests/html/" path)
    (string-replace (string-load path)  "\r\n" "\n")))

(define (test-html-title)
  (regression-test-group
   "export to html and load as string" "load as string"
   export-as-html-and-load load-html
   (test "test html title" "24_10_title.tmu" "24_10_title.html")))

(tm-define (test_24_10)
  (let ((n (+ (test-html-title))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 24_10: ok\n")))
