(import (liii check))

(define (test-single-quote)
  (let ((cork_60 (string (integer->char #x60)))
        (cork_27 (string (integer->char #x27))))
    (check (utf8->herk "‘’") => (string-append cork_60 "<#2019>"))
    (check (herk->utf8 (string-append cork_60 "<#2019>")) => "‘’")))


(tm-define (test_66_7)
  (test-single-quote))
