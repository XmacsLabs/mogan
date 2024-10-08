(import (liii check))

(define (test-single-quote)
  (let ((cork_60 (string (integer->char #x60)))
        (cork_27 (string (integer->char #x27))))
    (check (utf8->cork "‘’") => (string-append cork_60 cork_27))
    (check (cork->utf8 (string-append cork_60 cork_27)) => "‘'")
    (check (strict-cork->utf8 (string-append cork_60 cork_27)) => "‘'")))


(tm-define (test_11_38)
  (test-single-quote))
