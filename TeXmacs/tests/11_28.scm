(define (tex-font-list)
  (url->list
   (url-expand
    (url-complete
     (url-append (url-descendants "$TEXMACS_PATH/fonts")
                 (url-or (url-wildcard "*.pfb")
                         (url-wildcard "*.tfm")))   
     "fr"))))

(define (font-tuple-list)
  (map (lambda (x)
        (list (url->string (url-tail x))
              (string-replace (url->string x)
                              (url->string "$TEXMACS_PATH")
                              "$TEXMACS_PATH")))
       (tex-font-list)))

(define (font-dump-basename)
 (display* "(tuple" "\n")
 (map (lambda (x) (display* (string-quote (car x)) " " (string-quote (cadr x)) "\n"))
      (font-tuple-list))
 (display* ")" "\n"))

(tm-define (test_11_28)
 (noop))
