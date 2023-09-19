

(define src-dir (url->string (url-expand "$PWD/html-src")))
(define dest-dir (url->string (url-expand "$PWD/html-docs")))

(define (notes-run update?)
    (display* "Source dir :" src-dir "\n")
    (display* "Dest dir   :" dest-dir "\n")
            (display* "* Building website\n")
    (tmweb-convert-dir src-dir dest-dir)
    (display* "Done."))

(define (notes-update) (notes-run #t))
(define (notes-build)  (notes-run #f))