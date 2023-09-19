

;; run: build/packages/app.mogan/bin/mogan -headless -b TeXmacs/tests/24_4.scm -x "(build)" -q

(define src-dir (url->string (url-expand "$PWD/TeXmacs/tests/24_4-tm-src")))
(define dest-dir (url->string (url-expand "$PWD/TeXmacs/tests/24_4-html-docs")))

(define (notes-build)
    (display* "Source dir :" src-dir "\n")
    (display* "Dest dir   :" dest-dir "\n")
            (display* "* Building website\n")
    (tmweb-convert-dir src-dir dest-dir)
    (display* "Done."))

(tm-define (build) (notes-build))
