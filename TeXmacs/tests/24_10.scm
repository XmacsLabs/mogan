;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-htmlout.scm
;; DESCRIPTION : Tests for htmlout.scm changes
;; COPYRIGHT   : (C) 2024  ATQlove
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./TeXmacs/plugins/html/progs/convert/html/htmlout.scm")
(import (srfi srfi-78))

;; Define a global variable to capture output
(define *output* "")

;; Define htmlout-text function to record output
(define (htmlout-text . args)
  (set! *output* (string-append *output* (apply string-append args))))

(define (run-tests tests)
  (for-each
   (lambda (test)
     (test))
   tests))

(define (assert-equal actual expected)
  (if (equal? actual expected)
      (display "Test passed.\n")
      (begin
        (display "Test failed.\n")
        (display "Expected: ")
        (display expected)
        (display "\n")
        (display "Actual: ")
        (display actual)
        (display "\n"))))

;; Ensure attribute names are symbols and attribute values are strings
(define (format-attributes attrs)
  (map (lambda (attr) (list (car attr) (cdr attr))) attrs))

(define tests
  (list
   ;; Group 1: Self-closing tags without attributes
   ;; br
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'br '())
     (htmlout-close 'br)
     (assert-equal *output* "<br />\n"))
   ;; hr
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'hr '())
     (htmlout-close 'hr)
     (assert-equal *output* "<hr />\n"))
   ;; wbr
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'wbr '())
     (htmlout-close 'wbr)
     (assert-equal *output* "<wbr />\n"))


   ;; Group 2: Self-closing tags with attributes
   ;; area
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'area (format-attributes '((shape . "rect") (coords . "34,44,270,350") (alt . "Computer") (href . "computer.htm"))))
     (htmlout-close 'area)
     (assert-equal *output* "<area />\n"))
   ;; base
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'base (format-attributes '((href . "https://example.com/"))))
     (htmlout-close 'base)
     (assert-equal *output* "<base />\n"))
   ;; col
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'col (format-attributes '((span . "2") (class . "col-class"))))
     (htmlout-close 'col)
     (assert-equal *output* "<col />\n"))
   ;; embed
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'embed (format-attributes '((src . "video.mp4") (type . "video/mp4"))))
     (htmlout-close 'embed)
     (assert-equal *output* "<embed />\n"))
   ;; img
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'img (format-attributes '((src . "image.png") (alt . "image"))))
     (htmlout-close 'img)
     (assert-equal *output* "<img />\n"))
   ;; input
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'input (format-attributes '((type . "text") (value . "sample input"))))
     (htmlout-close 'input)
     (assert-equal *output* "<input />\n"))
   ;; link
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'link (format-attributes '((rel . "stylesheet") (href . "styles.css"))))
     (htmlout-close 'link)
     (assert-equal *output* "<link />\n"))
   ;; meta
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'meta (format-attributes '((charset . "UTF-8"))))
     (htmlout-close 'meta)
     (assert-equal *output* "<meta />\n"))
   ;; source
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'source (format-attributes '((src . "movie.mp4") (type . "video/mp4"))))
     (htmlout-close 'source)
     (assert-equal *output* "<source />\n"))
   ;; track
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'track (format-attributes '((kind . "subtitles") (src . "subtitles_en.vtt") (srclang . "en") (label . "English"))))
     (htmlout-close 'track)
     (assert-equal *output* "<track />\n"))
     

   ;; Group 3: Non-self-closing tags example
   ;; a
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'a (format-attributes '((href . "https://example.com") (title . "Example"))))
     (htmlout-close 'a)
     (assert-equal *output* "<a>\n\n</a>"))
   ;; div
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'div (format-attributes '((class . "container") (id . "main"))))
     (htmlout-close 'div)
     (assert-equal *output* "<div>\n\n</div>"))
   ))

(tm-define (test_24_10)
  ;; Run all tests and count the number of tests
  (let ((n (length tests)))
    (run-tests tests)
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 24_10: ok\n")))
