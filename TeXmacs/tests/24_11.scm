;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 24_11.scm
;; DESCRIPTION : Tests for self-close tags
;; COPYRIGHT   : (C) 2024  ATQlove
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using (load ...) instead of (use-modules ...) allows overriding definitions 
;; in the file. This lets us redefine functions like htmlout-text to capture 
;; output for testing.
(load "./TeXmacs/plugins/html/progs/convert/html/htmlout.scm")
(import (srfi srfi-78))

;; Define a global variable to capture output
(define *output* "")

;; Override htmlout-text to capture output
;; Necessary for testing by appending generated HTML to *output*
(define (htmlout-text . args)
  (set! *output* (string-append *output* (apply string-append args))))

(define (run-tests tests)
  (for-each
   (lambda (test)
     (test))
   tests))

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
     (check
      (begin
        *output*)
      =>
      "<br />"))
   ;; hr
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'hr '())
     (htmlout-close 'hr)
     (check
      (begin
        *output*)
      =>
      "<hr />"))
   ;; wbr
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'wbr '())
     (htmlout-close 'wbr)
     (check
      (begin
        *output*)
      =>
      "<wbr />"))


   ;; Group 2: Self-closing tags with attributes
   ;; area
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'area (format-attributes '((shape . "rect") (coords . "34,44,270,350") (alt . "Computer") (href . "computer.htm"))))
     (htmlout-close 'area)
     (check
      (begin
        *output*)
      =>
      "<area />"))
   ;; base
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'base (format-attributes '((href . "https://example.com/"))))
     (htmlout-close 'base)
     (check
      (begin
        *output*)
      =>
      "<base />"))
   ;; col
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'col (format-attributes '((span . "2") (class . "col-class"))))
     (htmlout-close 'col)
     (check
      (begin
        *output*)
      =>
      "<col />"))
   ;; embed
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'embed (format-attributes '((src . "video.mp4") (type . "video/mp4"))))
     (htmlout-close 'embed)
     (check
      (begin
        *output*)
      =>
      "<embed />"))
   ;; img
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'img (format-attributes '((src . "image.png") (alt . "image"))))
     (htmlout-close 'img)
     (check
      (begin
        *output*)
      =>
      "<img />"))
   ;; input
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'input (format-attributes '((type . "text") (value . "sample input"))))
     (htmlout-close 'input)
     (check
      (begin
        *output*)
      =>
      "<input />"))
   ;; link
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'link (format-attributes '((rel . "stylesheet") (href . "styles.css"))))
     (htmlout-close 'link)
     (check
      (begin
        *output*)
      =>
      "<link />"))
   ;; meta
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'meta (format-attributes '((charset . "UTF-8"))))
     (htmlout-close 'meta)
     (check
      (begin
        *output*)
      =>
      "<meta />"))
   ;; source
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'source (format-attributes '((src . "movie.mp4") (type . "video/mp4"))))
     (htmlout-close 'source)
     (check
      (begin
        *output*)
      =>
      "<source />"))
   ;; track
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'track (format-attributes '((kind . "subtitles") (src . "subtitles_en.vtt") (srclang . "en") (label . "English"))))
     (htmlout-close 'track)
     (check
      (begin
        *output*)
      =>
      "<track />"))
     

   ;; Group 3: Non-self-closing tags example
   ;; a
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'a (format-attributes '((href . "https://example.com") (title . "Example"))))
     (htmlout-close 'a)
     (check
      (begin
        *output*)
      =>
      "<a></a>"))
   ;; div
   (lambda ()
     (set! *output* "")  ; Reset output
     (htmlout-open-tags 'div (format-attributes '((class . "container") (id . "main"))))
     (htmlout-close 'div)
     (check
      (begin
        *output*)
      =>
      "<div></div>"))
   ))

(tm-define (test_24_11)
  (run-tests tests)
  (check-report)
  (if (check-failed?)
    (exit -1))
    (display "Test suite of 24_11 end\n"))