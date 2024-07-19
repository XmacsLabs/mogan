
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 24_10.scm
;; DESCRIPTION : Test for task 24_10
;; COPYRIGHT   : (C) 2024  ATQlove
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (srfi srfi-78))

(define (export-as-html-and-load path)
  (with path (string-append "$TEXMACS_PATH/tests/tmu/" path)
    ; (display (string-append "Exporting and loading: " path "\n"))
    (with tmpfile (url-temp)
      (display (string-append "Temporary file created: " (url->string tmpfile) "\n"))
      (load-buffer path)
      ; (display "Buffer loaded\n")
      (buffer-export path tmpfile "html")
      ; (display "Buffer exported to HTML\n")
      (let ((html-content (string-load tmpfile)))
        ; (display "HTML content loaded from temporary file\n")
        html-content))))

(define (load-html path)
  (with path (string-append "$TEXMACS_PATH/tests/html/" path)
    ; (display (string-append "Loading HTML: " path "\n"))
    (let ((html-content (string-replace (string-load path) "\r\n" "\n")))
      ; (display "HTML content loaded and newline characters normalized\n")
      html-content)))

(tm-define (test_24_10)
  (check
    (export-as-html-and-load "24_10.tmu")
    =>
    (load-html "24_10.html"))

  (check-report)
  (if (check-failed?)
    (exit -1))
  (display "All tests completed.\n"))


