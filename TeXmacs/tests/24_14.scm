
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 24_14.scm
;; DESCRIPTION : Test for 24_14
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (srfi srfi-78))

(define (tm->html tm-file-url html-file-url)
  ; Step 1: load the tm file
  (display* "load-buffer: " tm-file-url "\n")
  (load-buffer tm-file-url)
  ; it will crash when loading the buffer in beamer style before this pull request
  (display* "buffer-loaded: " tm-file-url "\n")

  ; Step 2: cleaning for the html file url
  (when (url-exists? html-file-url) (system-remove html-file-url))

  ; Step 3: Export the buffer to the html file url
  (display* "export buffer to: " html-file-url)
  (export-buffer-main (current-buffer) html-file-url "html" ())

  #t)

(define (test_24_14)
  (debug-set "qt" #t)
  (let ((result
         (tm->html (system->url "$TEXMACS_PATH/tests/tm/24_14.tm")
                   (url-glue (url-temp) ".html"))))
    (check result => #t)
    (check-report)
    (if (check-failed?) (exit -1))))
