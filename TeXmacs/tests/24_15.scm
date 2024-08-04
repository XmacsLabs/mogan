;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 24_15.scm
;; DESCRIPTION : Test for docx.scm
;; COPYRIGHT   : (C) 2024  ATQlove
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (srfi srfi-78))
(use-modules (data docx))

(define (tm->docx tm-file-url docx-file-url)
  ; Step 1: load the tm file
  ; (display* "load-buffer: " tm-file-url "\n")
  (load-buffer tm-file-url)
  ; (display* "buffer-loaded: " tm-file-url "\n")

  ; Step 2: Export the buffer to the docx file url
  ; (display* "export buffer to: " docx-file-url "\n")
  (export-to-docx tm-file-url docx-file-url)

  ; Check if the docx file exists
  (if (url-exists? docx-file-url)
      (display* "Export successful: " docx-file-url "\n")
      (display* "Export failed: " docx-file-url "\n"))

  (url-exists? docx-file-url))

(define (test_24_15)
  (display  (url-exists? (system->url "$TEXMACS_PATH/tests/tm/24_15.tm")))
  (let* ((tm-url "$TEXMACS_PATH/tests/tm/24_15.tm")
         (docx-url "$TEXMACS_PATH/tests/tm/24_15.docx")
         (result (tm->docx tm-url docx-url)))
    (check result => #t)
    (check-report)
    (if (check-failed?) (exit -1))))
