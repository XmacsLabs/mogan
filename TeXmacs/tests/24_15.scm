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

(define (tm->docx tm-file-url)
  ; Step 1: load the tm file
  ; (display* "load-buffer: " tm-file-url "\n")
  (load-buffer tm-file-url)
  ; (display* "buffer-loaded: " tm-file-url "\n")

  ; Step 2: Export the buffer to the docx string
  (let* ((result (texmacs-tree->docx-string tm-file-url `())) 
         (result_len (string-length result)))
    ; (display result) 
    (> result_len 0)))

(define (test_24_15)
  (when (os-macos?) (exit 0))
  (display  (url-exists? (system->url "$TEXMACS_PATH/tests/tm/24_15.tm")))
  (let* ((tm-url "$TEXMACS_PATH/tests/tm/24_15.tm")
         (result (tm->docx tm-url)))
    (check result => #t)
    (check-report)
    (if (check-failed?) (exit -1))))
