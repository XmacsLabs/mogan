
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : system-test.scm
;; DESCRIPTION : Test suite for glue symbols from src/System
;; COPYRIGHT   : (C) 2022  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(texmacs-module (check system-test))

(define (regtest-mimetype-for-url)
  (regression-test-group
   "mimetype-for-url" "string"
   mimetype-for-url :none
   ;; Code
   (test "python" "test.py" "text/x-python")
   ;; Image
   (test "png" "test.png" "image/png")
   ;; Sound
   (test "mpeg" "test.mp3" "audio/mpeg")
   ;; Not exists
   (test "not exists format" "test.abcdefg" "application/octet-stream")))

(tm-define (regtest-src-system)
  (let ((n (+ (regtest-mimetype-for-url))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of glue symbols from src/System: ok\n")))
