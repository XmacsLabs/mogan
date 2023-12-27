
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : data-test.scm
;; DESCRIPTION : Test suite for lolly::data
;; COPYRIGHT   : (C) 2023  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (lolly data-test))

(define (regtest-int-to-hex)
  (regression-test-group
   "int" "hex"
   integer->hexadecimal :none
   (test "integer->hexadecimal 1" 1 "1")
   (test "integer->hexadecimal 10" 10 "A")
   (test "integer->hexadecimal 255" 255 "FF")))

(tm-define (regtest-data)
  (let ((n (+ (regtest-int-to-hex))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of url: ok\n")))
