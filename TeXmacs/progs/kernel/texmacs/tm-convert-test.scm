
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-convert-test.scm
;; DESCRIPTION : Test suite for tm-convert
;; COPYRIGHT   : (C) 2022  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(texmacs-module (kernel texmacs tm-convert-test)
  (:use (kernel texmacs tm-define)))

(define (regtest-format?)
  (regression-test-group
   "format?" "boolean"
   format? :none
   (test "python format" "python" #t)
   (test "scala format" "scala" #t)
   (test "no such format" "no-such-format" #f)))

(define (regtest-format-get-name)
  (regression-test-group
   "format-get-name" "string"
   format-get-name :none
   (test "python format" "python" "Python Source Code")
   (test "scala format" "scala" "Scala Source Code")
   (test "no such format" "no-such-format" #f)))

(define (regtest-format-from-suffix)
  (regression-test-group
   "format-from-suffix" "string"
   format-from-suffix :none
   (test "python format" "py" "python")
   (test "scala format" "scala" "scala")
   (test "texmacs format" "tm" "texmacs")
   (test "texmacs format" "ts" "texmacs")
   (test "texmacs format" "tmml" "tmml")
   (test "texmacs format" "stm" "stm")
   (test "png format" "png" "png")
   (test "no such format" "no-such-format" "generic")))

(tm-define (regtest-tm-convert)
  (let ((n (+ (regtest-format?)
              (regtest-format-get-name)
              (regtest-format-from-suffix))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-convert: ok\n")))
