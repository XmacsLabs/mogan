
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-dialogue-test.scm
;; DESCRIPTION : Test suite for tm-dialogue
;; COPYRIGHT   : (C) 2022  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-dialogue-test)
  (:use (kernel texmacs tm-dialogue)))

(define (regtest-procedure-symbol-name)
  (regression-test-group
   "procedure" "symbol"
   procedure-symbol-name :none
   (test "glue procedure" system 'system)))

(tm-define (regtest-tm-dialogue)
  (let ((n (+ (regtest-procedure-symbol-name))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-dialogue: ok\n")))

