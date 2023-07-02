
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-modes-test.scm
;; DESCRIPTION : Test suite for tm-modes
;; COPYRIGHT   : (C) 2023  jingkaimori
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-modes-test)
  (:use (kernel texmacs tm-modes)))

(define (regtest-texmacs-mode-mode)
  (regression-test-group
   "procedure, texmacs-mode-mode" "texmacs-mode-mode"
   texmacs-mode-mode :none
   (test "procedure argument" in-math? 'unknown%)
   (test "invalid procedure" (lambda (f) #f) 'unknown%)
   (test "symbol argument" 'in-math? 'in-math%)))

(tm-define (regtest-tm-modes)
  (let ((n (+ (regtest-texmacs-mode-mode))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-modes: ok\n")))