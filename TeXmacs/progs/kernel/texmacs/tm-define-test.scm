
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-define-test.scm
;; DESCRIPTION : Test suite for tm-define
;; COPYRIGHT   : (C) 2021  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-define-test)
  (:use (kernel texmacs tm-define)))

(define s7-style1
  (let ((documentation (lambda (f) "documentation in documentation variable")))
    (lambda () #f)))

(define s7-style2
  (let ((+documentation+ "documentation in +documentation+ variable"))
    (lambda () #f)))

(define (s7-style3)
  "Documentation in first line"
  #f)

(tm-define (tm-style1)
  (:synopsis "Documentation in :synopsis tab")
  #f)

(define (regtest-help)
  (regression-test-group
   "procedure, help" "help"
   help :none
   (test "documentation in documentation variable" s7-style1
                      "documentation in documentation variable")
   (test "documentation in +documentation+ variable" s7-style2
                      "documentation in +documentation+ variable")
   (test "Documentation in first line" s7-style3
                      "Documentation in first line")
   (test "Documentation in :synopsis tab" tm-style1
                      `("Documentation in :synopsis tab"))))

(define (regtest-procedure-symbol-name)
  (regression-test-group
   "procedure, procedure-symbol-name" "symbol"
   procedure-symbol-name :none
   (test "glue procedure" system 'system)
   (test "glue procedure make-space" make-space 'make-space)
   (test "tm-defined" exec-interactive-command
                      'exec-interactive-command)
   (test "anonymous function" (lambda (x) (+ x 1)) #f)
   (test "invalid input" 1 #f)))

(define (regtest-procedure-name)
  (regression-test-group
   "procedure, procedure-name" "procedure"
   procedure-name :none
   (test "glue procedure" system 'system)
   (test "glue procedure make-space" make-space 'make-space)
   (test "tm-defined" exec-interactive-command
                      'exec-interactive-command)
   (test "anonymous function" (lambda (x) (+ x 1)) #f)
   (test "invalid input" 1 #f)))


(tm-define (regtest-tm-define)
  (let ((n (+ (regtest-procedure-name)
              (regtest-procedure-symbol-name)
              (regtest-help))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-define: ok\n")))
