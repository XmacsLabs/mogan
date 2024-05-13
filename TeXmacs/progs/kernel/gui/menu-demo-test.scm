;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-demo-test.scm
;; DESCRIPTION : Test suite for various widgets in the menu-demo.scm module
;; COPYRIGHT   : (C) 2024  ATQlove
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui menu-test)
  (:use (kernel gui menu-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simulate-click widget action expected-response)
  (let ((response (action)))
    (if (equal? response expected-response)
        (display* "Test " (widget-name widget) " passed.\n")
        (display* "Test " (widget-name widget) " failed: expected " expected-response ", got " response ".\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression test groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-widget1)
  (regression-test-group
   "Test for widget1 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget1 (lambda () (widget1-action 'toggle1)) #t))
   (lambda ()
     (simulate-click 'widget1 (lambda () (widget1-action 'toggle2)) #f))))

(define (regtest-widget2)
  (regression-test-group
   "Test for widget2 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget2 (lambda () (widget2-action 'tab1)) "ExpectedResult1"))
   (lambda ()
     (simulate-click 'widget2 (lambda () (widget2-action 'tab2)) "ExpectedResult2"))))

(define (regtest-widget3)
  (regression-test-group
   "Test for widget3 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget3 (lambda () (widget3-action 'toggle1)) #t))
   (lambda ()
     (simulate-click 'widget3 (lambda () (widget3-action 'toggle2)) #f))))

(define (regtest-widget4)
  (regression-test-group
   "Test for widget4 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget4 (lambda () (widget4-action 'choice)) "Third\n"))
   (lambda ()
     (simulate-click 'widget4 (lambda () (widget4-action 'choices)) "Third\nFifth\n"))))

(define (regtest-widget5)
  (regression-test-group
   "Test for widget5 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget5 (lambda () (widget5-action 'choice)) "Third\n"))
   (lambda ()
     (simulate-click 'widget5 (lambda () (widget5-action 'choices)) "Third\nFifth\n"))))

(define (regtest-widget6)
  (regression-test-group
   "Test for widget6 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget6 (lambda () (widget6-action 'output)) "This is true.\n"))
   (lambda ()
     (simulate-click 'widget6 (lambda () (widget6-action 'input)) "Trivial.\nBut you may add more details.\n"))))

(define (regtest-widget7)
  (regression-test-group
   "Test for widget7 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget7 (lambda () (widget7-action)) ""))  ;; Action and expected output
   (lambda () "")))  ;; Providing a lambda that returns the expected output for verification

(define (regtest-widget8)
  (regression-test-group
   "Test for widget8 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget8 (lambda () (widget8-action 'input "First")) "First\n"))
   (lambda ()
     (simulate-click 'widget8 (lambda () (widget8-action 'input "Second")) "Second\n"))))

(define (regtest-widget9)
  (regression-test-group
   "Test for widget9 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget9 (lambda () (widget9-action #f)) "Flag is off"))
   (lambda ()
     (simulate-click 'widget9 (lambda () (widget9-action #t)) "Flag is on"))))

(define (regtest-widget10)
  (regression-test-group
   "Test for widget10 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget10 (lambda () (widget10-action)) "Testing tree-view\n"))  ;; Action and expected output
   (lambda () "Testing tree-view\n")))  ;; Providing a lambda that returns the expected output for verification

(define (regtest-widget11)
  (regression-test-group
   "Test for widget11 functionality" "widget-tests"
   (lambda ()
     (simulate-click 'widget11 (lambda () (widget11-action "hop")) "hop: true\n"))
   (lambda ()
     (simulate-click 'widget11 (lambda () (widget11-action "hola")) "hola: false\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run all tests and display results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (run-all-widget-tests)
  (let ((results (list (regtest-widget1)
                       (regtest-widget2)
                       (regtest-widget3)
                       (regtest-widget4)
                       (regtest-widget5)
                       (regtest-widget6)
                       (regtest-widget7)
                       (regtest-widget8)
                       (regtest-widget9)
                       (regtest-widget10)
                       (regtest-widget11))))
    (display* "Total Tests Run: " (length results) "\n")
    (map (lambda (result)
           (if result
               (display "Test passed.\n")
               (display "Test failed.\n")))
         results)))
