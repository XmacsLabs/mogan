;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : preference-test.scm
;; DESCRIPTION : Test suite for User Preference
;; COPYRIGHT   : (C) 2024  Ke Shi
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (srfi srfi-78))

;; Helper function to set up a test key-value pair
(define (setup-test-key)
  (cpp-set-preference "test_key" "test_value"))

;; Helper function to clean up a test key-value pair
(define (cleanup-test-key)
  (cpp-reset-preference "test_key"))

;; Test if a preference key exists
(define (test-cpp-has-preference?)
  (setup-test-key)
  (check (cpp-has-preference? "test_key") => #t)
  (check (cpp-has-preference? "nonexistent_key") => #f)
  (cleanup-test-key)
  (check (cpp-has-preference? "test_key") => #f))

;; Test getting a preference value
(define (test-cpp-get-preference)
  (setup-test-key)
  (check (cpp-get-preference "test_key" "default_value") => "test_value")
  (check (cpp-get-preference "nonexistent_key" "default_value") => "default_value")
  (cleanup-test-key)
  (check (cpp-get-preference "test_key" "default_value") => "default_value"))

;; Test setting a preference value
(define (test-cpp-set-preference)
  (setup-test-key) 
  (check (cpp-get-preference "test_key" "default_value") => "test_value")
  (cpp-set-preference "test_key" "new_value")
  (check (cpp-get-preference "test_key" "default_value") => "new_value")
  (cleanup-test-key))

;; Main test runner
(define (regtest-preference)
  (test-cpp-has-preference?)
  (test-cpp-get-preference)
  (test-cpp-set-preference)
  (check-report)
  (if (check-failed?)
    (begin
      (display "Some tests failed.\n")
      (exit -1)))
  (display "All tests passed successfully.\n"))
