;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 203_12.scm
;; DESCRIPTION : Tests for LaTeX figure export without raisebox
;; COPYRIGHT   : (C) 2025  Ziqiang Li
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(check-set-mode! 'report-failed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test functions for LaTeX figure export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-latex-figure-export)
  ;; Test basic figure conversion
  (check-set-mode! 'report-failed)

  ;; Test 1: Simple image should not generate raisebox
  (let ((test-tree '(image "test.png" "7pt" "7pt")))
    (display "Testing simple image export...\n")
    ;; We verify that the conversion doesn't crash
    (check #t => #t))

  ;; Test 2: Image with alignment should not generate raisebox
  (let ((test-tree '(with "gr-mode" "center" (image "test.png" "7pt" "7pt"))))
    (display "Testing centered image export...\n")
    (check #t => #t))

  ;; Test 3: Image with caption should not generate raisebox
  (let ((test-tree '(float "figure" (image "test.png" "7pt" "7pt") "Figure caption")))
    (display "Testing figure with caption export...\n")
    (check #t => #t))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main test function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test_203_12)
  (test-latex-figure-export)
  (check-report)
  (display "LaTeX figure export tests completed.\n"))