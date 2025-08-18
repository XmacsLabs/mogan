
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 201_15.scm
;; DESCRIPTION : Tests for match-macro-prefix
;; COPYRIGHT   : (C) 2025  JimZhouZZY
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source macro-edit-test)
    (:use (source macro-edit)
          (source macro-widgets)))

(import (liii check))

(check-set-mode! 'report-failed)

(define (init-test)
  (load-buffer "$TEXMACS_PATH/tests/tmu/201_15.tmu")
  (insert "test-match-macro-prefix"))

(define (test-match-macro-prefix)
  (check (match-macro-prefix "strong") => '("strong" "strong-color"))
  (check (match-macro-prefix "strong1") => '())
  (check (match-macro-prefix "str") => '("stressed" "stressed-color"
                                         "stressed-distance" "strike-through"
                                         "string-color" "striped-table"
                                         "strong" "strong-color"))
  (check (match-macro-prefix "ref") => '("reference"))
  (check (match-macro-prefix "suste") => '("sustech")))

(define (test-match-macro-prefix-robust)
  (check (match-macro-prefix "啊啊啊") => '())
  (check (match-macro-prefix "  ") => '())
  (check (match-macro-prefix "测试") => '())
  (check (match-macro-prefix "💡") => '())
  (check (match-macro-prefix "😀") => '())
  (check (match-macro-prefix "!@#") => '())
  (check (match-macro-prefix "空格 ") => '())
  (check (match-macro-prefix "tab\t") => '())
  (check (match-macro-prefix "newline\n") => '()))

(define (test-fuzzy-match-macro-prefix)
  (check (car (fuzzy-match-macro-prefix "stong")) => "strong")
  (check (car (fuzzy-match-macro-prefix "refrence")) => "reference")
  (check (car (fuzzy-match-macro-prefix "latex")) => "LaTeX")
  (check (car (fuzzy-match-macro-prefix "susth")) => "sustech"))

(tm-define (test_201_15)
  (init-test)
  (test-match-macro-prefix)
  (test-match-macro-prefix-robust)
  (test-fuzzy-match-macro-prefix)
  (check-report))
