
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 201_11.scm
;; DESCRIPTION : Tests for tab-cycling completion
;; COPYRIGHT   : (C) 2025  JimZhouZZY
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(check-set-mode! 'report-failed)

(define (strip-lambda lst)
  (map (lambda (x)
         (list (car x)
               (map (lambda (y)
                      (list 'lambda (cadr y) (caddr y)))
                    (cdr x))))
       lst))

(define (test-kbd-find-prefix)
  (make 'math)
  (kbd-insert "now we are in math mode")
  (check (strip-lambda (kbd-find-prefix "a "))
    => (strip-lambda '(("a tab" ((#<lambda ()>) "<alpha>" "")))))
  (check (strip-lambda (kbd-find-prefix "e "))
    => (strip-lambda '(
                       ("e tab tab" ((#<lambda ()>) "<mathe>" ""))
                       ("e tab tab tab" ((#<lambda ()>) "<epsilon>" ""))
                       ("e tab tab tab tab" ((#<lambda ()>) "<backepsilon>" ""))
                       ("e tab" ((#<lambda ()>) "<varepsilon>" "")))))
  (check (strip-lambda (kbd-find-prefix "e tab tab tab"))
    => (strip-lambda '(
                       ("e tab tab tab" ((#<lambda ()>) "<epsilon>" ""))
                       ("e tab tab tab tab" ((#<lambda ()>) "<backepsilon>" ""))))))

(define (test-math-tabcycle-symbols)
  (make 'math)
  (kbd-insert "now we are in math mode")
  ;; 注意 symbol* 高亮的位置也是随着 tab 的次数变化的
  (check (math-tabcycle-symbols "a") => '((symbol* "a") (symbol "<alpha>")))
  (check (math-tabcycle-symbols "a tab")
    => '((symbol "a") (symbol* "<alpha>")))
  (check (math-tabcycle-symbols "a tab tab") => '())
  (check (math-tabcycle-symbols "= >")
    => '((symbol* "<Rightarrow>") (symbol "<Downarrow>") (symbol "<Uparrow>") (symbol "<Rrightarrow>")))
  (check (math-tabcycle-symbols "= > tab")
    => '((symbol "<Rightarrow>") (symbol* "<Downarrow>") (symbol "<Uparrow>") (symbol "<Rrightarrow>")))
  (check (math-tabcycle-symbols "= > tab tab")
    => '((symbol "<Rightarrow>") (symbol "<Downarrow>") (symbol* "<Uparrow>") (symbol "<Rrightarrow>")))
)

(tm-define (test_201_11)
  (test-kbd-find-prefix)
  (test-math-tabcycle-symbols)
  (check-report))
