
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

(define (prefix-symbols lst)
  (map (lambda (x)
         (list (car x) (cadr x)))
       lst))

(define (test-kbd-find-prefix)
  (make 'math)
  (kbd-insert "now we are in math mode")
  (check (prefix-symbols (kbd-find-prefix-tab "a"))
    => '(("a tab" "<alpha>")))
  (let* ((results (prefix-symbols (kbd-find-prefix-tab "e"))))
    (check results
      => '(("e tab" "<varepsilon>")
            ("e tab tab" "<mathe>")
            ("e tab tab tab" "<epsilon>")
            ("e tab tab tab tab" "<backepsilon>")))
    (check (list-tail results 2)
      => '(("e tab tab tab" "<epsilon>")
            ("e tab tab tab tab" "<backepsilon>")))))

(define (test-math-tabcycle-symbols)
  (make 'math)
  (kbd-insert "now we are in math mode")
  ;; 注意 symbol* 高亮的位置也是随着 tab 的次数变化的
  (check (math-tabcycle-symbols "a") => '((symbol-completion* "a") (symbol-completion "<alpha>")))
  (check (math-tabcycle-symbols "a tab")
    => '((symbol-completion "a") (symbol-completion* "<alpha>")))
  (check (math-tabcycle-symbols "a tab tab") => '())
  (check (math-tabcycle-symbols "= >")
    => '((symbol-completion* "<Rightarrow>") (symbol-completion "<Downarrow>") (symbol-completion "<Uparrow>") (symbol-completion "<Rrightarrow>")))
  (check (math-tabcycle-symbols "= > tab")
    => '((symbol-completion "<Rightarrow>") (symbol-completion* "<Downarrow>") (symbol-completion "<Uparrow>") (symbol-completion "<Rrightarrow>")))
  (check (math-tabcycle-symbols "= > tab tab")
    => '((symbol-completion "<Rightarrow>") (symbol-completion "<Downarrow>") (symbol-completion* "<Uparrow>") (symbol-completion "<Rrightarrow>")))
)

(tm-define (test_201_11)
  (test-kbd-find-prefix)
  (test-math-tabcycle-symbols)
  (check-report))
