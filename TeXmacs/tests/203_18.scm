;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 203_18.scm
;; DESCRIPTION : Unit tests for parse-latex (multi-integral and big operator import)
;; COPYRIGHT   : (C) 2026 AcceleratorX
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(check-set-mode! 'report-failed)

(define (test-parse-latex-big-operators)
	(check (tree->stree (latex->texmacs (parse-latex "\\( \\oint \\)")))
         => '(math (big "oint")))
  (check (tree->stree (latex->texmacs (parse-latex "\\( \\iiint \\)")))
         => '(math (big "iiint")))
	(check (tree->stree (latex->texmacs (parse-latex "\\( \\idotsint \\)")))
         => '(math (big "idotsint")))
	(check (tree->stree (latex->texmacs (parse-latex "\\oint")))
         => '(big "oint"))
	(check (tree->stree (latex->texmacs (parse-latex "\\iiint")))
         => '(big "iiint"))
	(check (tree->stree (latex->texmacs (parse-latex "\\idotsint")))
				 => '(big "idotsint"))
	(check (tree->stree (latex->texmacs (parse-latex "\\( {\\iiint }_{\\Omega }^{\\infty} \\)")))
				 => '(math (concat (big "iiint") (rsub "<Omega>") (rsup "<infty>"))))
)

(tm-define (test_203_18)
  (test-parse-latex-big-operators)
  (check-report))
