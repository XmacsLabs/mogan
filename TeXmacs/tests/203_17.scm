;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 203_17.scm
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
	(check (tree->stree (latex->texmacs (parse-latex "\\( \\left\\lbrack  {-1,1}\\right\\rbrack \\)")))
         => '(math (around* "[" "-1,1" "]")))
  (check (tree->stree (latex->texmacs (parse-latex "\\( \\mathrm{d}z = \\frac{1}{a}\\left\\lbrack {\\frac{1}{{e}^{y + \\mathrm{{az}}} + 1}\\mathrm{\\;d}x + \\frac{-{e}^{y + \\mathrm{{az}}}}{{e}^{y + \\mathrm{{az}}} + 1}\\mathrm{\\;d}y}\\right\\rbrack \\)")))
         => '(math (concat (math-up "d") "z=" (frac "1" "a") "*" (around* "[" (concat (frac "1" (concat "e" (rsup (concat "y+" (math-up "az"))) "+1")) (with "math-font-family" "rm" (concat (space "0.27em") "d")) "x+" (frac (concat "-e" (rsup (concat "y+" (math-up "az")))) (concat "e" (rsup (concat "y+" (math-up "az"))) "+1")) (with "math-font-family" "rm" (concat (space "0.27em") "d")) "y") "]"))))
	(check (tree->stree (latex->texmacs (parse-latex "\\( A = \\left\\lbrack  \\begin{array}{lll} a & 2 & 2 \\\\  2 & a & 2 \\\\  2 & 2 & a \\end{array}\\right\\rbrack \\)")))
				 => '(math (concat "A=" (around* "[" (tabular* (tformat (cwith "1" "-1" "1" "1" "cell-halign" "l") (cwith "1" "-1" "1" "1" "cell-lborder" "0ln") (cwith "1" "-1" "2" "2" "cell-halign" "l") (cwith "1" "-1" "3" "3" "cell-halign" "l") (cwith "1" "-1" "3" "3" "cell-rborder" "0ln") (table (row (cell "a") (cell "2") (cell "2")) (row (cell "2") (cell "a") (cell "2")) (row (cell "2") (cell "2") (cell "a"))))) "]"))))
)

(tm-define (test_203_17)
  (test-parse-latex-big-operators)
  (check-report))