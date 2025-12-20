
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 203_6.scm
;; DESCRIPTION : Tests for latex->texmacs conversion
;; COPYRIGHT   : (C) 2025  JimZhouZZY
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(check-set-mode! 'report-failed)

(define (test-latex-to-texmacs)
  (check (tree->stree (latex->texmacs (parse-latex "\\[ \\begin{cases} \n x \\\\\n [y]a \\end{cases} \\]")))
         => '(document (equation* (document (choice (tformat (table (row (cell "x")) (row (cell (concat (around "[" "y" "]") "*a"))))))))) )
  (check (tree->stree (latex->texmacs (parse-latex "\\[ \\begin{cases} \n x \\\\\n a[y] \\end{cases} \\]")))
         => '(document (equation* (document (choice (tformat (table (row (cell "x")) (row (cell (concat "a" (around "[" "y" "]")))))))))) )
  (check (tree->stree (latex->texmacs (parse-latex "\\[ \\begin{cases} \n [w]c + [wx]i_x + [wy]i_y = [wE] \\\\\n [wx]c + [wxx]i_x + [wxy]i_y = [wxE] \\\\\n [wy]c + [wxy]i_x + [wyy]i_y = [wyE] \\end{cases} \\]")))
         => '(document (equation* (document (choice (tformat (table (row (cell (concat (around "[" "w" "]") "*c+" (around "[" "w*x" "]") "*i" (rsub "x") "+" (around "[" "w*y" "]") "*i" (rsub "y") "=" (around "[" "w*E" "]")))) (row (cell (concat (around "[" "w*x" "]") "*c+" (around "[" "w*x*x" "]") "*i" (rsub "x") "+" (around "[" "w*x*y" "]") "*i" (rsub "y") "=" (around "[" "w*x*E" "]")))) (row (cell (concat (around "[" "w*y" "]") "*c+" (around "[" "w*x*y" "]") "*i" (rsub "x") "+" (around "[" "w*y*y" "]") "*i" (rsub "y") "=" (around "[" "w*y*E" "]")))))))))) )
)

(tm-define (test_203_16)
  (test-latex-to-texmacs)
  (check-report))
