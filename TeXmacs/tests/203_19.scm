;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 203_19.scm
;; DESCRIPTION : Unit tests for \overset \stackrel \underset rendering
;; COPYRIGHT   : (C) 2026 AcceleratorX
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(check-set-mode! 'report-failed)

(define (test-parse-latex-over-under-set)
  (check (tree->stree (latex->texmacs (parse-latex "\\( 2{\\mathrm{{NaHCO}}}_{3}\\overset{\\bigtriangleup }{ = }{\\mathrm{{Na}}}_{2}{\\mathrm{{CO}}}_{3} + {\\mathrm{H}}_{2}\\mathrm{O} + {\\mathrm{{CO}}}_{2} \\uparrow \\)")))
         => '(math (concat "2" (math-up "NaHCO") (rsub "3") (above "=" (big "triangleup")) (math-up "Na") (rsub "2") (math-up "CO") (rsub "3") "+" (math-up "H") (rsub "2") (math-up "O") "+" (math-up "CO") (rsub "2") "<uparrow>")))
  (check (tree->stree (latex->texmacs (parse-latex "\\( \\overset{\\bigtriangleup }{ = } \\)")))
         => '(math (above "=" (big "triangleup"))))
  (check (tree->stree (latex->texmacs (parse-latex "\\( \\stackrel{\\bigtriangleup }{ = } \\)")))
         => '(math (above "=" (big "triangleup"))))
  (check (tree->stree (latex->texmacs (parse-latex "\\( \\underset{\\bigtriangleup }{ = } \\)")))
         => '(math (below "=" (big "triangleup"))))
)

(tm-define (test_203_19)
  (test-parse-latex-over-under-set)
  (check-report))
