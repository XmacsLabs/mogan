;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 203_5.scm
;; DESCRIPTION : Tests for latex to tmu converter
;; COPYRIGHT   : (C) 2025  Jack Yansong Li
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(load "./TeXmacs/plugins/latex/progs/init-latex.scm")

(define (export-as-latex-and-load path)
  (with path (string-append "$TEXMACS_PATH/tests/tmu/" path)
    (with tmpfile (url-temp)
      (load-buffer path)
      (buffer-export path tmpfile "latex")
      (string-load tmpfile))))
  
(define (load-latex path)
  (with path (string-append "$TEXMACS_PATH/tests/tex/" path)
    (string-replace (string-load path)  "\r\n" "\n")))

(define (convert-latex-to-tmu path)
  (with path (string-append "$TEXMACS_PATH/tests/tex/" path)
    (with latex-content (string-load path)
      (with texmacs-tree (cpp-latex-document->texmacs latex-content #f)
        (serialize-tmu texmacs-tree)))))



(define (test_203_5_latex_to_tmu)
  (display "Testing LaTeX to TMU conversion...\n")
  (with tmu-content (convert-latex-to-tmu "203_5_command_line.tex")
    (display "Converted TMU content:\n")
    (display tmu-content)
    (display "\n")
    (check (string-contains? tmu-content "Test Document") => #t)
    (check (string-contains? tmu-content "Test Author") => #t)
    (check (string-contains? tmu-content "Introduction") => #t)
    (check (string-contains? tmu-content "LaTeX") => #t)
    (check (string-contains? tmu-content "TMU format") => #t)
    (check (string-contains? tmu-content "x<rsup|2>+y<rsup|2>=z<rsup|2>") => #t)
    (check (string-starts? tmu-content "<TMU|") => #t))
  (check-report))

(define (test_203_5)
  (test_203_5_latex_to_tmu))
