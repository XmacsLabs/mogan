
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-prog_dot.scm
;; DESCRIPTION : prog format for DOT
;; COPYRIGHT   : (C) 2023  Darcy Shen, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-format dot
  (:name "DOT Source Code")
  (:suffix "gv"))

(define (texmacs->dot x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (dot->texmacs x . opts)
  (code->texmacs x))

(define (dot-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree dot-document
  (:function texmacs->dot))

(converter dot-document texmacs-tree
  (:function dot->texmacs))
  
(converter texmacs-tree dot-snippet
  (:function texmacs->dot))

(converter dot-snippet texmacs-tree
  (:function dot-snippet->texmacs))
