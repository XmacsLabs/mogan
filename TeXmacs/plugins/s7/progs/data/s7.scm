
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : s7.scm
;; DESCRIPTION : prog format for s7
;; COPYRIGHT   : (C) 2022  Darcy Shen, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data s7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s7 source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format s7
  (:name "S7 source code")
  (:suffix "scm"))
  
(define (texmacs->s7 x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (s7->texmacs x . opts)
  (code->texmacs x))

(define (s7-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree s7-document
  (:function texmacs->s7))

(converter s7-document texmacs-tree
  (:function s7->texmacs))
  
(converter texmacs-tree s7-snippet
  (:function texmacs->s7))

(converter s7-snippet texmacs-tree
  (:function s7-snippet->texmacs))
