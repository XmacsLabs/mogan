
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : octave.scm
;; DESCRIPTION : prog format for octave
;; COPYRIGHT   : (C) 2022  Darcy Shen, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data octave))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; octave source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format octave
  (:name "Octave source code")
  (:suffix "m"))
  
(define (texmacs->octave x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (octave->texmacs x . opts)
  (code->texmacs x))

(define (octave-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree octave-document
  (:function texmacs->octave))

(converter octave-document texmacs-tree
  (:function octave->texmacs))
  
(converter texmacs-tree octave-snippet
  (:function texmacs->octave))

(converter octave-snippet texmacs-tree
  (:function octave-snippet->texmacs))

