
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : prog-drd.scm
;; DESCRIPTION : data relation definitions for prog mode
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog prog-drd))

;; Code fragments

(define-group code-tag
  (inline-code-tag) (block-code-tag))

(define-group inline-code-tag
  verbatim scm cpp dot-lang mmx r fortran octave
  python julia java javascript json scala scilab shell)

(define-group block-code-tag
  verbatim-code scm-code cpp-code dot-code mmx-code r-code fortran-code
  octave-code python-code julia-code java-code javascript-code json-code scala-code
  scilab-code shell-code)

