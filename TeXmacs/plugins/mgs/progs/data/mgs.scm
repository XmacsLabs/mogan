
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : data/mgs.scm
;; DESCRIPTION : Mogan Scheme (.mgs) data format
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data mgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme format for TeXmacs (no information loss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mgs-recognizes? s)
  (and (string? s) (string-starts? s "(document (TeXmacs")))

(define-format mgs
  (:name "Mogan Scheme")
  (:suffix "mgs")
  (:must-recognize mgs-recognizes?))

(define (texmacs->mgs t)
  (texmacs->stm (cork-tree->u8-tree t)))

(define (mgs->texmacs text)
  (u8-tree->cork-tree (stm->texmacs text)))

(define (mgs-snippet->texmacs text)
  (u8-tree->cork-tree (stm-snippet->texmacs text)))

(converter texmacs-tree mgs-document
  (:function texmacs->mgs))

(converter mgs-document texmacs-tree
  (:function mgs->texmacs))

(converter texmacs-tree mgs-snippet
  (:function texmacs->mgs))

(converter mgs-snippet texmacs-tree
  (:function mgs-snippet->texmacs))
