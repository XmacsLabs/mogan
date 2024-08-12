
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : data/tmu.scm
;; DESCRIPTION : tmu data format
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data tmu))

(define-format tmu 
  (:name "TMU")
  (:suffix "tmu"))

(define (texmacs->tmu t)
  (serialize-tmu (cork-tree->utf8-tree t)))

(define (tmu->texmacs t)
  (utf8-tree->cork-tree (parse-tmu t)))

(define (tmu-snippet->texmacs t)
  (utf8-tree->cork-tree (parse-tmu-snippet t)))

(converter tmu-document texmacs-tree
  (:function tmu->texmacs))

(converter texmacs-tree tmu-document
  (:function texmacs->tmu))

(converter tmu-snippet texmacs-tree
  (:function tmu-snippet->texmacs))

(converter texmacs-tree tmu-snippet
  (:function texmacs->tmu))
