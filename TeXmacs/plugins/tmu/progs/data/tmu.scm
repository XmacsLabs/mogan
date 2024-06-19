
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
  (:suffix "tmu" "tsu"))

(converter tmu-document texmacs-tree
  (:function parse-tmu))

(converter texmacs-tree tmu-document
  (:function serialize-tmu))

(converter tmu-snippet texmacs-tree
  (:function parse-tmu-snippet))

(converter texmacs-tree tmu-snippet
  (:function serialize-tmu))
