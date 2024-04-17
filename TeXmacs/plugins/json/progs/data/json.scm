
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-data_json.scm
;; DESCRIPTION : json data format
;; COPYRIGHT   : (C) 2022  Darcy Shen, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data json))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format json
  (:name "JSON")
  (:suffix "json"))

(define (texmacs->json x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (json->texmacs x . opts)
  (code->texmacs x))

(define (json-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree json-document
  (:function texmacs->json))

(converter json-document texmacs-tree
  (:function json->texmacs))
  
(converter texmacs-tree json-snippet
  (:function texmacs->json))

(converter json-snippet texmacs-tree
  (:function json-snippet->texmacs))

