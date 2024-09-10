
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : default-ast-lang.scm
;; DESCRIPTION : the default AST Language
;; COPYRIGHT   : (C) 2024  UnbSky
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code default-ast-lang))

(tm-define (parser-feature lan key)
  `(,(string->symbol key)))

(tm-define (parser-feature lan key)
  (:require (== key "id"))
  `(,(string->symbol key)
    ,lan))

(tm-define (parser-feature lan key)
  (:require (== key "special_symbol"))
  `(,(string->symbol key)))

;; 3 is the color cycle of brackets
;; the depth start count at 0, the depth for ((())) is 0,1,2
;; When depth is 0, 3, 6 ... , the type is "(0" ")0"
;; When depth is 1, 4, 7 ... , the type is "(1" ")1"
;; When depth is 2, 5, 8 ... , the type is "(2" ")2"
;; The cycle period of the colors can be customized by changing the first number.
;; Different depths of bracket colors can be defined in <lang>-ast-lang.scm.
(tm-define (parser-feature lan key)
  (:require (== key "brackets"))
  `(,(string->symbol key)
    (3 "(" ")")
    (3 "[" "]")
    (3 "{" "}")))
