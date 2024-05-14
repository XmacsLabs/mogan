
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : s7-lang.scm
;; DESCRIPTION : the S7 Scheme Language
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code s7-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "s7") (== key "keyword")))
  `(,(string->symbol key)
    (constant
      "#t" "#f" "pi")
    (declare_type "define" "set!" "lambda")
    ;(declare_module "pkg")
    (keyword_conditional
      "if")))
    ;(keyword_control
    ;  "catch" "try")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "s7") (== key "operator")))
  `(,(string->symbol key)
    (operator ";" ":" "=" "+" "-")
    (operator_special "@" "," "'" "`")
    ;(operator_field ".")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(define (s7-number-suffix)
  `(suffix
    (imaginary "j" "J")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "s7") (== key "number")))
  `(,(string->symbol key)
    (bool_features
     "prefix_0x" "prefix_0b"
     "sci_notation")
    (separator "_")
    ,(s7-number-suffix)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "s7") (== key "string")))
  `(,(string->symbol key)
    (bool_features 
     "hex_with_8_bits" "hex_with_16_bits"
     "hex_with_32_bits" "octal_upto_3_digits")
    (escape_sequences "\\" "\"" "a" "b" "f" "n" "r" "t" "v")))

; See: https://s7.org/doc/v6.1.0/Single-Line-Comments.html
(tm-define (parser-feature lan key)
  (:require (and (== lan "s7") (== key "comment")))
  `(,(string->symbol key)
    (inline ";")))

(define (notify-s7-syntax var val)
  (syntax-read-preferences "s7"))

(define-preferences
  ("syntax:s7:none" "red" notify-s7-syntax)
  ("syntax:s7:comment" "brown" notify-s7-syntax)
  ("syntax:s7:keyword" "#309090" notify-s7-syntax)
  ("syntax:s7:error" "dark red" notify-s7-syntax)
  ("syntax:s7:constant_number" "#4040c0" notify-s7-syntax)
  ("syntax:s7:constant_string" "dark grey" notify-s7-syntax)
  ("syntax:s7:constant_char" "#333333" notify-s7-syntax)
  ("syntax:s7:variable_identifier" "#204080" notify-s7-syntax)
  ("syntax:s7:declare_category" "#d030d0" notify-s7-syntax))
