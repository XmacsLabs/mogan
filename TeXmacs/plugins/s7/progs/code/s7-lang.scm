
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
    (declare_type
     "define" "set!" "lambda" "define-macro" "define-constant" "let")
    (keyword
     "bignum" "string->number" "number->string" "string?" "length"
     "append" "map" "for-each" "list" "cons" "cdr"  "procedure-source"
     "sin" "cos" "sinh" "cosh" "tanh" "asinh" "acosh" "atanh" "expt" "sqrt"
     "logior" "logxor" "logand" "lognot" "logbit?" "ash" "integer-decode-float"
     "random" "nan?" "infinite?" "nan" "nan-payload"
     "exact?" "rational?" "rationalize" "floor?" "remainder" "modulo" "lcm" "log"
     "complex" "integer?")
    (keyword_conditional
      "if" "cond" "else")
    (keyword_control
      "begin" "error" "catch" "throw")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "s7") (== key "operator")))
  `(,(string->symbol key)
    (operator "and" "or" "not" "=" "+" "-" "*" "/" )
    (operator_special "@" "," "'" "`")
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
    (escape_sequences "\\" "\"" "a" "b" "f" "n" "r" "t" "v")
    (pairs "\"")))

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
  ("syntax:s7:declare_type" "#309090" notify-s7-syntax)
  ("syntax:s7:keyword_conditional" "#309090" notify-s7-syntax)
  ("syntax:s7:keyword_control" "#309090" notify-s7-syntax)
  ("syntax:s7:keyword" "#204080" notify-s7-syntax)
  ("syntax:s7:error" "dark red" notify-s7-syntax)
  ("syntax:s7:constant_number" "#4040c0" notify-s7-syntax)
  ("syntax:s7:constant_string" "dark grey" notify-s7-syntax)
  ("syntax:s7:constant_char" "#333333" notify-s7-syntax)
  ("syntax:s7:operator_special" "dark magenta" notify-s7-syntax)
  ("syntax:s7:variable_identifier" "#204080" notify-s7-syntax)
  ("syntax:s7:declare_category" "#d030d0" notify-s7-syntax))
