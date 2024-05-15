
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
    (extra_chars "?" "-" "!" "*" ">" "=" "<")
    (constant "pi")
    (declare_type
     "define" "set!" "lambda" "define-macro" "define-constant" "let")
    (keyword
     "eq?" "bignum" "length" "append" "procedure-source"
     ; SRFI-1: List constructors
     "list" "cons" "xcons" "cons*" "make-list"
     "list-tabulate" "list-copy" "circular-list" "iota"
     ; SRFI-1: List predicates
     "pair?" "null?" "proper-list?" "circular-list?" "dotted-list?"
     "not-pair?" "null-list?" "list="
     ; SRFI-1: List selectors
     "car" "cdr" "caar" "cadr" "list-ref"
     "first" "second" "third" "fourth" "fifth"
     "take" "drop" "take-right" "drop-right" "last"
     ; SRFI-1: MISC
     "concatenate" "reverse" "append-reverse" "zip" "count"
     ; SRFI-1: fold, unfold, map
     "fold" "fold-right" "reduce" "reduce-right" "map"
     "unfold" "unfold-right" "for-each" "map-in-order"
     ; SRFI-1: Filtering & Parititioning
     "filter" "parition" "remove"
     ; SRFI-1: Searching
     "find" "find-tail" "take-while" "drop-while" "span"
     "any" "every" "list-index" "member" "memq" "memv"
     ; SRFI-1: Deletion
     "delete" "delete-duplicates"
     ; SRFI-13: String predicates
     "string?" "string-null?" "string-every" "string-any"
     ; SRFI-13: String constructors
     "make-string" "string" "string-tabulate"
     ; SRFI-13: List & String Conversion
     "string->list" "list->string" "reverse-list->string" "string-join"
     ; SRFI-13: String selection
     "string-length" "string-ref" "string-copy" "substring" "string-copy!"
     "string-take" "string-take-right" "string-drop" "string-drop-right" "string-pad"
     "string-pad-right" "string-trim" "string-trim-right" "string-trim-both"
     ; SRFI-13: String comparison
     "string-compare" "string=" "string<>"
     ; SRFI-13: String Prefixes & Suffixes
     "string-prefix?" "string-suffix?"
     ; SRFI-13: String searching
     "string-index" "string-index-right" "string-skip" "string-skip-right" "string-count"
     "string-contains"
     ; SRFI-13: String insertion and parsing
     "string-replace" "string-tokenize"
     ; SRFI-13: Filtering & Deleting
     "string-filter" "string-delete"
     ; SRFI-60: Integers as Bits
     "logand" "logior" "logxor" "lognot" "logand"
     "logbit?" "ash"
     ; SRFI-70: Numbers
     "number?" "complex?" "real?" "rational?" "integer?"
     "exact?" "inexact?" "finite?" "infinite?" "zero?"
     "positive?" "negative?" "odd?" "even?" "floor?"
     "max" "min" "abs" "quotient" "remainder" "modulo"
     "gcd" "lcm" "numerator" "denominator" "floor"
     "ceiling" "truncate" "round" "rationalize"
     "expt" "log" "complex" "real-part" "imag-part"
     "sin" "cos" "tan" "asin" "acos" "atan"
     "sinh" "cosh" "tanh" "asinh" "acosh" "atanh"
     "sqrt" "expt" "make-rectangular" "make-polar" "magnitude"
     "angle" "exact->inexact" "inexact->exact" "string->number" "number->string"
     ; MISC
     "integer-decode-float" "random" "nan?" "nan" "nan-payload")
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
    (imaginary "i")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "s7") (== key "number")))
  `(,(string->symbol key)
    (bool_features "prefix_#")
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
