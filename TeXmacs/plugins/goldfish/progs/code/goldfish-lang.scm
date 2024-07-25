
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

(define (srfi-1)
  (list
    "srfi-1" ; List Library
    ; SRFI-1: List constructors
    "list" "cons" "xcons" "cons*" "make-list"
    "list-tabulate" "list-copy" "circular-list" "iota"
    ; SRFI-1: List predicates
    "pair?" "null?" "proper-list?" "circular-list?" "dotted-list?"
    "not-pair?" "null-list?" "list=" "list?"
    ; SRFI-1: List selectors
    "car" "caar" "cdar" "cadr" "caddr" "cadddr"
    "cdr" "cddr" "cdddr" "cddddr"
    "list-ref"
    "first" "second" "third" "fourth" "fifth"
    "sixth" "seventh" "eighth" "ninth" "tenth"
    "take" "drop" "take-right" "drop-right" "last"
    ; SRFI-1: MISC
    "concatenate" "reverse" "append-reverse" "zip" "count"
    ; SRFI-1: fold, unfold, map
    "fold" "fold-right" "reduce" "reduce-right" "map"
    "unfold" "unfold-right" "for-each" "map-in-order"
    ; SRFI-1: Filtering & Parititioning
    "filter" "partition" "remove"
    ; SRFI-1: Searching
    "find" "find-tail" "take-while" "drop-while" "span"
    "any" "every" "list-index" "member" "memq" "memv"
    ; SRFI-1: Deletion
    "delete" "delete-duplicates"))

(define (srfi-8)
  (list
    "srfi-8"
    "call-with-values" "receive"))

(define (srfi-13)
  (list
    "srfi-13" ; String Library
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
    "string-reverse" "string-append"
    ; SRFI-13: Functional programming
    "string-map" "string-fold" "string-fold-right" "string-for-each" "string-for-each-index"
    ; SRFI-13: String insertion and parsing
    "string-replace" "string-tokenize"
    ; SRFI-13: Filtering & Deleting
    "string-filter" "string-delete"))

(define (srfi-70)
  (list
    "srfi-70" ; Numbers
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
    "angle" "exact->inexact" "inexact->exact" "string->number" "number->string"))

(define (srfi-78)
  (list
    "srfi-78" ; Light-weighted Test framework
    "check" "check-set-mode!" "check-report" "check-reset!"))

(tm-define (parser-feature lan key)
  (:require (and (== lan "s7") (== key "keyword")))
  `(,(string->symbol key)
    (extra_chars "?" "+" "-" "." "!" "*" ">" "=" "<" "#")
    (constant
      "pi" "+inf.0" "-inf.0" "+nan.0" "#t" "#true" "#f" "#false"
      "*stdin*" "*stdout*" "*stderr*"
      "*load-hook*" "*autoload-hook*" "*error-hook*" "*read-error-hook*"
      "*rootlet-redefinition-hook*" "*unbound-variable-hook*"
      "*missing-close-paren-hook*")
    (declare_type
     "define" "defined?" "set!" "lambda" "define-macro"
     "define-constant" "let" "let*" "apply" "eval"
     "load" "eval" "eval-string" "values" "autoload" "require" "provide")
    (keyword
     "eq?" "equal?" "equivalent?" "help" "display"
     "quote" "quasiquote" "unquote"
     "bignum" "length" "append" "procedure-source"

     ; S7 built-ins
     "*load-path*" "*s7*" "*features*" "*libraries*"
     "*cload-directory*" "*#readers*"

     ,@(srfi-1) ,@(srfi-8) ,@(srfi-13) ,@(srfi-70) ,@(srfi-78)

     ; SRFI-60: Integers as Bits
     "logand" "logior" "logxor" "lognot" "logand"
     "logbit?" "ash"
     ; MISC
     "integer-decode-float" "random" "nan?" "nan" "nan-payload"
     "make-vector" "vector-length" "vector" "format" "object->string"
     "vector-set!" "immutable!" "immutable?" "make-hash-table" "hash-table"
     "hash-table?" "hash-table-ref" "hash-table-set!" "hash-table-entries" "hash-code")
    (keyword_error
     "syntax-error" "wrong-type-arg" "immutable-error" "out-of-range" "division-by-zero"
     "unbound-variable" "read-error" "format-error" "missing-method" "out-of-memory"
     "bad-result" "no-catch" "wrong-number-of-args" "io-error" "bignum-error")
    (keyword_conditional
     "if" "cond" "else" "case")
    (keyword_control
     "begin" "error" "catch" "throw")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "s7") (== key "operator")))
  `(,(string->symbol key)
    (operator "and" "or" "not" "=" "+" "-" "*" "/" "=>" "->")
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
  ("syntax:s7:keyword_error" "dark red" notify-s7-syntax)
  ("syntax:s7:constant_number" "#4040c0" notify-s7-syntax)
  ("syntax:s7:constant_string" "dark grey" notify-s7-syntax)
  ("syntax:s7:constant_char" "#333333" notify-s7-syntax)
  ("syntax:s7:operator_special" "dark magenta" notify-s7-syntax)
  ("syntax:s7:operator_openclose" "dark" notify-s7-syntax)
  ("syntax:s7:variable_identifier" "#204080" notify-s7-syntax)
  ("syntax:s7:declare_category" "#d030d0" notify-s7-syntax))
