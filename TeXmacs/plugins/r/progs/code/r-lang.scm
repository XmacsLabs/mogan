;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : r-lang.scm
;; DESCRIPTION : R Language (syntax highlighting / parser features)
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code r-lang)
  (:use (prog default-lang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords / identifiers
;;
;; R is dynamically typed; there is no built-in "type declaration" keyword set
;; like in Rust/MoonBit/etc. The highlighting should focus on:
;; - control flow
;; - function definition
;; - special constants
;; - common reserved words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parser-feature lan key)
  (:require (and (== lan "r") (== key "keyword")))
  `(,(string->symbol key)

    ;; R logical / null / missingness / special numeric constants
    (constant
      "TRUE" "FALSE" "T" "F"
      "NULL"
      "NA" "NA_integer_" "NA_real_" "NA_complex_" "NA_character_"
      "NaN" "Inf")

    ;; Control flow keywords
    (keyword_conditional
      "if" "else" "repeat" "while" "for" "in" "next" "break")

    ;; Function and evaluation related
    (declare_function
      "function" "return")

    ;; Common reserved / special words used in language & formula context
    ;; Note: `...` is not a normal identifier but is widely used in signatures.
    (keyword
      "..." "library" "require" "attach" "detach"
      "setwd" "getwd" "source")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operators
;;
;; R has:
;; - assignment: <-, <<-, ->, ->>, =
;; - arithmetic: + - * / ^ %% %/%
;; - matrix: %*%
;; - comparison: == != < <= > >=
;; - logical: ! & && | ||
;; - sequence: :
;; - namespace: :: :::
;; - member: $ @
;; - formula/tilde: ~
;; - help: ?
;; - pipe (base R 4.1+): |>
;; - user-defined infix: %...% (e.g. %in%, %>% from magrittr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parser-feature lan key)
  (:require (and (== lan "r") (== key "operator")))
  `(,(string->symbol key)

    ;; Arithmetic & special infix operators
    (operator
      "+" "-" "*" "/" "^"
      "%%" "%/%" "%*%"
      ":")

    ;; Comparison
    (operator
      "==" "!=" "<" "<=" ">" ">=")

    ;; Logical
    (operator
      "!" "&" "&&" "|" "||")

    ;; Assignment
    (operator_assignment
      "<-" "<<-" "->" "->>" "=")

    ;; Namespace / access / misc
    (operator_special
      "::" ":::"
      "$" "@"
      "~" "?"
      "|>"
      "," ";" ".")

    ;; Brackets / braces / parentheses
    ;; R has both [] and [[]]
    (operator_openclose
      "{" "}" "(" ")" "[" "]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers
;;
;; R numeric literals:
;; - decimal: 1, 1.0, .5, 1.
;; - scientific: 1e3, 1E-3
;; - hex: 0x1f, 0XFF
;; - integer suffix: 1L (ONLY L / l, commonly L)
;; - complex: 1i, 2.3i, 1+2i (lexing usually highlights trailing i)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (r-number-suffix)
  `(suffix
    ;; R integer literal suffix
    (integer "L" "l")
    ;; R imaginary unit suffix for complex literals (common)
    (imag "i")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "r") (== key "number")))
  `(,(string->symbol key)
    (bool_features
      ;; Hex prefix: 0x / 0X
      "prefix_0x"
      ;; Scientific notation: e/E
      "sci_notation")
    ,(r-number-suffix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;
;; R strings can be single-quoted or double-quoted.
;; Escapes:
;; - \\ \" \' \n \r \t \b \f \a \v
;; - \xhh (hex byte)
;; - \uXXXX \UXXXXXXXX (unicode)
;; R also has raw string syntax in some contexts? (not standard like Python),
;; so we keep standard escapes only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parser-feature lan key)
  (:require (and (== lan "r") (== key "string")))
  `(,(string->symbol key)
    (bool_features
      ;; Unicode escapes: \uXXXX and \UXXXXXXXX are common in R
      "unicode_escape"
      ;; Hex byte escapes: \xhh
      "hex_escape")
    (escape_sequences
      "\\" "\"" "'"
      "a" "b" "f" "n" "r" "t" "v"
      "x" "u" "U")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes
;; - Comments in R start with '#'. (Usually handled by generic lexer, but if you
;;   later add comment features, '#' is the marker.)
;; - Infix operators of the form %...% (e.g. %in%, %>% ) are user-definable.
;;   If your lexer supports a "percent operator" feature, you can add it later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parser-feature lan key)
  (:require (and (== lan "r") (== key "comment")))
  `(,(string->symbol key)
    (inline "#")))  ;; Line comments start with #

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-r-syntax var val)
  (syntax-read-preferences "r"))

(define-preferences
  ("syntax:r:none" "red" notify-r-syntax)
  ("syntax:r:comment" "brown" notify-r-syntax)
  ("syntax:r:error" "dark red" notify-r-syntax)
  ("syntax:r:constant" "#4040c0" notify-r-syntax)
  ("syntax:r:constant_number" "#3030b0" notify-r-syntax)
  ("syntax:r:constant_string" "dark grey" notify-r-syntax)
  ("syntax:r:constant_char" "#333333" notify-r-syntax)
  ("syntax:r:declare_function" "#0000c0" notify-r-syntax)
  ("syntax:r:declare_type" "#0000c0" notify-r-syntax)
  ("syntax:r:declare_module" "#0000c0" notify-r-syntax)
  ("syntax:r:operator" "#8b008b" notify-r-syntax)
  ("syntax:r:operator_openclose" "#B02020" notify-r-syntax)
  ("syntax:r:operator_field" "#888888" notify-r-syntax)
  ("syntax:r:operator_special" "orange" notify-r-syntax)
  ("syntax:r:keyword" "#309090" notify-r-syntax)
  ("syntax:r:keyword_conditional" "#309090" notify-r-syntax)
  ("syntax:r:keyword_control" "#008080ff" notify-r-syntax))
