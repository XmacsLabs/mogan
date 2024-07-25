
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : srfi-keyword.scm
;; DESCRIPTION : the Scheme Keyword defined in R7RS
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code srfi-keyword))

(tm-define (srfi-1-keywords)
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

(tm-define (srfi-8-keywords)
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

(tm-define (srfi-70-keywords)
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

(tm-define (srfi-78-keywords)
  (list
    "srfi-78" ; Light-weighted Test framework
    "check" "check-set-mode!" "check-report" "check-reset!"))
