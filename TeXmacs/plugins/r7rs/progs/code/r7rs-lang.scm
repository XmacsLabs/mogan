
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : r7rs-lang.scm
;; DESCRIPTION : the Scheme Language defined in R7RS
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code r7rs-lang)
  (:use (prog default-lang)))

(define (r7rs-scheme-base)
  (map symbol->string
    '(abs and append apply assoc assq assv begin binary-port? boolean=? boolean? bytevector bytevector-append bytevector-copy bytevector-copy! bytevector-length bytevector-u8-ref bytevector-u8-set! bytevector? caar cadr call-with-current-continuation call-with-port call-with-values call/cc car case cdar cddr cdr ceiling char->integer char-ready? char<=? char<? char=? char>=? char>? char? close-input-port close-output-port close-port complex? cond cond-expand cons current-error-port current-input-port current-output-port define define-record-type define-syntax define-values denominator do dynamic-wind else eof-object eof-object? eq? equal? eqv? error error-object-irritants error-object-message error-object? even? exact exact-integer-sqrt exact-integer? exact? expt features file-error? floor floor-quotient floor-remainder floor/ flush-output-port for-each gcd get-output-bytevector get-output-string guard if include include-ci inexact inexact? input-port-open? input-port? integer->char integer? lambda lcm length let let* let*-values let-syntax let-values letrec letrec* letrec-syntax list list->string list->vector list-copy list-ref list-set! list-tail list? make-bytevector make-list make-parameter make-string make-vector map max member memq memv min modulo negative? newline not null? number->string number? numerator odd? open-input-bytevector open-input-string open-output-bytevector open-output-string or output-port-open? output-port? pair? parameterize peek-char peek-u8 port? positive? procedure? quasiquote quote quotient raise raise-continuable rational? rationalize read-bytevector read-bytevector! read-char read-error? read-line read-string read-u8 real? remainder reverse round set! set-car! set-cdr! sin sqrt string string->list string->number string->symbol string->utf8 string->vector string-append string-copy string-copy! string-fill! string-for-each string-length string-map string-ref string-set! string<=? string<? string=? string>=? string>? string? substring symbol->string symbol=? symbol? syntax-error syntax-rules textual-port? truncate truncate-quotient truncate-remainder truncate/ u8-ready? unless unquote unquote-splicing utf8->string values vector vector->list vector->string vector-append vector-copy vector-copy! vector-fill! vector-for-each vector-length vector-map vector-ref vector-set! vector? when with-exception-handler write-bytevector write-char write-string write-u8 zero?)))

(define (r7rs-scheme-case-lambda)
  (map symbol->string '(case-lambda)))

(define (r7rs-scheme-char)
  (map symbol->string
    '(char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase char-foldcase char-lower-case? char-numeric? char-upcase char-upper-case? char-whitespace? digit-value string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? string-downcase string-foldcase string-upcase)))

(define (r7rs-scheme-complex)
  (map symbol->string
    '(angle imag-part magnitude make-polar make-rectangular real-part)))

(define (r7rs-scheme-CxR)
  (map symbol->string
    '(caaaar caaadr caaar caadar caaddr caadr cadaar cadadr cadar caddar cadddr caddr cdaaar cdaadr cdaar cdadar cdaddr cdadr cddaar cddadr cddar cdddar cddddr cdddr)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "r7rs") (== key "keyword")))
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
     ,@(r7rs-scheme-base) ,@(r7rs-scheme-case-lambda) ,@(r7rs-scheme-char)
     ,@(r7rs-scheme-complex) ,@(r7rs-scheme-CxR)
    )
    (keyword_error
     "syntax-error" "wrong-type-arg" "immutable-error" "out-of-range" "division-by-zero"
     "unbound-variable" "read-error" "format-error" "missing-method" "out-of-memory"
     "bad-result" "no-catch" "wrong-number-of-args" "io-error" "bignum-error")
    (keyword_conditional
     "if" "cond" "else" "case")
    (keyword_control
     "begin" "error" "catch" "throw")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "r7rs") (== key "operator")))
  `(,(string->symbol key)
    (operator "and" "or" "not" "=" "+" "-" "*" "/" "=>" "->")
    (operator_special "@" "," "'" "`")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(define (r7rs-number-suffix)
  `(suffix
    (imaginary "i")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "r7rs") (== key "number")))
  `(,(string->symbol key)
    (bool_features "prefix_#")
    (separator "_")
    ,(r7rs-number-suffix)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "r7rs") (== key "string")))
  `(,(string->symbol key)
    (bool_features 
     "hex_with_8_bits" "hex_with_16_bits"
     "hex_with_32_bits" "octal_upto_3_digits")
    (escape_sequences "\\" "\"" "a" "b" "f" "n" "r" "t" "v")
    (pairs "\"")))

; See: https://r7rs.org/doc/v6.1.0/Single-Line-Comments.html
(tm-define (parser-feature lan key)
  (:require (and (== lan "r7rs") (== key "comment")))
  `(,(string->symbol key)
    (inline ";")))

(define (notify-r7rs-syntax var val)
  (syntax-read-preferences "r7rs"))

(define-preferences
  ("syntax:r7rs:none" "red" notify-r7rs-syntax)
  ("syntax:r7rs:comment" "brown" notify-r7rs-syntax)
  ("syntax:r7rs:declare_type" "#309090" notify-r7rs-syntax)
  ("syntax:r7rs:keyword_conditional" "#309090" notify-r7rs-syntax)
  ("syntax:r7rs:keyword_control" "#309090" notify-r7rs-syntax)
  ("syntax:r7rs:keyword" "#204080" notify-r7rs-syntax)
  ("syntax:r7rs:keyword_error" "dark red" notify-r7rs-syntax)
  ("syntax:r7rs:constant_number" "#4040c0" notify-r7rs-syntax)
  ("syntax:r7rs:constant_string" "dark grey" notify-r7rs-syntax)
  ("syntax:r7rs:constant_char" "#333333" notify-r7rs-syntax)
  ("syntax:r7rs:operator_special" "dark magenta" notify-r7rs-syntax)
  ("syntax:r7rs:operator_openclose" "dark" notify-r7rs-syntax)
  ("syntax:r7rs:variable_identifier" "#204080" notify-r7rs-syntax)
  ("syntax:r7rs:declare_category" "#d030d0" notify-r7rs-syntax))
