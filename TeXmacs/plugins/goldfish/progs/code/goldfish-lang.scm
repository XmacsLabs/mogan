
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : goldfish-lang.scm
;; DESCRIPTION : the Goldfish Scheme Language
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code goldfish-lang)
  (:use (prog default-lang)
        (code r7rs-keyword)
        (code srfi-keyword)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "goldfish") (== key "keyword")))
  `(,(string->symbol key)
    (extra_chars "?" "+" "-" "." "!" "*" ">" "=" "<" "#")
    (constant
      ,@(r7rs-keywords-constant)
      "pi" "*stdin*" "*stdout*" "*stderr*"
      "*load-hook*" "*autoload-hook*" "*error-hook*" "*read-error-hook*"
      "*rootlet-redefinition-hook*" "*unbound-variable-hook*"
      "*missing-close-paren-hook*")
    (declare_type
      ,@(r7rs-keywords-define)
      "defined?" "define-macro" "define-constant" "autoload" "require" "provide" "define*" "lambda*")
    (keyword
      ,@(r7rs-keywords-others) ,@(srfi-1-keywords) ,@(srfi-8-keywords) ,@(srfi-13-keywords) ,@(srfi-60-keywords) ,@(srfi-78-keywords)

      ; S7 built-ins
      "*load-path*" "*goldfish*" "*features*" "*libraries*"
      "*cload-directory*" "*#readers*"

      "help" "bignum" "append" "procedure-source"

      ; MISC
      "integer-decode-float" "random" "nan" "nan-payload" "format" "object->string" "immutable!" "immutable?" "make-hash-table" "hash-table" "hash-table?" "hash-table-ref" "hash-table-set!" "hash-table-entries" "hash-code")
    (keyword_error
      "syntax-error" "wrong-type-arg" "immutable-error" "out-of-range" "division-by-zero"
      "unbound-variable" "read-error" "format-error" "missing-method" "out-of-memory"
      "bad-result" "no-catch" "wrong-number-of-args" "io-error" "bignum-error")
    (keyword_conditional ,@(r7rs-keywords-branch))
    (keyword_control ,@(r7rs-keywords-exception))))

(tm-define (parser-feature lan key)
  (:require (and (== lan "goldfish") (== key "operator")))
  `(,(string->symbol key)
    (operator "=" "+" "-" "*" "/" "=>" "->")
    (operator_special "@" "," "'" "`")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(define (goldfish-number-suffix)
  `(suffix
    (imaginary "i")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "goldfish") (== key "number")))
  `(,(string->symbol key)
    (bool_features "prefix_#")
    (separator "_")
    ,(goldfish-number-suffix)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "goldfish") (== key "string")))
  `(,(string->symbol key)
    (bool_features
     "hex_with_8_bits" "hex_with_16_bits"
     "hex_with_32_bits" "octal_upto_3_digits")
    (escape_sequences "\\" "\"" "a" "b" "f" "n" "r" "t" "v")
    (pairs "\"")))

; See: https://goldfish.org/doc/v6.1.0/Single-Line-Comments.html
(tm-define (parser-feature lan key)
  (:require (and (== lan "goldfish") (== key "comment")))
  `(,(string->symbol key)
    (inline ";")))

(define (notify-goldfish-syntax var val)
  (syntax-read-preferences "goldfish"))

(define-preferences
  ("syntax:goldfish:none" "red" notify-goldfish-syntax)
  ("syntax:goldfish:comment" "brown" notify-goldfish-syntax)
  ("syntax:goldfish:declare_type" "#309090" notify-goldfish-syntax)
  ("syntax:goldfish:keyword_conditional" "#309090" notify-goldfish-syntax)
  ("syntax:goldfish:keyword_control" "#309090" notify-goldfish-syntax)
  ("syntax:goldfish:keyword" "#204080" notify-goldfish-syntax)
  ("syntax:goldfish:keyword_error" "dark red" notify-goldfish-syntax)
  ("syntax:goldfish:constant_number" "#4040c0" notify-goldfish-syntax)
  ("syntax:goldfish:constant_string" "dark grey" notify-goldfish-syntax)
  ("syntax:goldfish:constant_char" "#333333" notify-goldfish-syntax)
  ("syntax:goldfish:operator_special" "dark magenta" notify-goldfish-syntax)
  ("syntax:goldfish:operator_openclose" "dark" notify-goldfish-syntax)
  ("syntax:goldfish:variable_identifier" "#204080" notify-goldfish-syntax)
  ("syntax:goldfish:declare_category" "#d030d0" notify-goldfish-syntax))
