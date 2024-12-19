
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : elvish-lang.scm
;; DESCRIPTION : Elvish language syntax
;; COPYRIGHT   : (C) 2024  Liii Network Inc
;; All rights reverved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code elvish-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "elvish") (== key "keyword")))
  `(,(string->symbol key)
    (constant "$true" "$false")
    (declare_type
     "var" "set")
    (keyword
     "all" "assoc" "base" "benchmark" "bool" "break"
     "call" "cd" "compact" "compare" "conj" "constantly"
     "continue" "count" "defer" "deprecate" "dissoc" "drop"
     "each" "eawk" "echo" "eq" "eval" "exact-num"
     "exec" "exit" "external" "fail" "from-json" "from-lines"
     "from-terminated" "get-env" "has-env" "has-external" "has-key" "has-value"
     "is" "keys" "kind-of" "make-map" "nop" "not"
     "not-eq" "ns" "num" "one" "only-bytes"
     "only-values" "order" "peach" "pprint" "print" "printf"
     "put" "rand" "randint" "range" "read-bytes" "read-line"
     "read-upto" "render-styledown" "repeat" "repr" "resolve" "return"
     "run-parallel" "search-external" "set-env" "show" "sleep" "slurp"
     "src" "styled" "styled-segment" "take" "tilde-abbr" "time"
     "to-json" "to-lines" "to-string" "to-terminated" "unset-env" "use-mod"
     "wcswidth")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "elvish") (== key "comment")))
  `(,(string->symbol key)
    (inline "#")))

(define (notify-elvish-syntax var val)
  (syntax-read-preferences "elvish"))

(define-preferences
  ("syntax:elvish:none" "red" notify-elvish-syntax)
  ("syntax:elvish:comment" "brown" notify-elvish-syntax)
  ("syntax:elvish:keyword" "dark green" notify-elvish-syntax)
  ("syntax:elvish:declare_type" "#0000c0" notify-elvish-syntax))

