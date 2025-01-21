;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (liii json)
(import (liii base) (guenchi json))
(export
  json json-null json-true json-false json-string json-parse
  json-string-escape json-string-unescape string->json json->string
  json-ref json-ref*
  json-set json-set* json-push json-push* json-drop json-drop* json-reduce json-reduce*)
(begin

(define-case-class json ((data any?))

(define (%get)
  data)

(typed-define (%get-string (key any?) (default string?))
  (let1 r (json-ref data key)
    (if (string? r) r default)))

(typed-define (%get-number (key any?) (default number?))
  (let1 r (json-ref data key)
    (if (number? r) r default)))

(define (%apply x . xs)
  (json (apply json-ref* (cons data (cons x xs)))))

(define (%null?)
  (eq? data 'null))

(define (%object?)
  (and (list? data) (not (null? data))))

(define (%contains-key? key)
  (if (not (%object?))
      #f
      ((box data)
       :exists (lambda (x) (== (car x) key)))))

(define (%array?)
  (vector? data))

(define (%string?)
  (string? data))

(define (%number?)
  (number? data))

(define (%integer?)
  (integer? data))

(define (%float?)
  (float? data))

(define (%boolean?)
  (boolean? data))

(define (%to-string)
  (cond ((integer? data) (number->string data))
        ((symbol? data) (symbol->string data))
        (else (json->string data))))

)

(define make-json json)

(define (json x)
  (cond
    ((string? x) (make-json x))
    ((null? x) (make-json 'null))
    ((boolean? x) (if x (make-json 'true) (make-json 'false)))
    ((number? x) (make-json x))
    ((procedure? x)
     (type-error "json: a procedure could not be converted to json case class"))
    (else (make-json x))))

(define (json-null)
  (json 'null))
(define (json-true)
  (json 'true))

(define (json-false)
  (json 'false))

(define (json-parse s)
  (json (string->json s)))

) ; end of begin
) ; end of define-library

