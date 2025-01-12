;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii base)
(import (scheme base)
        (srfi srfi-2)
        (srfi srfi-8))
(export
  ; (scheme base) defined by R7RS
  let-values
  ; R7RS 5: Program Structure
  define-values define-record-type
  ; R7RS 6.2: Numbers
  square exact inexact max min floor s7-floor ceiling s7-ceiling truncate s7-truncate
  round s7-round floor-quotient gcd lcm s7-lcm
  ; R7RS 6.3: Booleans
  boolean=?
  ; R7RS 6.4: list
  pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr
  null? list? make-list list length append reverse list-tail
  list-ref list-set! memq memv member assq assv assoc list-copy
  ; R7RS 6.5: Symbol
  symbol? symbol=? string->symbol symbol->string
  ; R7RS 6.6: Characters
  digit-value
  ; R7RS 6.7: String
  string-copy
  ; R7RS 6.8 Vector
  vector->string string->vector vector-copy vector-copy! vector-fill!
  ; R7RS 6.9 Bytevectors
  bytevector? make-bytevector bytevector bytevector-length bytevector-u8-ref
  bytevector-u8-set! bytevector-append utf8->string string->utf8 u8-string-length
  u8-substring
  ; Input and Output
  call-with-port port? binary-port? textual-port? input-port-open? output-port-open?
  open-binary-input-file open-binary-output-file close-port eof-object
  ; Control flow
  string-map vector-map string-for-each vector-for-each
  ; Exception
  raise guard read-error? file-error?
  ; SRFI-2
  and-let*
  ; SRFI-8
  receive
  ; Extra routines
  loose-car loose-cdr in? compose identity any?
  ; Extra structure
  let1  typed-lambda typed-define define-case-class case-class?
  == != display* object->string
)
(begin

(define* (u8-substring str (start 0) (end #t))
  (utf8->string (string->utf8 str start end)))

(define (loose-car pair-or-empty)
  (if (eq? '() pair-or-empty)
      '()
      (car pair-or-empty)))

(define (loose-cdr pair-or-empty)
  (if (eq? '() pair-or-empty)
      '()
      (cdr pair-or-empty)))

(define (in? elem l)
  (cond ((list? l) (not (not (member elem l))))
        ((vector? l)
         (let loop ((i (- (vector-length l) 1)))
           (if (< i 0)
               #f
               (if (== elem (vector-ref l i))
                   #t
                   (loop (- i 1))))))
        ((and (char? elem) (string? l))
         (in? elem (string->list l)))
        (else (error 'type-error "type mismatch"))))

(define identity (lambda (x) x))

(define (compose . fs)
  (if (null? fs)
      (lambda (x) x)
      (lambda (x)
        ((car fs) ((apply compose (cdr fs)) x)))))
  
(define (any? x) #t)

(define-macro (let1 name1 value1 . body)
  `(let ((,name1 ,value1))
     ,@body))

; 0 clause BSD, from S7 repo stuff.scm
(define-macro (typed-lambda args . body)
  ; (typed-lambda ((var [type])...) ...)
  (if (symbol? args)
      (apply lambda args body)
      (let ((new-args (copy args)))
        (do ((p new-args (cdr p)))
            ((not (pair? p)))
          (if (pair? (car p))
              (set-car! p (caar p))))
        `(lambda ,new-args
           ,@(map (lambda (arg)
                    (if (pair? arg)
                        `(unless (,(cadr arg) ,(car arg))
                           (error 'type-error
                             "~S is not ~S~%" ',(car arg) ',(cadr arg)))
                        (values)))
                  args)
           ,@body))))

(define-macro (typed-define name-and-params x . xs)
  (let* ((name (car name-and-params))
         (params (cdr name-and-params)))
    `(define* (,name ,@(map (lambda (param)
                              (let ((param-name (car param))
                                    (type-pred (cadr param))
                                    (default-value (cddr param)))
                                (if (null? default-value)
                                    param-name
                                    `(,param-name ,(car default-value)))))
                            params))

       ,@(map (lambda (param)
                (let ((param-name (car param))
                      (type-pred (cadr param)))
                  `(unless (,type-pred ,param-name)
                     (error 'type-error (string-append "Invalid type for " (symbol->string ',param-name))))))
              params)
       ,x
       ,@xs)))

(define-macro (define-case-class class-name fields . extra-operations)
  (let ((constructor (string->symbol (string-append (symbol->string class-name))))
        (key-fields (map (lambda (field)
                           (string->symbol (string-append ":" (symbol->string (car field)))))
                         fields)))
    `(begin
       (typed-define ,(cons class-name fields)
         (define (%is-instance-of x)
           (eq? x ',class-name))
         
         (typed-define (%equals (that case-class?))
           (and (that :is-instance-of ',class-name)
                ,@(map (lambda (field)
                         `(equal? ,(car field) (that ',(car field))))
                       fields)))
         
         (define (%apply . args)
           (when (null? args)
             (??? ,class-name "apply on zero args is not implemented"))
           (cond ((equal? ((symbol->string (car args)) 0) #\:)
                  (??? ,class-name
                    "No such method: " (car args)
                    "Please implement the method"))
                 (else
                  (??? ,class-name "No such field: " (car args)
                       "Please use the correct field name"
                       "Or you may implement %apply to process " args))))
         
         (define (%to-string)
           (let ((field-strings
                  (list ,@(map (lambda (field key-field)
                                 `(string-append
                                   ,(symbol->string key-field) " "
                                   (object->string ,(car field))))
                               fields key-fields))))
             (let loop ((strings field-strings)
                        (acc ""))
               (if (null? strings)
                   (string-append "(" ,(symbol->string class-name) " " acc ")")
                   (loop (cdr strings)
                         (if (zero? (string-length acc))
                             (car strings)
                             (string-append acc " " (car strings))))))))

         ,@extra-operations

         (lambda (msg . args)
           (cond
             ((eq? msg :is-instance-of) (apply %is-instance-of args))
             ((eq? msg :equals) (apply %equals args))
             ((eq? msg :to-string) (%to-string))
             
             ,@(map (lambda (field)
                      `((eq? msg ',(car field)) ,(car field)))
                    fields)
             ,@(map (lambda (field key-field)
                      `((eq? msg ,key-field)
                        (,constructor ,@(map (lambda (f)
                                               (if (eq? (car f) (car field))
                                                   '(car args)
                                                   (car f)))
                                             fields))))
                    fields key-fields)

             ,@(map (lambda (op)
                      `((eq? msg ,(string->symbol (string-append ":" (substring (symbol->string (caadr op)) 1))))
                        (apply ,(caadr op) args)))
                    extra-operations)

             (else (apply %apply (cons msg args)))))))))

(define (case-class? x)
  (and-let* ((is-proc? (procedure? x))
             (source (procedure-source x))
             (body (source 2))
             (is-cond? (eq? (car body) 'cond))
             (at-least-2? (>= (length body) 3))
             (pred1 ((body 1) 0))
             (pred2 ((body 2) 0)))
    (and (equal? pred1 '(eq? msg :is-instance-of))
         (equal? pred2 '(eq? msg :equals)))))

(define (== left right)
  (if (and (case-class? left) (case-class? right))
      (left :equals right)
      (equal? left right)))

(define (!= left right)
  (not (== left right)))

(define (display* . params)
  (define (%display x)
    (if (case-class? x)
        (display (x :to-string))
        (display x)))
  (for-each %display params))

(define s7-object->string object->string)

(define (object->string x)
  (if (case-class? x)
      (x :to-string)
      (s7-object->string x)))

) ; end of begin
) ; end of define-library

