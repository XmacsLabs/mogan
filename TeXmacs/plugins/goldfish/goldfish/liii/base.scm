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
  square exact inexact floor s7-floor ceiling s7-ceiling truncate s7-truncate
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
  ; Extra routines for (liii base)
  == != display* in? let1 compose identity typed-lambda
)
(begin

(define* (u8-substring str (start 0) (end #t))
  (utf8->string (string->utf8 str start end)))

(define == equal?)

(define (!= left right)
  (not (equal? left right)))

(define (display* . params)
  (for-each display params))

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

(define-macro (let1 name1 value1 . body)
  `(let ((,name1 ,value1))
     ,@body))

(define identity (lambda (x) x))

(define (compose . fs)
  (if (null? fs)
      (lambda (x) x)
      (lambda (x)
        ((car fs) ((apply compose (cdr fs)) x)))))
  
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

) ; end of begin
) ; end of define-library

