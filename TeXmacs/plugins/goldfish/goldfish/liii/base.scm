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
(export == != display* in? let1 compose identity)
(begin

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
  
) ; end of begin
) ; end of define-library

