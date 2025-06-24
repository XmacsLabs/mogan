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

(define-library (liii lang)
                
(import (only (liii base)
              u8-string-length any? receive u8-substring)
        (only (liii oop)
              define-case-class display* @ typed-define case-class? chained-define
              define-object define-class chain-apply object->string)
        (only (liii sort) list-stable-sort vector-stable-sort)
        (only (liii hash-table)
              hash-table-update!/default hash-table-for-each hash-table-ref/default hash-table-contains? hash-table-delete!
              hash-table-count)
        (only (liii bitwise) bitwise-and bitwise-ior arithmetic-shift)
        (liii error)
        (liii list)
        (liii option)
        (liii either)
        (liii rich-list)
        (liii rich-char)
        (liii rich-vector)
        (liii rich-string)
        (liii rich-hash-table))

(export
  @ typed-define define-case-class define-object define-class
  case-class? class=? chained-define display* object->string
  option none either left right
  rich-integer rich-float rich-char rich-string
  rich-vector rich-list array rich-hash-table
  box $
)
(begin

(define (class=? left right)
  (cond
    ((and (case-class? left) (case-class? right))
     (left :equals right))
    ((case-class? left)
     (left :equals ($ right)))
    ((case-class? right)
     ($ left :equals right))
    (else
     (equal? left right))))

(define (box x)
  (cond ((integer? x) (rich-integer x))
        ((rational? x) (rich-rational x))
        ((float? x) (rich-float x))
        ((char? x) (rich-char x))
        ((string? x) (rich-string x))
        ((list? x) (rich-list x))
        ((vector? x) (rich-vector x))
        ((hash-table? x) (rich-hash-table x))
        (else (type-error "box: x must be integer?, rational?, float?, char?, string?, list?, vector?, hash-table?"))))

(define ($ x . xs)
  (if (null? xs) (box x) (apply (box x) xs)))

(define-case-class rich-integer ((data integer?))

(define (%get) data)

(define (%to n) 
  (unless (integer? n) 
    (type-error 
      (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
              %to '(n) 'n "integer" (object->string n))))
  (if (< n data) 
    (rich-list (list)) 
    (rich-list (iota (+ (- n data) 1) data))))

(define (%until n) 
  (unless (integer? n) 
    (type-error 
      (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
              %until '(n) 'n "integer" (object->string n))))
  (if (<= n data) 
    (rich-list (list)) 
    (rich-list (iota (+ (- n data)) data))))

(define (%to-rich-char)
  (rich-char data))

(define (%to-string)
  (number->string data))

(define (@max-value) 9223372036854775807)

(define (@min-value) -9223372036854775808)

;;return exact integer
(define (%sqrt)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
      (inexact->exact (floor (sqrt data)))))

)

(define-case-class rich-rational ((data rational?))

(define (%get) data)

(define (%abs) 
  (if (< data 0)
      (- 0 data)
      data))
  
)

(define-case-class rich-float ((data float?))
                   
(define (%get) data)

(define (%abs) 
  (if (< data 0)
      (- 0 data)
      data))
  
(define (%to-string)
  (number->string data))

(define (%sqrt)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative float is undefined!         ** Got ~a **" data))
      (sqrt data)))

)


(define array rich-vector)



) ; end of begin
) ; end of library

