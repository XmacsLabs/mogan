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

(define-library (liii list)
(export
  ; SRFI 1: Constructors
  circular-list iota
  ; SRFI 1: Predicates
  null-list? circular-list?
  ; SRFI 1: Selectors
  first second third fourth fifth sixth seventh eighth ninth tenth
  take drop take-right drop-right
  last-pair last
  ; SRFI 1: fold, unfold & map
  count fold fold-right reduce reduce-right
  filter partition remove
  ; SRFI 1: Searching
  find any every list-index
  take-while drop-while
  ; SRFI 1: deleting
  delete
  ; Liii List extensions
  list-view flatmap
  list-null? list-not-null? not-null-list?
)
(import (srfi srfi-1))
(begin

(define (list-view scheme-list)
  (define (f-inner . funcs)
    (cond ((null? funcs) scheme-list)
          ((= (length funcs) 2)
           (list-view ((car funcs) (cadr funcs) scheme-list)))
          ((even? (length funcs))
           (apply
             (f-inner (car funcs) (cadr funcs))
             (cddr funcs)))
          (else (error 'wrong-number-of-args
                       "list-view only accepts even number of args"))))
  f-inner)

(define (flatmap f seq)
  (fold-right append () (map f seq)))

; the opposite of null-list?
(define (not-null-list? l)
  (cond ((pair? l)
         (or (null? (cdr l)) (pair? (cdr l))))
        ((null? l) #f)
        (else
         (error 'type-error "type mismatch"))))

; no exception version of null-list?
(define (list-null? l)
  (and (not (pair? l)) (null? l)))

; no exception version of not-null-list?
(define (list-not-null? l)
  (and (pair? l)
       (or (null? (cdr l)) (pair? (cdr l)))))

) ; end of begin
) ; end of library

