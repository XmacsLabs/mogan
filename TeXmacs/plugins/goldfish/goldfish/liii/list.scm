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
  ; S7 built-in
  cons car cdr map for-each
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
  filter partition remove append-map
  ; SRFI 1: Searching
  find any every list-index
  take-while drop-while
  ; SRFI 1: deleting
  delete
  ; Liii List extensions
  list-view flatmap
  list-null? list-not-null? not-null-list?
  length=? length>? length>=?
)
(import (srfi srfi-1)
        (liii error))
(begin

(define (length=? x scheme-list)
  (when (< x 0)
    (value-error "length=?: expected non-negative integer x but received ~d" x))
  (cond ((and (= x 0) (null? scheme-list)) #t)
        ((or (= x 0) (null? scheme-list)) #f)
        (else (length=? (- x 1) (cdr scheme-list)))))

(define (length>? lst len)
  (let loop ((lst lst)
             (cnt 0))
    (cond ((null? lst) (< len cnt))
          ((pair? lst) (loop (cdr lst) (+ cnt 1)))
          (else (< len cnt)))))

(define (length>=? lst len)
  (let loop ((lst lst)
             (cnt 0))
    (cond ((null? lst) (<= len cnt))
          ((pair? lst) (loop (cdr lst) (+ cnt 1)))
          (else (<= len cnt)))))

(define (list-view scheme-list)
  (define (f-inner-reducer scheme-list filter filter-func rest-funcs)
    (cond ((null? rest-funcs) (list-view (filter filter-func scheme-list)))
          (else
           (f-inner-reducer (filter filter-func scheme-list)
                            (car rest-funcs)
                            (cadr rest-funcs)
                            (cddr rest-funcs)))))
  (define (f-inner . funcs)
    (cond ((null? funcs) scheme-list)
          ((length=? 2 funcs)
           (list-view ((car funcs) (cadr funcs) scheme-list)))
          ((even? (length funcs))
           (f-inner-reducer scheme-list
                            (car funcs)
                            (cadr funcs)
                            (cddr funcs)))
          (else (error 'wrong-number-of-args
                       "list-view only accepts even number of args"))))
  f-inner)

(define flatmap append-map)

(define (not-null-list? l)
  (cond ((pair? l)
         (or (null? (cdr l)) (pair? (cdr l))))
        ((null? l) #f)
        (else
         (error 'type-error "type mismatch"))))

(define (list-null? l)
  (and (not (pair? l)) (null? l)))

(define (list-not-null? l)
  (and (pair? l)
       (or (null? (cdr l)) (pair? (cdr l)))))

) ; end of begin
) ; end of library

