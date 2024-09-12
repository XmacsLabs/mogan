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

(define-library (srfi srfi-125)
(import (srfi srfi-1))
(export
  make-hash-table hash-table hash-table-unfold alist->hash-table
  hash-table? hash-table-contains? hash-table-empty? hash-table=?
  hash-table-mutable?
  hash-table-ref hash-table-ref/default
  hash-table-set! hash-table-delete! hash-table-intern! hash-table-update!
  hash-table-update!/default hash-table-pop! hash-table-clear!
  hash-table-size hash-table-keys hash-table-values hash-table-entries
  hash-table-find hash-table-count hash-table->alist
)
(begin

(define (assert-hash-table-type ht f)
  (when (not (hash-table? ht))
    (error 'type-error f "this parameter must be typed as hash-table")))

(define s7-hash-table-set! hash-table-set!)
(define s7-make-hash-table make-hash-table)

(define (make-hash-table . args)
  (cond ((null? args) (s7-make-hash-table))
        ((comparator? (car args))
         (let* ((equiv (comparator-equality-predicate (car args)))
                (hash-func (comparator-hash-function (car args))))
           (s7-make-hash-table 8 (cons equiv hash-func) (cons #t #t))))
        (else (type-error "make-hash-table"))))

(define (hash-table-contains? ht key)
  (not (not (hash-table-ref ht key))))

(define (hash-table-empty? ht)
  (zero? (hash-table-size ht)))

(define (hash-table=? ht1 ht2)
  (equal? ht1 ht2))

(define-macro (hash-table-ref/default ht key default)
  `(or (hash-table-ref ,ht ,key)
        ,default))

(define (hash-table-set! ht . rest)
  (assert-hash-table-type ht hash-table-set!)
  (let1 len (length rest)
    (when (or (odd? len) (zero? len))
      (error 'wrong-number-of-args len "but must be even and non-zero"))
    
    (s7-hash-table-set! ht (car rest) (cadr rest))
    (when (> len 2)
          (apply hash-table-set! (cons ht (cddr rest))))))

(define (hash-table-delete! ht key . keys)
  (assert-hash-table-type ht hash-table-delete!)
  (let1 all-keys (cons key keys)
    (length
      (filter
        (lambda (x)
          (if (hash-table-contains? ht x)
              (begin
                (s7-hash-table-set! ht x #f)
                #t)
              #f))
        all-keys))))

(define (hash-table-update! ht key value)
  (hash-table-set! ht key value))

(define (hash-table-clear! ht)
  (for-each
    (lambda (key)
      (hash-table-set! ht key #f))
    (hash-table-keys ht)))

(define hash-table-size hash-table-entries)

(define (hash-table-keys ht)
  (map car (map values ht)))

(define (hash-table-values ht)
  (map cdr (map values ht)))

(define (hash-table->alist table)
  (map values table))

) ; end of begin
) ; end of define-library

