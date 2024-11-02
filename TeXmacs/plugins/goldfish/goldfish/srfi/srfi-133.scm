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

(define-library (srfi srfi-133)
(import (liii base))
(export
  vector-empty?
  vector-count
  vector-any vector-every vector-copy vector-copy!
  vector-index vector-index-right vector-partition
  vector-swap! vector-cumulate reverse-list->vector)
(begin

(define (vector-empty? v)
  (when (not (vector? v))
    (error 'type-error "v is not a vector"))
  (zero? (vector-length v)))

; TODO optional parameters
(define (vector-count pred v)
  (let loop ((i 0) (count 0))
       (cond ((= i (vector-length v)) count)
             ((pred (vector-ref v i)) 
              (loop (+ i 1) (+ count 1)))
             (else (loop (+ i 1) count)))))

; Return a new vector v-rst with same length of input vector vec.
; Every element of the result is the result the i-th iteration of fn cumu_i vec_i.
;   Where fn should be a procedure with 2 args.
;   The type of knil and vector could be different.
; In the i-th iteration, cumu_i = fn cumu_(i-1) vec_i, with cumu_0 = fn knil vec_0.

(define vector-cumulate
  (typed-lambda ((fn procedure?) knil (vec vector?))
    (let* ((len (vector-length vec))
           (v-rst (make-vector len)))
      (let loop ((i 0) (lhs knil))
           (if (= i len)
               v-rst
               (let1 cumu-i (fn lhs (vec i))
                 (begin
                   (vector-set! v-rst i cumu-i)
                   (loop (+ 1 i) cumu-i))))))))
; TODO optional parameters
(define (vector-any pred v)
  (let loop ((i 0))
       (cond ((= i (vector-length v)) #f)
             ((pred (vector-ref v i)) #t)
             (else (loop (+ i 1))))))

; TODO optional parameters
(define (vector-every pred v)
  (let loop ((i 0))
       (cond ((= i (vector-length v)) #t)
             ((not (pred (vector-ref v i))) #f)
             (else (loop (+ i 1))))))

; TODO optional parameters
(define (vector-index pred v)
  (let loop ((i 0))
       (cond ((= i (vector-length v)) #f)
             ((pred (vector-ref v i)) i)
             (else (loop (+ i 1))))))

; TODO optional parameters
(define (vector-index-right pred v)
  (let ((len (vector-length v)))
    (let loop ((i (- len 1)))
         (cond ((< i 0) #f)
               ((pred (vector-ref v i)) i)
               (else (loop (- i 1)))))))

(define (vector-partition pred v)
  (let* ((len (vector-length v))
         (cnt (vector-count pred v))
         (ret (make-vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (if (= i len)
        (values ret cnt)
        (let ((elem (vector-ref v i)))
          (if (pred elem)
            (begin
              (vector-set! ret yes elem)
              (loop (+ i 1) (+ yes 1) no))
            (begin
              (vector-set! ret no elem)
              (loop (+ i 1) yes (+ no 1)))))))))

(define (vector-swap! vec i j)
  (let ((elem-i (vector-ref vec i))
        (elem-j (vector-ref vec j)))
    (vector-set! vec i elem-j)
    (vector-set! vec j elem-i)
    ))

; Input a proper-list, return a vector with inversed order elements.
(define reverse-list->vector
  (typed-lambda ((lst proper-list?))
    (let* ((len (length lst)) (v-rst (make-vector len)))
      (let loop ((l lst) (i (- len 1)))
        (if (null? l) v-rst
            (begin
              (vector-set! v-rst i (car l))
              (loop (cdr l) (- i 1))))))))

) ; end of begin
) ; end of define-library

