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

(define-library (srfi srfi-132)
(export list-sorted? vector-sorted?
        list-merge  list-sort  list-stable-sort  vector-merge  vector-sort  vector-stable-sort
        list-merge! list-sort! list-stable-sort! vector-merge! vector-sort! vector-stable-sort!)
(import (liii list)
        (liii error)
        (scheme case-lambda))
(begin

  (define (list-sorted? less-p lis)
    (if (null? lis)
      #t
      (do ((first lis (cdr first))
           (second (cdr lis) (cdr second))
           (res #t (not (less-p (car second) (car first)))))
        ((or (null? second) (not res)) res))))

  ; TODO optional parameters
  (define (vector-sorted? less-p v)
    (let ((start 0)
          (end (length v)))
      (do ((first start (+ 1 first))
           (second (+ 1 start) (+ 1 second))
           (res #t (not (less-p (vector-ref v second) (vector-ref v first)))))
        ((or (>= second end) (not res)) res))))

  (define (list-merge less-p lis1 lis2)
    (let loop
      ((res '())
       (lis1 lis1)
       (lis2 lis2))
      (cond
        ((and (null? lis1) (null? lis2)) (reverse res))
        ((null? lis1) (loop (cons (car lis2) res) lis1 (cdr lis2)))
        ((null? lis2) (loop (cons (car lis1) res) lis2 (cdr lis1)))
        ((less-p (car lis2) (car lis1)) (loop (cons (car lis2) res) lis1 (cdr lis2)))
        (else (loop (cons (car lis1) res) (cdr lis1) lis2)))))

  ; this list-merge! violates SRFI 132, since it does not satisfy the constant running space constraint specified in SRFI 132, and does not work "in place"
  (define list-merge! list-merge)

  (define (list-stable-sort less-p lis)
    (define (sort l r)
      (cond
        ((= l r) '())
        ((= (+ l 1) r) (list (list-ref lis l)))
        (else
          (let* ((mid (quotient (+ l r) 2))
                 (l-sorted (sort l mid))
                 (r-sorted (sort mid r)))
            (list-merge less-p l-sorted r-sorted)))))
    (sort 0 (length lis)))

  (define list-sort list-stable-sort)
  (define list-sort! list-stable-sort)
  (define list-stable-sort! list-stable-sort)

  (define vector-stable-sort
    (case-lambda
      ((less-p v)
       (list->vector (list-stable-sort less-p (vector->list v))))
      ((less-p v start)
       (list->vector (list-stable-sort less-p (subvector->list v start (vector-length v)))))
      ((less-p v start end)
       (list->vector (list-stable-sort less-p (subvector->list v start end))))))

  (define vector-sort vector-stable-sort)

  (define (vector-sort! . r) (???))
  (define (vector-stable-sort! . r) (???))

  (define (subvector->list v start end)
    (do ((r '() (cons (vector-ref v p) r))
         (p start (+ 1 p)))
      ((>= p end) (reverse r))))

  (define vector-merge
    (case-lambda
      ((less-p v1 v2)
       (list->vector (list-merge less-p (vector->list v1) (vector->list v2))))
      ((less-p v1 v2 start1)
       (list->vector (list-merge less-p (subvector->list v1 start1 (vector-length v1)) (vector->list v2))))
      ((less-p v1 v2 start1 end1)
       (list->vector (list-merge less-p (subvector->list v1 start1 end1) (vector->list v2))))
      ((less-p v1 v2 start1 end1 start2)
       (list->vector (list-merge less-p (subvector->list v1 start1 end1) (subvector->list v2 start2 (vector-length v2)))))
      ((less-p v1 v2 start1 end1 start2 end2)
       (list->vector (list-merge less-p (subvector->list v1 start1 end1) (subvector->list v2 start2 end2))))))

  (define (vector-merge! . r) (???))

) ; end of begin
) ; end of library

