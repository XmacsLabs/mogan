;;; SRFI-1 list-processing library                      -*- Scheme -*-
;;; Reference implementation
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin
;;;
;;; Copyright (c) 2024 The S7 SRFI Authors
;;; Follow the same License as the original one

(define-library (srfi srfi-1)
(export
  circular-list iota circular-list? null-list?
  first second third fourth fifth
  sixth seventh eighth ninth tenth
  take drop take-right drop-right count fold fold-right
  reduce reduce-right filter partition remove find
  delete delete-duplicates
  take-while drop-while)
(begin

; 0 clause BSD, from S7 repo stuff.scm
(define circular-list
  (lambda objs
    (let ((lst (copy objs)))
      (set-cdr! (list-tail lst (- (length lst) 1)) lst))))

; 0 clause BSD, from S7 repo stuff.scm
(define* (iota n (start 0) (incr 1)) 
  (if (or (not (integer? n)) (< n 0))
    (error 'wrong-type-arg
           "iota length ~A should be a non-negative integer" n))
  (let ((lst (make-list n)))
    (do ((p lst (cdr p))
         (i start (+ i incr)))
      ((null? p) lst)
      (set! (car p) i))))

; 0 clause BSD, from S7 repo stuff.scm
(define circular-list?
  (lambda (obj)
    (catch #t
      (lambda () (infinite? (length obj)))
      (lambda args #f))))

(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else
         (error 'wrong-type-arg "null-list?: argument out of domain" l))))

(define first car)

(define second cadr)

(define third caddr)

(define (fourth x) (list-ref x 3))

(define (fifth x) (list-ref x 4))

(define (sixth x) (list-ref x 5))

(define (seventh x) (list-ref x 6))

(define (eighth x) (list-ref x 7))

(define (ninth x) (list-ref x 8))

(define (tenth x) 
  (cadr (cddddr (cddddr x))))

(define (take l k)
  (let recur ((l l) (k k))
    (if (zero? k) '()
        (cons (car l)
              (recur (cdr l) (- k 1))))))

(define (drop l k)
  (let iter ((l l) (k k))
    (if (zero? k) l (iter (cdr l) (- k 1)))))

(define (take-right l k)
  (let lp ((lag l)  (lead (drop l k)))
    (if (pair? lead)
        (lp (cdr lag) (cdr lead))
        lag)))

(define (drop-right l k)
  (let recur ((lag l) (lead (drop l k)))
    (if (pair? lead)
        (cons (car lag) (recur (cdr lag) (cdr lead)))
        '())))

(define (count pred list1 . lists)
  (let lp ((lis list1) (i 0))
    (if (null-list? lis) i
        (lp (cdr lis) (if (pred (car lis)) (+ i 1) i)))))

(define (fold f initial l)
  (if (null? l)
      initial
      (fold f
            (f (car l) initial)
            (cdr l))))

(define (fold-right f initial l)
  (if (null? l)
    initial
    (f (car l)
        (fold-right f
                    initial
                    (cdr l)))))

(define (reduce f initial l)
  (if (null-list? l) initial
      (fold f (car l) (cdr l))))

(define (reduce-right f initial l)
  (if (null-list? l) initial
      (let recur ((head (car l)) (l (cdr l)))
        (if (pair? l)
            (f head (recur (car l) (cdr l)))
            head))))

(define (filter pred l)
  (let recur ((l l))
    (if (null-list? l) l
        (let ((head (car l))
              (tail (cdr l)))
          (if (pred head)
              (let ((new-tail (recur tail)))
                (if (eq? tail new-tail) l
                    (cons head new-tail)))
              (recur tail))))))

(define (partition pred l)
  (let loop ((lst l) (satisfies '()) (dissatisfies '()))
    (cond ((null? lst)
           (cons satisfies dissatisfies))
          ((pred (car lst))
           (loop (cdr lst) (cons (car lst) satisfies) dissatisfies))
          (else
           (loop (cdr lst) satisfies (cons (car lst) dissatisfies))))))

(define (remove pred l)
  (filter (lambda (x) (not (pred x))) l))

(define (find pred l)
  (cond ((null? l) #f)
        ((pred (car l)) (car l)) 
        (else (find pred (cdr l)))))

(define (take-while pred lst)
  (if (null? lst) 
      '()
      (if (pred (car lst))
          (cons (car lst) (take-while pred (cdr lst)))
          '())))

(define (drop-while pred l)
  (if (null? l)
      '()
      (if (pred (car l))
          (drop-while pred (cdr l))
          l)))

(define (%extract-maybe-equal maybe-equal)
  (let ((my-equal (if (null-list? maybe-equal)
                      =
                      (car maybe-equal))))
    (if (procedure? my-equal)
        my-equal
        (error 'wrong-type-arg "maybe-equal must be procedure"))))
(define (delete x l . maybe-equal)
  (let ((my-equal (%extract-maybe-equal maybe-equal)))
    (filter (lambda (y) (not (my-equal x y))) l)))

;;; right-duplicate deletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete-duplicates delete-duplicates!
;;;
;;; Beware -- these are N^2 algorithms. To efficiently remove duplicates
;;; in long lists, sort the list to bring duplicates together, then use a
;;; linear-time algorithm to kill the dups. Or use an algorithm based on
;;; element-marking. The former gives you O(n lg n), the latter is linear.

(define (delete-duplicates lis . maybe-equal)
  (let ((my-equal (%extract-maybe-equal maybe-equal)))
    (let recur ((lis lis))
      (if (null-list? lis)
          lis
          (let* ((x (car lis))
                 (tail (cdr lis))
                 (new-tail (recur (delete x tail my-equal))))
            (if (eq? tail new-tail)
                lis
                (cons x new-tail)))))))

) ; end of begin
) ; end of define-library
