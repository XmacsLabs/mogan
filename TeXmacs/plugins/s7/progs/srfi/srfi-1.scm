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

(provide 'srfi-1)
(provide 'null-list?)
(provide 'first)
(provide 'second)
(provide 'third)
(provide 'fourth)
(provide 'fifth)
(provide 'sixth)
(provide 'seventh)
(provide 'eighth)
(provide 'ninth)
(provide 'tenth)
(provide 'take)
(provide 'drop)
(provide 'take-right)
(provide 'drop-right)
(provide 'count)
(provide 'fold)
(provide 'fold-right)
(provide 'reduce)
(provide 'reduce-right)
(provide 'filter)
(provide 'partition)
(provide 'remove)
(provide 'find)
(provide 'take-while)
(provide 'drop-while)
(provide 'check-report)
(provide 'check-reset!)

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

