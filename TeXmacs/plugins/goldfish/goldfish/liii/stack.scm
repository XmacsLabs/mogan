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

(define-library (liii stack)
(import (srfi srfi-9)
        (liii base)
        (liii error))
(export
 stack
 stack? stack-empty?
 stack-size stack-top
 stack-push! stack-pop!
 stack->list
)
(begin

(define-record-type :stack
  (make-stack data)
  stack?
  (data get-data set-data!))

(define (%stack-check-parameter st)
  (when (not (stack? st))
    (error 'type-error "Parameter st is not a stack")))

(define (stack . l)
  (if (null? l)
      (make-stack '())
      (make-stack l)))

(define (stack-empty? st)
  (%stack-check-parameter st)
  (null? (get-data st)))

(define (stack-size st)
  (%stack-check-parameter st)
  (length (get-data st)))

(define (stack-top st)
  (%stack-check-parameter st)
  (car (get-data st)))

(define (stack-push! st elem)
  (%stack-check-parameter st)
  (set-data! st (cons elem (get-data st))))

(define (stack-pop! st)
  (%stack-check-parameter st)
  (when (stack-empty? st)
    (error 'value-error "Failed to stack-pop! on empty stack"))
  (let1 data (get-data st)
    (set-data! st (cdr data))
    (car data)))

(define (stack->list st)
  (%stack-check-parameter st)
  (get-data st))

) ; end of begin
) ; end of library

