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

(define-library (liii queue)
(import (liii list)
        (liii base)
        (srfi srfi-9)
        (liii error))
(export
  queue
  queue? queue-empty?
  queue-size queue-front queue-back
  queue-pop! queue-push!
  queue->list)
(begin
                
(define-record-type :queue
  (make-queue data)
  queue?
  (data get-data set-data!))

(define (%queue-assert-type q)
  (when (not (queue? q))
    (type-error "Parameter q is not a queue")))

(define (%queue-assert-value q)
  (when (queue-empty? q)
    (value-error "q must be non-empty")))

(define (queue . l)
  (if (null? l)
      (make-queue '())
      (make-queue l)))

(define (queue-empty? q)
  (%queue-assert-type q)
  (null? (get-data q)))

(define (queue-size q)
  (%queue-assert-type q)
  (length (get-data q)))

(define (queue-front q)
  (%queue-assert-type q)
  (%queue-assert-value q)
  (first (get-data q)))

(define (queue-back q)
  (%queue-assert-type q)
  (%queue-assert-value q)
  (last (get-data q)))

(define (queue-push! q x)
  (%queue-assert-type q)
  (let1 data (get-data q)
    (set-data! q (append data (list x)))))

(define (queue-pop! q)
  (%queue-assert-type q)
  (%queue-assert-value q)
  (let1 data (get-data q)
    (set-data! q (cdr data))
    (car data)))

(define (queue->list q)
  (get-data q))

) ; end of begin
) ; end of library

