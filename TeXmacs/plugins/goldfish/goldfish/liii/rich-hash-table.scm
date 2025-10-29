;
; Copyright (C) 2025 The Goldfish Scheme Authors
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

(define-library (liii rich-hash-table)
  (import (liii hash-table) (liii oop) (liii option) (srfi srfi-8))
  (export rich-hash-table)
  (begin

    (define-case-class rich-hash-table ((data hash-table?))
      (define (%collect) data)

      (chained-define (@empty)
        (rich-hash-table (make-hash-table)))

      (define (%find pred?)
        (define iter (make-iterator data))
        (let loop ((kv (iter)))
          (cond 
            ((eof-object? kv) (none))
            ((and (pair? kv) (pred? (car kv) (cdr kv))) (option kv))
            (else (loop (iter))))))

      (define (%get k)
        (option (hash-table-ref/default data k '())))

      (define (%remove k)
        (rich-hash-table
         (let ((new (make-hash-table)))
           (hash-table-for-each
            (lambda (key val)
             (unless (equal? key k)
              (hash-table-set! new key val)))
            data)
           new)))

      (chained-define (%remove! k)
        (hash-table-delete! data k)
        %this)

      (define (%contains k)
        (hash-table-contains? data k))

      (define (%forall pred?)
        (let ((all-kv (map identity data)))
          (let loop ((kvs all-kv))  
            (if (null? kvs)
                #t  
                (let ((kv (car kvs)))
                  (if (pred? (car kv) (cdr kv))
                      (loop (cdr kvs))  
                      #f))))))  

      (define (%exists pred?)
        (define iter (make-iterator data))
        (let loop ((kv (iter)))
          (cond 
            ((eof-object? kv) #f)
            ((and (pair? kv) (pred? (car kv) (cdr kv))) #t)
            (else (loop (iter))))))

      (define (%map f . args)
        (chain-apply args
          (let ((r (make-hash-table)))
            (hash-table-for-each
              (lambda (k v)
                (receive (k1 v1) (f k v)
                  (hash-table-set! r k1 v1)))
              data)
            (rich-hash-table r))))

      (define (%count pred)
        (hash-table-count pred data))

      (define (%for-each proc)
        (hash-table-for-each proc data))

      (define (%filter f . args)
        (chain-apply args
          (let ((r (make-hash-table)))
            (hash-table-for-each
              (lambda (k v)
                (when (f k v) (hash-table-set! r k v)))
              data)
            (rich-hash-table r))))

      (define (%size)
        (hash-table-size data))

      ) ; end of define-case-class

    ) ; end of begin
  ) ; end of define-library
