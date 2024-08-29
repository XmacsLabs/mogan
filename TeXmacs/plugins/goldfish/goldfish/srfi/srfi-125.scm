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
(export
  make-hash-table, hash-table, hash-table-unfold, alist->hash-table
  hash-table? hash-table-contains? hash-table-empty? hash-table=?
  hash-table-mutable?
  hash-table-ref hash-table-ref/default
  hash-table-set! hash-table-delete! hash-table-intern! hash-table-update!
  hash-table-update!/default hash-table-pop! hash-table-clear!
  hash-table-size hash-table-keys hash-table-values hash-table-entries
  hash-table-find hash-table-count
)
(begin

(define (hash-table-contains? ht key)
  (not (not (hash-table-ref ht key))))

(define (hash-table-empty? ht)
  (zero? (hash-table-size ht)))

(define (hash-table=? ht1 ht2)
  (equal? ht1 ht2))

(define hash-table-size hash-table-entries)

(define (hash-table->alist table)
  (map values table))

) ; end of begin
) ; end of define-library

