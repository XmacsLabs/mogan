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

(define-library (srfi srfi-151)
(import (liii base))
(export
  bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-eqv bitwise-nor bitwise-nand 
  bit-count bitwise-orc1 bitwise-orc2 bitwise-andc1 bitwise-andc2
  arithmetic-shift integer-length
)
(begin

(define bitwise-not lognot)

(define bitwise-and logand)

(define bitwise-ior logior)

(define bitwise-xor logxor)

(define (bitwise-eqv a b)
        (= (bitwise-xor a b) 0))

(define (bitwise-nor a b)  
        (lognot (bitwise-ior a b)))

(define (bitwise-nand a b)  
        (lognot (bitwise-and a b)))

(define bit-count 
  (typed-lambda ((i integer?))
    (define (bit-count-positive i)
      (let loop ((n i) (cnt 0))
        (if (= n 0)
            cnt
            (loop (logand n (- n 1)) (+ cnt 1)))))

    (cond ((zero? i) 0)
          ((positive? i) (bit-count-positive i))
          (else (bit-count-positive (lognot i))))))

(define (bitwise-orc1 i j)
  (bitwise-ior (bitwise-not i) j))

(define (bitwise-orc2  i j) 
  (bitwise-ior i (bitwise-not j)))

(define (bitwise-andc1 i j)  
  (bitwise-and (bitwise-not i) j))

(define (bitwise-andc2 i j) 
  (bitwise-and i (bitwise-not j)))

(define arithmetic-shift ash)

(define (integer-length n)
  (if (zero? n)
      0
      (let loop ((value (abs n)) (count 1))
           (if (<= value 1)
               count
               (loop (ash value -1) (+ count 1))))))

) ; end of begin
) ; end of define-library

