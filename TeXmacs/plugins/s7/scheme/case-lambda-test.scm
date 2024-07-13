;
; Copyright (C) 2024 The S7 SRFI Authors
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

(import (srfi srfi-1)
        (srfi srfi-78)
        (scheme case-lambda))

(check-set-mode! 'report-failed)

(define (my-func . args)
  (case-lambda
    (() "zero args")
    ((x) (+ x x))
    ((x y) (+ x y))
    ((x y . rest) (reduce + 0 (cons x (cons y rest))))))

(check ((my-func)) => "zero args")
(check ((my-func) 2) => 4)
(check ((my-func) 3 4) => 7)
(check ((my-func) 1 2 3 4) => 10)

