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

(define-library (liii range)
(import (liii oop) (only (liii lang) rich-list))
(export range)
(begin

(define-case-class range
  ((start integer?) (end integer?) (step integer?) (inclusive? boolean?))

(define* (@inclusive start end (step 1))
  (range start end step #t))

(define (%empty?)
  (or (and (> start end) (> step 0))
      (and (< start end) (< step 0))
      (and (= start end) (not inclusive?))))

(define (%map map-func)
  (if (%empty?)
      (rich-list :empty)
      (let loop ((current start) (result '()))
        (cond
          ((or (and (> step 0) (if inclusive? (> current end) (>= current end)))
                (and (< step 0) (if inclusive? (< current end) (<= current end))))
            (rich-list (reverse result)))
          (else
            (loop (+ current step)
                  (cons (map-func current) result)))))))

(define (%filter f)
  (if (%empty?)
      (rich-list :empty)
      (let loop ((i start) (return '()))
        (cond
          ((or (and (> step 0) (> i end))
               (and (< step 0) (< i end))
               (and (= i end) (not inclusive?)))
           (rich-list (reverse return)))
          (else
           (loop (+ i step)
                 (if (f i)
                     (cons i return)
                     return)))))))

) ; define-case-cass
) ; begin
) ; define-library

