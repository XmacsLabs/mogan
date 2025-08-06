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

(define-library (liii option)
  (import (liii oop) (liii base))
  (export option none)
  (begin

    (define-case-class option ((value any?))

      (define (%get)
        (if (null? value)
            (value-error "option is empty, cannot get value")
            value))

      (define (%get-or-else default)
        (cond ((not (null? value)) value)
              ((and (procedure? default) (not (case-class? default)))
               (default))
              (else default)))

      (define (%or-else default . args)
        (when (not (option :is-type-of default))
          (type-error "The first parameter of option%or-else must be a option case class"))
  
        (chain-apply args
          (if (null? value)
              default
              (option value))))

      (define (%equals that)
        (class=? value (that 'value)))

      (define (%defined?) (not (null? value)))
  
      (define (%empty?)
        (null? value))

      (define (%forall f)
        (if (null? value)
            #f
            (f value)))

      (define (%exists f)
        (if (null? value)
            #f
            (f value)))

      (define (%contains elem)
        (if (null? value)
            #f
            (equal? value elem)))

      (define (%for-each f)
        (when (not (null? value))
              (f value)))

      (define (%map f . args)
        (chain-apply args
          (if (null? value)
              (option '())
              (option (f value)))))

      (define (%flat-map f . args)
        (chain-apply args
          (if (null? value)
              (option '())
              (f value))))

      (define (%filter pred . args)
        (chain-apply args
          (if (or (null? value) (not (pred value)))
              (option '())
              (option value))))

      )

    (define (none) (option '()))

    ) ; end of begin
  ) ; end of define-library
