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

; see https://docs.python.org/3/library/exceptions.html#exception-hierarchy
(define-library (liii error)
(export ???
  os-error file-not-found-error not-a-directory-error file-exists-error
  timeout-error
  type-error value-error)
(begin

(define (os-error . args)
  (apply error (cons 'os-error args)))

(define (file-not-found-error . args)
  (apply error (cons 'file-not-found-error args)))

(define (not-a-directory-error . args)
  (apply error (cons 'not-a-directory-error args)))

(define (file-exists-error . args)
  (apply error (cons 'file-exists-error args)))

(define (timeout-error . args)
  (apply error (cons 'timeout-error args)))

(define (type-error . args)
  (apply error (cons 'type-error args)))

(define (value-error . args)
  (apply error (cons 'value-error args)))

(define (???)
  (error 'not-implemented-error "???"))

) ; begin
) ; define-library

