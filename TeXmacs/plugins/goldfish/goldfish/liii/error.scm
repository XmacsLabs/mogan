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
(import (scheme process-context))
(begin

(define (os-error msg)
  (error 'os-error msg))

(define (file-not-found-error msg)
  (error 'file-not-found-error msg))

(define (not-a-directory-error msg)
  (error 'not-a-directory-error msg))

(define (file-exists-error msg)
  (error 'file-exists-error msg))

(define (timeout-error args)
  (apply (cons 'timeout-error args) error))

(define (type-error args)
  (apply (cons 'type-error args) error))

; nice Scala style to throw the not-implemented-error
(define (???)
  (error 'not-implemented-error))

) ; begin
) ; define-library
