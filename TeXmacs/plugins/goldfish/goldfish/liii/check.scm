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

(define-library (liii check)
(export test check check-set-mode! check:proc
  check-catch check-report check-failed?
  check-true check-false)
(import (srfi srfi-78)
        (rename (srfi srfi-78)
                (check-report srfi-78-check-report)))
(begin

(define (check-report . msg)
  (if (not (null? msg))
    (begin
      (display (car msg))))
  (srfi-78-check-report)
  (if (check-failed?) (exit -1)))

(define-macro (test left right)
  `(check ,left => ,right))

(define-macro (check-true body)
  `(check ,body => #t))

(define-macro (check-false body)
  `(check ,body => #f))

(define-macro (check-catch error-id body)
  `(check
    (catch ,error-id
      (lambda () ,body)
      (lambda args ,error-id))
      => ,error-id))


) ; end of begin
) ; end of define-library
