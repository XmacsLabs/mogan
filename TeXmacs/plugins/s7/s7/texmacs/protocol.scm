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

(define-library (texmacs protocol)
(export (flush-verbatim))
(begin
(define (data-begin)
  (display (integer->char 2)))

(define (data-end)
  (display (integer->char 5))
  (flush-output-port))

(define (data-escape)
  (write (integer->char 27)))

(define (flush-verbatim msg)
  (data-begin)
  (display "verbatim:")
  (display msg)
  (data-end))

)
)