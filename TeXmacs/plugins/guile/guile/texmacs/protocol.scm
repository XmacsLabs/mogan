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

(define-module (texmacs protocol))

(define-public (data-begin)
  (display (integer->char 2)))

(define-public (data-end)
  (display (integer->char 5))
  (force-output))

(define-public (data-escape)
  (write (integer->char 27)))

(define-public (flush-any msg)
  (data-begin)
  (display msg)
  (data-end))

(define-public (flush-verbatim msg)
  (flush-any (string-append "verbatim:" msg)))

(define-public (flush-scheme msg)
  (flush-any (string-append "scheme:" msg)))

(define-public (flush-prompt msg)
  (flush-any (string-append "prompt#" msg)))
