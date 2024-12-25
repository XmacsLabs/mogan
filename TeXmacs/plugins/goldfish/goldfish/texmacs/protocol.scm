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

(define-library (texmacs protocol)
(export
  data-begin data-end data-escape
  flush-verbatim flush-prompt flush-scheme flush-scheme-u8 flush-file
  read-paragraph-by-visible-eof
)
(begin

(define (data-begin)
  (display (integer->char 2)))

(define (data-end)
  (display (integer->char 5))
  (flush-output-port))

(define (data-escape)
  (write (integer->char 27)))

(define (flush-any msg)
  (data-begin)
  (display msg)
  (data-end))

(define (flush-verbatim msg)
  (flush-any (string-append "verbatim:" msg)))

(define (flush-scheme msg)
  (flush-any (string-append "scheme:" msg)))

(define (flush-scheme-u8 msg)
  (flush-any (string-append "scheme_u8:" msg)))

(define (flush-prompt msg)
  (flush-any (string-append "prompt#" msg)))

(define (flush-file path)
  (flush-any (string-append "file:" path)))

(define (read-paragraph-by-visible-eof)
  (define (read-code code)
    (let ((line (read-line)))
      (if (string=? line "<EOF>\n")
          code
          (read-code (string-append code line)))))

  (read-code ""))

) ; end of begin
) ; end of define-library
