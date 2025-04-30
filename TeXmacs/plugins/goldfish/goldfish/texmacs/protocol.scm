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
  DATA_BEGIN DATA_END DATA_COMMAND DATA_ESCAPE
  flush-verbatim flush-prompt flush-scheme flush-scheme-u8 flush-file
  flush-markdown flush-latex flush-command
  read-paragraph-by-visible-eof
)
(begin

(define DATA_BEGIN (integer->char 2))
(define DATA_END (integer->char 5))
(define DATA_COMMAND (integer->char 16))
(define DATA_ESCAPE (integer->char 27))

(define (data-begin)
  (display DATA_BEGIN))

(define (data-end)
  (display DATA_END)
  (flush-output-port))

(define (data-escape)
  (write DATA_ESCAPE))

(define (flush-any msg)
  (data-begin)
  (display msg)
  (data-end))

(define (flush-verbatim msg)
  (flush-any (string-append "verbatim:" msg)))

(define (flush-scheme msg)
  (if (string? msg)
    (flush-any (string-append "scheme:" msg))
    (flush-any (string-append "scheme:" (object->string msg)))))

(define (flush-scheme-u8 msg)
  (if (string? msg)
    (flush-any (string-append "scheme_u8:" msg))
    (flush-any (string-append "scheme_u8:" (object->string msg)))))

(define (flush-prompt msg)
  (flush-any (string-append "prompt#" msg)))

(define (flush-file path)
  (flush-any (string-append "file:" path)))

(define (flush-markdown msg)
  (flush-any (string-append "markdown:" msg)))

(define (flush-command msg)
  (if (string? msg)
    (flush-any (string-append "command:" msg))
    (flush-any (string-append "command:" (object->string msg)))))

(define (flush-latex msg)
  (flush-any (string-append "latex:" msg)))

(define (read-paragraph-by-visible-eof)
  (define (read-code code)
    (let ((line (read-line)))
      (if (string=? line "<EOF>\n")
          code
          (read-code (string-append code line)))))

  (read-code ""))

) ; end of begin
) ; end of define-library
