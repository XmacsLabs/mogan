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

(set! *load-path*
  (cons (append (getenv "TEXMACS_PATH") "/plugins/s7/s7")
        *load-path*))

(import (texmacs protocol))

(define (s7-read-code)
  (define (read-code code)
    (let ((line (read-line)))
      (if (string=? line "<EOF>\n")
          code
          (read-code (append code line)))))

  (read-code ""))


(define (s7-print obj)
  (define (s7-dquote s)
    (append "\"\\\"" s "\\\"\""))

  (define (s7-quote s)
    (append "\"" s "\""))

  (define (write-to-string obj)
    (let ((port (open-output-string)))
      (write obj port)
      (get-output-string port)))

  (define (build-s7-result obj)
    (let ((output 
           (cond ((string? obj)
                  (s7-dquote obj))
                 (else
                  (s7-quote (write-to-string obj))))))
      (append "(s7-result " output ")")))

  (flush-scheme (build-s7-result obj)))

(define (eval-and-print code)
  (let ((result (eval-string code)))
    (if result (s7-print result))))

(define (read-eval-print)
  (let ((code (s7-read-code)))
    (if (string=? code "")
      #t
      (eval-and-print code))))

(define (s7-welcome)
  (flush-prompt "> ")
  (flush-verbatim
    (append "S7 Scheme: " (substring (*s7* 'version) 3))))

(define (s7-repl)
  (if (read-eval-print)
      (s7-repl)
      (display "Bye!\n")))

(s7-welcome)
(s7-repl)
