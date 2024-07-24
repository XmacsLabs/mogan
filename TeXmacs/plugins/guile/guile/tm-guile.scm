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

(use-modules (texmacs protocol)
             (ice-9 rdelim))

(define (guile-welcome)
  (flush-prompt "> ")
  (flush-verbatim
    (string-append "GNU Guile (" (version) ") session by XmacsLabs")))

(define (guile-repl)
  (define (string-concat l)
    (cond ((null? l) "")
          ((= (length l) 1) (car l))
          (else
            (string-append
              (car l)
              (string-concat (cdr l))))))

  (define (guile-read-code)
    (define (read-code code)
      (let ((line (read-line)))
        (if (string=? line "<EOF>")
            code
            (read-code (string-append code line)))))

    (read-code ""))

  (define (escape-string str)
    (string-concat
      (map (lambda (char)
             (if (char=? char #\")
                 (string #\\ #\")
                 (if (char=? char #\\)
                     (string #\\ #\\)
                     (string char))))
           (string->list str))))

  (define (guile-quote s)
    (string-append "\"" (escape-string s) "\""))

  (define (build-guile-result obj)
    (let ((output (object->string obj)))
      (string-append "(guile-result " (guile-quote output) ")")))

  (define (guile-print obj)
    (if (unspecified? obj)
      (flush-scheme "")
      (flush-scheme (build-guile-result obj))))

  (define (eval-and-print code)
    (catch #t
      (lambda ()
        (guile-print (eval-string code)))
      (lambda (key . args)
        (begin
          (flush-scheme
            (string-append "(errput (document "
              (guile-quote (symbol->string key))
              (guile-quote (car args))
              (if (and (>= (length args) 2) (string? (cadr args)))
                  (guile-quote (cadr args))
                  "")
              "))"))))))

  (define (read-eval-print)
    (let ((code (guile-read-code)))
      (if (string=? code "")
        #t
        (eval-and-print code))))

  (begin (read-eval-print)
         (guile-repl)))

(guile-welcome)
(guile-repl)
