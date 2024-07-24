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

(import (texmacs protocol)
        (chezscheme))

(define (chez-welcome)
  (flush-prompt "> ")
  (flush-verbatim
    (string-append (scheme-version) " (Session made by XmacsLabs)")))

(define (chez-repl)
  (define (string-join l)
    (cond ((null? l) "")
          ((= (length l) 1) (car l))
          (else
            (string-append
              (car l)
              ""
              (string-join (cdr l))))))

  (define (read-line)
    (let loop ((chars '()) (char (read-char)))
      (cond ((eof-object? char) (list->string (reverse chars)))
            ((char=? char #\newline) (list->string (reverse chars)))
            ((char=? char #\return) (list->string (reverse chars)))
            (else (loop (cons char chars) (read-char))))))

  (define (chez-read-code)
    (define (read-code code)
      (let ((line (read-line)))
        (if (string=? line "<EOF>")
            code
            (read-code (string-append code line)))))

    (read-code ""))

  (define (escape-string str)
    (string-join
      (map (lambda (char)
             (if (char=? char #\")
                 (string #\\ #\")
                 (if (char=? char #\\)
                     (string #\\ #\\)
                     (string char))))
           (string->list str))))

  (define (chez-quote s)
    (string-append "\"" (escape-string s) "\""))

  (define (object->string obj)
    (let ((port (open-output-string)));
      (write obj port)
      (get-output-string port)))

  (define (build-chez-result obj)
    (let ((output (object->string obj)))
      (string-append "(chez-result " (chez-quote output) ")")))

  (define (chez-print obj)
    (if (eq? obj (void))
      (flush-scheme "")
      (flush-scheme (build-chez-result obj))))

  (define (eval-string str)
    (let ([p (open-string-input-port str)])
      (eval (get-datum p))))

  (define (eval-and-print code)
    (chez-print (eval-string code)))
    ;(catch #t
    ;  (lambda ()
    ;  (lambda args
    ;    (begin
    ;      (flush-scheme
    ;        (string-append "(errput (document "
    ;          (chez-quote (symbol->string (car args)))
    ;          (chez-quote (apply format #f (cadr args)))
    ;          "))"))))))

  (define (read-eval-print)
    (let ((code (chez-read-code)))
      (if (string=? code "")
        #t
        (eval-and-print code))))

  (begin (read-eval-print)
         (chez-repl)))

(chez-welcome)
(chez-repl)
