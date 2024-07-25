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

(import (texmacs protocol))

(define (goldfish-welcome)
  (flush-prompt "> ")
  (flush-verbatim
    (string-append "S7 Scheme: " (substring (*goldfish* 'version) 3))))

(define (goldfish-repl)
  ; SRFI 1
  (define (find pred l)
    (cond ((null? l) #f)
          ((pred (car l)) (car l)) 
          (else (find pred (cdr l)))))

  ; SRFI 13
  (define (string-join l)
    (cond ((null? l) "")
          ((= (length l) 1) (car l))
          (else
            (string-append
              (car l)
              (string-join (cdr l))))))

  (define (string-prefix? prefix str)
    (let* ((prefix-len (length prefix))
           (str-len (length str)))
      (and (<= prefix-len str-len)
           (let loop ((i 0))
             (or (= i prefix-len)
                 (and (char=? (string-ref prefix i)
                              (string-ref str i))
                      (loop (+ i 1))))))))

  (define (goldfish-read-code)
    (define (read-code code)
      (let ((line (read-line)))
        (if (string=? line "<EOF>\n")
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

  (define (goldfish-quote s)
    (string-append "\"" (escape-string s) "\""))

  (define (build-goldfish-result obj)
    (let ((output (object->string obj))
          (leadings (list "(document" "(math" "(equation*" "(align" "(with" "(graphics")))
      (if (find (lambda (x) (string-prefix? x output)) leadings)
          output
          (string-append "(goldfish-result " (s7-quote output) ")"))))

  (define (goldfish-print obj)
    (if (eq? obj #<unspecified>)
      (flush-scheme "")
      (flush-scheme (build-goldfish-result obj))))

  (define (eval-and-print code)
    (catch #t
      (lambda ()
        (goldfish-print (eval-string code (rootlet))))
      (lambda args
        (begin
          (flush-scheme
            (string-append "(errput (document "
              (goldfish-quote (symbol->string (car args)))
              (goldfish-quote (apply format #f (cadr args)))
              "))"))))))

  (define (read-eval-print)
    (let ((code (goldfish-read-code)))
      (if (string=? code "")
        #t
        (eval-and-print code))))

  (begin (read-eval-print)
         (goldfish-repl)))

(goldfish-welcome)
(goldfish-repl)
