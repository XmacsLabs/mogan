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

(import (texmacs protocol)
        (liii os)
        (liii uuid))

(define (gnuplot-welcome)
  (flush-prompt "> ")
  (flush-verbatim
    (string-append
      "Gnuplot session by XmacsLabs\n"
      "implemented on Goldfish Scheme (" (version) ")")))

(define (gnuplot-read-code)
  (define (read-code code)
    (let ((line (read-line)))
      (if (string=? line "<EOF>\n")
          code
          (read-code (string-append code line)))))

  (read-code ""))

(define (gen-temp-path)
  (string-append (os-temp-dir) "/" (uuid4)))

(define (gen-png-precode png-path)
  (string-append
    "reset\n"
    "set terminal pngcairo enhanced\n"
    "set output\n"
    "set output '" png-path "'\n"
    "set size 1,1\n"))

(define (gnuplot-dump-code code-path png-path code)
  (with-output-to-file code-path
    (lambda ()
      (display (gen-png-precode png-path))
      (display code))))

(define (gnuplot-plot code-path)
  (os-call (string-append "gnuplot" " " "-c" " " code-path)))

(define (eval-and-print code)
  (let* ((temp-path (gen-temp-path))
         (png-path (string-append temp-path ".png"))
         (code-path (string-append temp-path ".gnuplot")))
    (gnuplot-dump-code code-path png-path code)
    (gnuplot-plot code-path)
    (flush-file png-path)))

(define (read-eval-print)
  (let ((code (gnuplot-read-code)))
    (if (string=? code "")
      #t
      (eval-and-print code))))

(define (gnuplot-repl)
  (begin (read-eval-print)
         (gnuplot-repl)))

(gnuplot-welcome)
(gnuplot-repl)
