;
; Copyright (C) 2024 The Mogan STEM Suite Authors
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
        (liii uuid)
        (liii sys)
        (srfi srfi-1))

(define (gnuplot-welcome)
  (let ((format (last (argv))))
    (flush-prompt (string-append format "] "))
    (flush-verbatim
      (string-append
        "Gnuplot session by XmacsLabs\n"
        "implemented on Goldfish Scheme (" (version) ")"))))

(define (gnuplot-read-code)
  (define (read-code code)
    (let ((line (read-line)))
      (if (string=? line "<EOF>\n")
          code
          (read-code (string-append code line)))))

  (read-code ""))

(define (gen-temp-path)
  (let ((gnuplot-tmpdir (string-append (os-temp-dir) "/gnuplot")))
    (when (not (file-exists? gnuplot-tmpdir))
      (mkdir gnuplot-tmpdir))
    (string-append gnuplot-tmpdir "/" (uuid4))))

(define (gen-eps-precode eps-path)
  (string-append
   "reset\n"
   "set terminal postscript eps enhanced\n"
   "set output\n"
   "set output '" eps-path "'\n"
   "set size 1,1\n"
   "set autoscale\n"))

(define (gen-png-precode png-path)
  (string-append
    "reset\n"
    "set terminal pngcairo enhanced\n"
    "set output\n"
    "set output '" png-path "'\n"
    "set size 1,1\n"))

(define (gen-svg-precode svg-path)
  (string-append
    "reset\n"
    "set terminal svg enhanced\n"
    "set output\n"
    "set output '" svg-path "'\n"
    "set size 1,1\n"
    "set autoscale\n"))

(define (gen-precode format path)
  (cond ((string=? format "png") (gen-png-precode path))
        ((string=? format "svg") (gen-svg-precode path))
        ((string=? format "eps") (gen-eps-precode path))
        (else (error 'wrong-args))))

(define (gnuplot-dump-code code-path image-format image-path code)
  (with-output-to-file code-path
    (lambda ()
      (display (gen-precode image-format image-path))
      (display code))))

(define (gnuplot-plot code-path)
  (let ((cmd (fourth (argv))))
    (os-call (string-append cmd " " "-c" " " code-path))))

(define (eval-and-print code)
  (let* ((format (last (argv)))
         (temp-path (gen-temp-path))
         (image-path (string-append temp-path "." format))
         (code-path (string-append temp-path ".gnuplot")))
    (gnuplot-dump-code code-path format image-path code)
    (gnuplot-plot code-path)
    (flush-file image-path)))

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
