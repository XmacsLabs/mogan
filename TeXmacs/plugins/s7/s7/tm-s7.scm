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
  (cons (append (getenv "TEXMACS_HOME_PATH") "plugins/s7/s7")
        *load-path*))

(import (texmacs protocol))

(define (s7-welcome)
  (flush-verbatim
    (append "S7 Scheme: " (substring (*s7* 'version) 3))))

(define (eval-and-print line)
  (let ((result (eval-string line)))
    (if result
      (begin 
        (display "<=\n")
        (display "=> ")
        (display result)
        (display "<=\n")))))

(define (read-eval-print)
  (let ((line (read-line *stdin*)))
    (cond 
      ((string=? line "\n")
       #t)
      (else
       (eval-and-print line)
       #t))))

(define (loop)
  (if (read-eval-print)
      (loop)
      (display "Bye!\n")))

(s7-welcome)
