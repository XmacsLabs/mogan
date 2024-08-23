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

(define-library (scheme base)
(export
  let-values
  square
  ; String
  string-copy
  ; Vector
  vector->string
  string->vector
  vector-copy
  vector-copy!
  ; Input and Output
  call-with-port port? binary-port? textual-port?
  input-port-open? output-port-open?
  open-binary-input-file open-binary-output-file
  close-port
  eof-object
  ; Control flow
  string-map vector-map string-for-each vector-for-each
  ; Exception
  raise guard read-error? file-error?)
(begin

; 0-clause BSD
; Bill Schottstaedt
; from S7 source repo: r7rs.scm
(define-macro (let-values vars . body)
  (if (and (pair? vars)
           (pair? (car vars))
           (null? (cdar vars)))
      `((lambda ,(caar vars)
          ,@body)
        ,(cadar vars))
      `(with-let
        (apply sublet (curlet)
          (list
            ,@(map
               (lambda (v)
                 `((lambda ,(car v)
                     (values ,@(map (lambda (name)
                       (values (symbol->keyword name) name))
                         (let args->proper-list ((args (car v)))
                           (cond ((symbol? args)
                                  (list args))
                                 ((not (pair? args))
                                  args)
                                 ((pair? (car args))
                                  (cons (caar args)
                                        (args->proper-list (cdr args))))
                                 (else
                                  (cons (car args)
                                        (args->proper-list (cdr args)))))))))
                   ,(cadr v)))
                vars)))
        ,@body)))

(define (square x) (* x x))

(define (string-copy str . start_end)
  (cond ((null? start_end)
         (substring str 0))
        ((= (length start_end) 1)
          (substring str (car start_end)))
        ((= (length start_end) 2)
         (substring str (car start_end) (cadr start_end)))
        (else (error 'wrong-number-of-args))))

; 0-clause BSD
; Bill Schottstaedt
; from S7 source repo: r7rs.scm
(define* (vector->string v (start 0) end) 
  (let ((stop (or end (length v)))) 
    (copy v (make-string (- stop start)) start stop)))

; 0-clause BSD
; Bill Schottstaedt
; from S7 source repo: r7rs.scm
(define* (string->vector s (start 0) end)
  (let ((stop (or end (length s)))) 
    (copy s (make-vector (- stop start)) start stop)))

(define* (vector-copy v (start 0) (end (vector-length v)))
  (if (or (> start end) (> end (vector-length v)))
      (error 'out-of-range "vector-copy")
      (let ((new-v (make-vector (- end start))))
        (let loop ((i start) (j 0))
          (if (>= i end)
              new-v
              (begin
                (vector-set! new-v j (vector-ref v i))
                (loop (+ i 1) (+ j 1))))))))

(define* (vector-copy! to at from (start 0) (end (vector-length from)))
  (if (or (< at 0)
          (> start (vector-length from))
          (< end 0)
          (> end (vector-length from))
          (> start end)
          (> (+ at (- end start)) (vector-length to)))
      (error 'out-of-range "vector-copy!")
      (let loop ((to-i at) (from-i start))
        (if (>= from-i end)
            to
            (begin
              (vector-set! to to-i (vector-ref from from-i))
              (loop (+ to-i 1) (+ from-i 1)))))))

(define vector-fill! fill!)

(define (string-map p . args) (apply string (apply map p args)))

(define (vector-map p . args) (apply vector (apply map p args)))

(define string-for-each for-each)

(define vector-for-each for-each)

(define (raise . args)
  (apply throw #t args))

(define-macro (guard results . body)
  `(let ((,(car results) 
          (catch #t 
            (lambda () 
              ,@body) 
            (lambda (type info)
              (if (pair? (*s7* 'catches))
                  (lambda () (apply throw type info))
                  (car info))))))
     (cond ,@(cdr results)
           (else 
            (if (procedure? ,(car results)) 
                (,(car results))
                ,(car results))))))

(define (read-error? obj) (eq? (car obj) 'read-error))

(define (file-error? obj) (eq? (car obj) 'io-error))

(define (call-with-port port proc)
  (let ((res (proc port)))
    (if res (close-port port))
    res))

(define (port? p) (or (input-port? p) (output-port? p)))
(define binary-port? port?)
(define textual-port? port?)

(define (input-port-open? p) (not (port-closed? p)))
(define (output-port-open? p) (not (port-closed? p)))

(define (close-port p)
  (if (input-port? p)
      (close-input-port p)
      (close-output-port p)))

(define (eof-object) #<eof>)

) ; end of begin
) ; end of define-library
