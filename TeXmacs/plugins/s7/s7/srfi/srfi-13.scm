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

(define-library (srfi srfi-13)
  (import
    (scheme base)
    (srfi srfi-1))
  (export
    string-null? string-copy string-join
    string-every string-any
    string-take string-take-right string-drop string-drop-right 
    string-pad string-pad-right
    string-trim string-trim-right string-trim-both
    string-prefix? string-suffix? 
    string-index string-index-right
    string-contains string-count
    string-reverse
    string-tokenize)
(begin

(define (%string-from-range str start_end)
  (cond ((null-list? start_end) str)
        ((= (length start_end) 1)
          (substring str (car start_end)))
        ((= (length start_end) 2)
         (substring str (first start_end) (second start_end)))
        (else (error 'wrong-number-of-args "%string-from-range"))))

(define (%make-criterion char/pred?)
  (cond ((char? char/pred?) (lambda (x) (char=? x char/pred?)))
        ((procedure? char/pred?) char/pred?)
        (else (error 'wrong-type-arg "%make-criterion"))))

(define (string-join l . delim+grammer)
  (define (extract-params params-l)
    (cond ((null-list? params-l)
           (list "" 'infix))
          ((and (= (length params-l) 1)
                (string? (car params-l)))
           (list (car params-l) 'infix))
          ((and (= (length params-l) 2)
                (string? (first params-l))
                (symbol? (second params-l)))
           params-l)
          ((> (length params-l) 2)
           (error 'wrong-number-of-args "optional params in string-join"))
          (else (error 'wrong-type-arg "optional params in string-join"))))
  
  (define (string-join-sub l delim)
    (cond ((null-list? l) "")
          ((= (length l) 1) (car l))
          (else
            (string-append
              (car l)
              delim
              (string-join-sub (cdr l) delim)))))
  
  (let* ((params  (extract-params delim+grammer))
         (delim   (first params))
         (grammer (second params))
         (ret     (string-join-sub l delim)))
    (case grammer
      ('infix ret)
      ('strict-infix
       (if (null-list? l)
           (error 'wrong-type-arg "empty list not allowed")
           ret))
      ('suffix
        (if (null-list? l) "" (string-append ret delim)))
      ('prefix
        (if (null-list? l) "" (string-append delim ret)))
      (else (error 'wrong-type-arg "invalid grammer")))))

(define (string-null? str)
  (and (string? str) 
       ((lambda (x) (= x 0)) (string-length str))))

(define (string-every char/pred? str . start+end)
  (define (string-every-sub pred? str)
    (let
      loop ((i 0) (len (string-length str)))
      (or (= i len)
          (and (pred? (string-ref str i)) 
               (loop (+ i 1) len)))))
  
  (let ((str-sub (%string-from-range str start+end))
        (criterion (%make-criterion char/pred?)))
    (string-every-sub criterion str-sub)))

(define (string-any char/pred? str . start+end)
  (define (string-any-sub pred? str)
    (let
      loop ((i 0) (len (string-length str)))
      (if (= i len)
          #f   
          (or (pred? (string-ref str i)) 
              (loop (+ i 1) len)))))
  
  (let ((str_sub (%string-from-range str start+end))
        (criterion (%make-criterion char/pred?)))
    (string-any-sub criterion str_sub)))

(define (string-take str k)
  (list->string (take (string->list str) k)))

(define (string-take-right str k)
  (list->string (take-right (string->list str) k)))

(define (string-drop str k)
  (list->string (drop (string->list str) k)))

(define (string-drop-right str k)
  (list->string (drop-right (string->list str) k)))

(define (string-pad str len . char+start+end)
  (define (string-pad-sub str len ch)
    (let ((orig-len (string-length str)))
      (if (< len orig-len)
          (string-take-right str len)
          (string-append (make-string (- len orig-len) ch) str))))
  
  (cond ((null-list? char+start+end)
         (string-pad-sub str len #\ ))
        ((list? char+start+end)
         (string-pad-sub
           (%string-from-range str (cdr char+start+end))
           len
           (car char+start+end)))
        (else (error 'wrong-type-arg "string-pad"))))

(define (string-pad-right str len . char+start+end)
  (define (string-pad-right-sub str len ch)
    (let ((orig-len (string-length str)))
      (if (< len orig-len)
          (string-take str len)
          (string-append str (make-string (- len orig-len) ch)))))
  
  (cond ((null-list? char+start+end)
         (string-pad-right-sub str len #\ ))
        ((list? char+start+end)
         (string-pad-right-sub
           (%string-from-range str (cdr char+start+end))
           len
           (car char+start+end)))
        (else (error 'wrong-type-arg "string-pad"))))

(define (%trim-do str string-trim-sub criterion+start+end)
  (cond ((null-list? criterion+start+end)
         (string-trim-sub str #\ ))
        ((list? criterion+start+end)
         (if (char? (car criterion+start+end))
             (string-trim-sub
               (%string-from-range str (cdr criterion+start+end))
               (car criterion+start+end))
             (string-trim-sub
               (%string-from-range str criterion+start+end)
               #\ )))
        (else (error 'wrong-type-arg "string-trim"))))

(define (string-trim str . criterion+start+end)
  (define (string-trim-sub str space-or-char)
    (let loop ((i 0)
               (len (string-length str)))
         (if (or (= i len) (not (char=? space-or-char (string-ref str i))))
             (substring str i len)
             (loop (+ i 1) len))))
  (%trim-do str string-trim-sub criterion+start+end))

(define (string-trim-right str . criterion+start+end)
  (define (string-trim-right-sub str space-or-char)
    (let loop ((i (- (string-length str) 1)))
      (cond ((negative? i) "")
            ((char=? space-or-char (string-ref str i)) (loop (- i 1)))
            (else (substring str 0 (+ i 1))))))
  (%trim-do str string-trim-right-sub criterion+start+end))

(define (string-trim-both str . criterion+start+end)
  (define (string-trim-both-sub str space-or-char)
    (let loop ((i 0)
             (len (string-length str)))
    (if (or (= i len) (not (char=? space-or-char (string-ref str i))))
        (let loop-end ((j (- len 1)))
          (if (or (< j 0) (not (char=? space-or-char (string-ref str j))))
              (substring str i (+ j 1))
              (loop-end (- j 1))))
        (loop (+ i 1) len))))
  (%trim-do str string-trim-both-sub criterion+start+end))

(define (string-prefix? prefix str)
  (let* ((prefix-len (string-length prefix))
         (str-len (string-length str)))
    (and (<= prefix-len str-len)
         (let loop ((i 0))
           (or (= i prefix-len)
               (and (char=? (string-ref prefix i)
                            (string-ref str i))
                    (loop (+ i 1))))))))

(define (string-suffix? suffix str)
  (let* ((suffix-len (string-length suffix))
         (str-len (string-length str)))
    (and (<= suffix-len str-len)
         (let loop ((i 0)
                    (j (- str-len suffix-len)))
           (or (= i suffix-len)
               (and (char=? (string-ref suffix i)
                            (string-ref str j))
                    (loop (+ i 1) (+ j 1))))))))

(define (string-index str char/pred? . start+end)
  (define (string-index-sub str pred?)
    (let loop ((i 0))
      (cond ((>= i (string-length str)) #f)
            ((pred? (string-ref str i)) i)
            (else (loop (+ i 1))))))
  
  (let* ((start (if (null-list? start+end) 0 (car start+end)))
         (str-sub (%string-from-range str start+end))
         (pred? (%make-criterion char/pred?))
         (ret (string-index-sub str-sub pred?)))
    (if ret (+ start ret) ret)))

(define (string-index-right str char/pred? . start+end)
  (define (string-index-right-sub str pred?)
    (let loop ((i (- (string-length str) 1)))
      (cond ((< i 0) #f)
            ((pred? (string-ref str i)) i)
            (else (loop (- i 1))))))
  
  (let* ((start (if (null-list? start+end) 0 (car start+end)))
        (str-sub (%string-from-range str start+end))
        (pred? (%make-criterion char/pred?))
        (ret (string-index-right-sub str-sub pred?)))
    (if ret (+ start ret) ret)))

(define (string-contains str sub-str)
  (let loop ((i 0))
    (let ((len (string-length str))
          (sub-str-len (string-length sub-str)))
      (if (> i (- len sub-str-len)) 
          #f
          (if (string=? 
                (substring
                  str
                  i
                  (+ i sub-str-len))
                sub-str)
              #t
              (loop (+ i 1)))))))

(define (string-count str char/pred? . start+end)
  (let ((str-sub (%string-from-range str start+end))
        (criterion (%make-criterion char/pred?)))
    (count criterion (string->list str-sub))))

(define (string-reverse str . start+end)
  (cond ((null-list? start+end)
         (reverse str))
        ((= (length start+end) 1)
         (let ((start (first start+end)))
           (string-append (substring str 0 start)
                          (reverse (substring str start)))))
        ((= (length start+end) 2)
         (let ((start (first start+end))
               (end (second start+end)))
           (string-append (substring str 0 start)
                          (reverse (substring str start end))
                          (substring str end))))
        (else (error 'wrong-number-of-args "string-reverse"))))

(define (string-tokenize str . char+start+end)
  
  (define (string-tokenize-sub str char)
    
    (define (tokenize-helper tokens cursor)
      (let ((sep-pos/false (string-index str char cursor)))
        (if (not sep-pos/false)
            (reverse (cons (substring str cursor) tokens))
            (let ((new-tokens
                   (if (= cursor sep-pos/false)
                       tokens
                       (cons (substring str cursor sep-pos/false) tokens)))
                  (next-cursor (+ sep-pos/false 1)))
              (tokenize-helper new-tokens next-cursor)))))
  
      (tokenize-helper '() 0))
  
  (cond ((null-list? char+start+end)
         (string-tokenize-sub str #\ ))
        ((list? char+start+end)
         (string-tokenize-sub
               (%string-from-range str (cdr char+start+end))
               (car char+start+end)))
        (else (error 'wrong-type-arg "string-tokenize"))))

) ; end of begin
) ; end of define-library

