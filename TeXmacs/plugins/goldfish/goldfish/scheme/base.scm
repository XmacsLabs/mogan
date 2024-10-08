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
  ; R7RS 5: Program Structure
  define-values define-record-type
  ; R7RS 6.2: Numbers
  square
  exact inexact
  floor s7-floor ceiling s7-ceiling truncate s7-truncate round s7-round
  floor-quotient
  gcd lcm s7-lcm
  boolean=?
  ; R7RS 6.4: list
  pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr
  null? list? make-list list length append reverse list-tail
  list-ref list-set! memq memv member assq assv assoc list-copy
  ; R7RS 6.5: Symbol
  symbol? symbol=? string->symbol symbol->string
  ; R7RS 6.6: Characters
  digit-value
  ; R7RS 6.7: String
  string-copy
  ; R7RS 6.8: Vector
  vector->string string->vector
  vector-copy vector-copy! vector-fill!
  ; R7RS 6.9: Bytevectors
  bytevector? make-bytevector bytevector bytevector-length
  bytevector-u8-ref bytevector-u8-set! bytevector-append
  utf8->string string->utf8 u8-string-length
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

; 0-clause BSD by Bill Schottstaedt from S7 source repo: s7test.scm
(define-macro (define-values vars expression)
  `(if (not (null? ',vars))
       (varlet (curlet) ((lambda ,vars (curlet)) ,expression))))

; 0-clause BSD by Bill Schottstaedt from S7 source repo: r7rs.scm
(define-macro (define-record-type type make ? . fields)
  (let ((obj (gensym))
        (typ (gensym)) ; this means each call on this macro makes a new type
        (args (map (lambda (field)
                     (values (list 'quote (car field))
                             (let ((par (memq (car field) (cdr make))))
                               (and (pair? par) (car par)))))
                   fields)))
    `(begin
       (define (,? ,obj)
         (and (let? ,obj)
              (eq? (let-ref ,obj ',typ) ',type)))
       
       (define ,make 
         (inlet ',typ ',type ,@args))

       ,@(map
          (lambda (field)
            (when (pair? field)
              (if (null? (cdr field))
                  (values)
                  (if (null? (cddr field))
                      `(define (,(cadr field) ,obj)
                         (let-ref ,obj ',(car field)))
                      `(begin
                         (define (,(cadr field) ,obj)
                           (let-ref ,obj ',(car field)))
                         (define (,(caddr field) ,obj val)
                           (let-set! ,obj ',(car field) val)))))))
          fields)
       ',type)))

(define exact inexact->exact)

(define inexact exact->inexact)

(define s7-floor floor)

(define (floor x)
  (if (inexact? x)
      (inexact (s7-floor x))
      (s7-floor x)))

(define s7-ceiling ceiling)

(define (ceiling x)
  (if (inexact? x)
      (inexact (s7-ceiling x))
      (s7-ceiling x)))

(define s7-truncate truncate)

(define (truncate x)
  (if (inexact? x)
      (inexact (s7-truncate x))
      (s7-truncate x)))

(define s7-round round)

(define (round x)
  (if (inexact? x)
      (inexact (s7-round x))
      (s7-round x)))

(define (floor-quotient x y) (floor (/ x y)))

(define s7-lcm lcm)

(define (lcm2 x y)
  (cond ((and (inexact? x) (exact? y))
         (inexact (s7-lcm (exact x) y)))
        ((and (exact? x) (inexact? y))
         (inexact (s7-lcm x (exact y))))
        ((and (inexact? x) (inexact? y))
         (inexact (s7-lcm (exact x) (exact y))))
        (else (s7-lcm x y))))

(define (lcm . args)
  (cond ((null? args) 1)
        ((null? (cdr args))
         (car args))
        ((null? (cddr args))
         (lcm2 (car args) (cadr args)))
        (else (apply lcm (cons (lcm (car args) (cadr args))
                               (cddr args))))))

(define (square x) (* x x))

(define (boolean=? obj1 obj2 . rest)
  (define (same-boolean obj rest)
    (if (null? rest)
        #t
        (and (equal? obj (car rest))
             (same-boolean obj (cdr rest)))))
  (cond ((not (boolean? obj1)) #f)
        ((not (boolean? obj2)) #f)
        ((not (equal? obj1 obj2)) #f)
        (else (same-boolean obj1 rest))))

(define (symbol=? sym1 sym2 . rest)
  (define (same-symbol sym rest)
    (if (null? rest)
        #t
        (and (eq? sym (car rest))
             (same-symbol sym (cdr rest)))))
  (cond ((not (symbol? sym1)) #f)
        ((not (symbol? sym2)) #f)
        ((not (eq? sym1 sym2)) #f)
        (else (same-symbol sym1 rest))))

(define bytevector byte-vector)

(define bytevector? byte-vector?)

(define make-bytevector make-byte-vector)

(define bytevector-length length)

(define bytevector-u8-ref byte-vector-ref)

(define bytevector-u8-set! byte-vector-set!)

(define bytevector-append append)

(define* (bytevector-advance-u8 bv index (end (length bv)))
  (if (>= index end)
      index  ; Reached the end without errors, sequence is valid
      (let ((byte (bv index)))
        (cond
         ;; 1-byte sequence (0xxxxxxx)
         ((< byte #x80)
          (+ index 1))
           
         ;; 2-byte sequence (110xxxxx 10xxxxxx)
         ((< byte #xe0)
          (if (>= (+ index 1) end)
              index  ; Incomplete sequence
              (let ((next-byte (bv (+ index 1))))
                (if (not (= (logand next-byte #xc0) #x80))
                    index  ; Invalid continuation byte
                    (+ index 2)))))
           
         ;; 3-byte sequence (1110xxxx 10xxxxxx 10xxxxxx)
         ((< byte #xf0)
          (if (>= (+ index 2) end)
              index  ; Incomplete sequence
              (let ((next-byte1 (bv (+ index 1)))
                    (next-byte2 (bv (+ index 2))))
                (if (or (not (= (logand next-byte1 #xc0) #x80))
                        (not (= (logand next-byte2 #xc0) #x80)))
                    index  ; Invalid continuation byte(s)
                    (+ index 3)))))
           
         ;; 4-byte sequence (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
         ((< byte #xf8)
          (if (>= (+ index 3) end)
              index  ; Incomplete sequence
              (let ((next-byte1 (bv (+ index 1)))
                    (next-byte2 (bv (+ index 2)))
                    (next-byte3 (bv (+ index 3))))
                (if (or (not (= (logand next-byte1 #xc0) #x80))
                        (not (= (logand next-byte2 #xc0) #x80))
                        (not (= (logand next-byte3 #xc0) #x80)))
                    index  ; Invalid continuation byte(s)
                    (+ index 4)))))
         (else index)))))  ; Invalid leading byte

(define (u8-string-length str)
  (let ((bv (string->byte-vector str))
        (N (string-length str)))
    (let loop ((pos 0) (cnt 0))
      (let ((next-pos (bytevector-advance-u8 bv pos N)))
        (cond
         ((= next-pos N)
          (+ cnt 1))
         ((= next-pos pos)
          (error 'value-error "Invalid UTF-8 sequence at index: " pos))
         (else (loop next-pos (+ cnt 1))))))))

(define* (utf8->string bv (start 0) (end (bytevector-length bv)))
  (if (or (< start 0) (> end (bytevector-length bv)) (> start end))
      (error 'out-of-range start end)
      (let loop ((pos start))
        (let ((next-pos (bytevector-advance-u8 bv pos end)))
          (cond
           ((= next-pos end)
            (copy bv (make-string (- end start)) start end))
           ((= next-pos pos)
            (error 'value-error "Invalid UTF-8 sequence at index: " pos))
           (else
            (loop next-pos)))))))

(define* (string->utf8 str (start 0) (end #t))
  ; start < end in this case
  (define (string->utf8-sub str start end)
    (let ((bv (string->byte-vector str))
          (N (string-length str)))
      (let loop ((pos 0) (cnt 0) (start-pos 0))
        (let ((next-pos (bytevector-advance-u8 bv pos N)))
          (cond
           ((and (not (zero? start)) (zero? start-pos) (= cnt start))
            (loop next-pos (+ cnt 1) pos))
           ((and (integer? end) (= cnt end))
            (copy bv (make-byte-vector (- pos start-pos)) start-pos pos))
           ((and end (= next-pos N))
            (copy bv (make-byte-vector (- N start-pos)) start-pos N))
           ((= next-pos pos)
            (error 'value-error "Invalid UTF-8 sequence at index: " pos))
           (else
            (loop next-pos (+ cnt 1) start-pos)))))))
  
  (when (not (string? str))
    (error 'type-error "str must be string"))
  (let ((N (u8-string-length str)))
    (when (or (< start 0) (>= start N))
        (error 'out-of-range
               (string-append "start must >= 0 and < " (number->string N))))
    (when (and (integer? end) (or (< end 0) (>= end (+ N 1))))
          (error 'out-of-range
                 (string-append "end must >= 0 and < " (number->string (+ N 1)))))         
    (when (and (integer? end) (> start end))
          (error 'out-of-range "start <= end failed" start end))
    
    (if (and (integer? end) (= start end))
      (byte-vector)
      (string->utf8-sub str start end))))

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

(define textual-port? port?)

(define binary-port? port?)

(define (input-port-open? p) (not (port-closed? p)))

(define (output-port-open? p) (not (port-closed? p)))

(define (close-port p)
  (if (input-port? p)
      (close-input-port p)
      (close-output-port p)))

(define (eof-object) #<eof>)

; 0 clause BSD, from S7 repo r7rs.scm
(define list-copy copy)

(define (string-copy str . start_end)
  (cond ((null? start_end)
         (substring str 0))
        ((= (length start_end) 1)
          (substring str (car start_end)))
        ((= (length start_end) 2)
         (substring str (car start_end) (cadr start_end)))
        (else (error 'wrong-number-of-args))))

(define (string-map p . args) (apply string (apply map p args)))

(define string-for-each for-each)

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

(define (vector-map p . args) (apply vector (apply map p args)))

(define vector-for-each for-each)

(define vector-fill! fill!)

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

) ; end of begin
) ; end of define-library

