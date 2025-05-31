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

(define-library (liii lang)
                
(import (only (liii base)
              u8-string-length any? receive u8-substring)
        (only (liii oop)
              define-case-class display* == @ typed-define case-class? chained-define
              define-object define-class != chain-apply object->string)
        (only (liii string)
              string-join string-null? string-starts? string-contains string-trim
              string-trim-right string-trim-both string-remove-prefix string-remove-suffix string-pad
              string-pad-right)
        (only (liii vector)
              vector= vector-every vector-any vector-filter reverse-list->vector
              vector-index vector-index-right vector-fold vector-fold-right)
        (only (liii sort) list-stable-sort vector-stable-sort)
        (only (liii list)
              length=? iota take filter count
              drop every any take-right drop-right
              fold fold-right reduce take-while drop-while list-index)
        (only (liii hash-table)
              hash-table-update!/default hash-table-for-each hash-table-ref/default hash-table-contains? hash-table-delete!
              hash-table-count)
        (only (liii bitwise) bitwise-and bitwise-ior arithmetic-shift)
        (liii error))

(export
  @ typed-define define-case-class define-object define-class
  case-class? == != chained-define display* object->string
  option none either left right
  rich-integer rich-float rich-char rich-string
  rich-list rich-vector array rich-hash-table
  box $
)
(begin

(define (box x)
  (cond ((integer? x) (rich-integer x))
        ((rational? x) (rich-rational x))
        ((float? x) (rich-float x))
        ((char? x) (rich-char x))
        ((string? x) (rich-string x))
        ((list? x) (rich-list x))
        ((vector? x) (rich-vector x))
        ((hash-table? x) (rich-hash-table x))
        (else (type-error "box: x must be integer?, rational?, float?, char?, string?, list?, vector?, hash-table?"))))

(define ($ x . xs)
  (if (null? xs) (box x) (apply (box x) xs)))

(define-case-class rich-integer ((data integer?))

(define (%get) data)

(define (%to n) 
  (unless (integer? n) 
    (type-error 
      (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
              %to '(n) 'n "integer" (object->string n))))
  (if (< n data) 
    (rich-list (list)) 
    (rich-list (iota (+ (- n data) 1) data))))

(define (%until n) 
  (unless (integer? n) 
    (type-error 
      (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
              %until '(n) 'n "integer" (object->string n))))
  (if (<= n data) 
    (rich-list (list)) 
    (rich-list (iota (+ (- n data)) data))))

(define (%to-rich-char)
  (rich-char data))

(define (%to-string)
  (number->string data))

(define (@max-value) 9223372036854775807)

(define (@min-value) -9223372036854775808)

;;return exact integer
(define (%sqrt)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
      (inexact->exact (floor (sqrt data)))))

)

(define-case-class rich-rational ((data rational?))

(define (%get) data)

(define (%abs) 
  (if (< data 0)
      (- 0 data)
      data))
  
)

(define-case-class rich-float ((data float?))
                   
(define (%get) data)

(define (%abs) 
  (if (< data 0)
      (- 0 data)
      data))
  
(define (%to-string)
  (number->string data))

(define (%sqrt)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative float is undefined!         ** Got ~a **" data))
      (sqrt data)))

)

(define-case-class rich-char ((data any?))
  
(define code-point
  (cond ((char? data)
         (char->integer data))
        ((integer? data)
         (if (and (>= data 0) (<= data #x10FFFF))
             data
             (value-error "rich-char: code point out of range" data)))
        (else
         (type-error "rich-char: only accept char and integer"))))

(define (%equals that)
  (cond ((char? that)
         (= code-point (char->integer that)))
        ((rich-char :is-type-of that)
         (= code-point (that :to-integer)))
        (else #f)))

(define (%ascii?)
  (and (>= code-point 0) (<= code-point 127)))

;;  依赖于 R7RS 标准函数 integer->char 和 char-numeric?

(define (%numeric?)
  (if (and (>= code-point 0) (<= code-point 255))
      (let ((ch (integer->char code-point)))
        (char-numeric? ch))
      ;; 超出code-point范围返回false
      #f))

(define (%upper?)
  (and (>= code-point #x41) (<= code-point #x5A)))

(define (%lower?)
  (and (>= code-point #x61) (<= code-point #x7A)))

(define (%digit?)
  (or
   (and (>= code-point 48) (<= code-point 57))
   (and (>= code-point #xFF10) (<= code-point #xFF19))
   (and (>= code-point #x0660) (<= code-point #x0669))
   (and (>= code-point #x06F0) (<= code-point #x06F9))
   (and (>= code-point #x0966) (<= code-point #x096F))
   (and (>= code-point #x09E6) (<= code-point #x09EF))
   (and (>= code-point #x0A66) (<= code-point #x0A6F))
   (and (>= code-point #x0AE6) (<= code-point #x0AEF))
   (and (>= code-point #x0B66) (<= code-point #x0B6F))
   (and (>= code-point #x0BE6) (<= code-point #x0BEF))
   (and (>= code-point #x0C66) (<= code-point #x0C6F))
   (and (>= code-point #x0CE6) (<= code-point #x0CEF))
   (and (>= code-point #x0D66) (<= code-point #x0D6F))
   (and (>= code-point #x0E50) (<= code-point #x0E59))
   (and (>= code-point #x0ED0) (<= code-point #x0ED9))
   (and (>= code-point #x0F20) (<= code-point #x0F29))
   (and (>= code-point #x1040) (<= code-point #x1049))
   (and (>= code-point #x17E0) (<= code-point #x17E9))
   (and (>= code-point #x1810) (<= code-point #x1819))))
  
(define (%to-upper . args)
  (chain-apply args
    (rich-char
      (if (and (>= code-point #x61) (<= code-point #x7A))
          (bitwise-and code-point #b11011111)
          code-point))))

(define (%to-lower . args)
  (chain-apply args
    (rich-char
      (if (and (>= code-point #x41) (<= code-point #x5A))
          (bitwise-ior code-point #b00100000)
          code-point))))

(define (%to-bytevector)
  (cond
    ((<= code-point #x7F)
     (bytevector code-point))

    ((<= code-point #x7FF)
     (let ((byte1 (bitwise-ior #b11000000 (bitwise-and (arithmetic-shift code-point -6) #b00011111)))
           (byte2 (bitwise-ior #b10000000 (bitwise-and code-point #b00111111))))
       (bytevector byte1 byte2)))

    ((<= code-point #xFFFF)
     (let ((byte1 (bitwise-ior #b11100000 (bitwise-and (arithmetic-shift code-point -12) #b00001111)))
           (byte2 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift code-point -6) #b00111111)))
           (byte3 (bitwise-ior #b10000000 (bitwise-and code-point #b00111111))))
       (bytevector byte1 byte2 byte3)))

    ((<= code-point #x10FFFF)
     (let ((byte1 (bitwise-ior #b11110000 (bitwise-and (arithmetic-shift code-point -18) #b00000111)))
           (byte2 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift code-point -12) #b00111111)))
           (byte3 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift code-point -6) #b00111111)))
           (byte4 (bitwise-ior #b10000000 (bitwise-and code-point #b00111111))))
       (bytevector byte1 byte2 byte3 byte4)))

    (else
     (value-error "Invalid code point"))))

(define (@from-bytevector x)
  (define (utf8-byte-sequence->code-point byte-seq)
    (let ((len (bytevector-length byte-seq)))
      (cond
        ((= len 1)
         (bytevector-u8-ref byte-seq 0))
        ((= len 2)
         (let ((b1 (bytevector-u8-ref byte-seq 0))
               (b2 (bytevector-u8-ref byte-seq 1)))
           (bitwise-ior
            (arithmetic-shift (bitwise-and b1 #x1F) 6)
            (bitwise-and b2 #x3F))))
        ((= len 3)
         (let ((b1 (bytevector-u8-ref byte-seq 0))
               (b2 (bytevector-u8-ref byte-seq 1))
               (b3 (bytevector-u8-ref byte-seq 2)))
           (bitwise-ior
            (arithmetic-shift (bitwise-and b1 #x0F) 12)
            (arithmetic-shift (bitwise-and b2 #x3F) 6)
            (bitwise-and b3 #x3F))))
        ((= len 4)
         (let ((b1 (bytevector-u8-ref byte-seq 0))
               (b2 (bytevector-u8-ref byte-seq 1))
               (b3 (bytevector-u8-ref byte-seq 2))
               (b4 (bytevector-u8-ref byte-seq 3)))
           (bitwise-ior
           (arithmetic-shift (bitwise-and b1 #x07) 18)
           (arithmetic-shift (bitwise-and b2 #x3F) 12)
           (arithmetic-shift (bitwise-and b3 #x3F) 6)
           (bitwise-and b4 #x3F))))
        (else
         (value-error "Invalid UTF-8 byte sequence length")))))

  (rich-char (utf8-byte-sequence->code-point x)))

(define (%to-string)
  (if (%ascii?)
      (object->string (integer->char code-point))
      (string-append "#\\" (utf8->string (%to-bytevector)))))

(define (@from-string x)
  (when (not (string-starts? x "#\\"))
    (value-error "char@from-string: the input must start with #\\"))
  (if (= 1 ($ x :drop 2 :length))
      (rich-char :from-bytevector (string->utf8 ($ x :drop 2 :get)))
      (value-error "rich-char: must be u8 string which length equals 1")))

(define (%make-string)
  (utf8->string (%to-bytevector)))

(define (@from-integer x . args)
  (chain-apply args
    (rich-char x)))

(define (%to-integer)
  code-point)

)

(define-case-class rich-string
  ((data string?))
  
(define N (u8-string-length data))

(define (@empty . args)
  (chain-apply args (rich-string "")))

(define (@value-of v . args)
  (chain-apply args
    (cond ((char? v) (rich-string (string v)))
          ((number? v) (rich-string (number->string v)))
          ((symbol? v) (rich-string (symbol->string v)))
          ((string? v) (rich-string v))
          ((rich-char :is-type-of v)
           (rich-string (v :make-string)))
          (else (type-error "Expected types are char, rich-char, number, symbol or string")))))

(define (%get) data)

(define (%length)
  N)

(define (%char-at index)
  (when (not (integer? index))
    (type-error "rich-string%char-at: index must be integer" index))

  (let* ((start index)
         (end (+ index 1))
         (byte-seq (string->utf8 data start end)))
    (rich-char :from-bytevector byte-seq)))

(define (%apply i)
  (%char-at i))

(define (%find pred) ((%to-rich-vector) :find pred))

(define (%find-last pred) ((%to-rich-vector) :find-last pred))

(define (%head)
  (if (string-null? data)
      (index-error "rich-string%head: string is empty")
      ($ data 0)))

(define (%head-option)
  (if (string-null? data)
      (none)
      (option ($ data 0))))

(define (%last)
  (if (string-null? data)
      (index-error "rich-string%last: string is empty")
      ($ data (- N 1))))

(define (%last-option)
  (if (string-null? data)
      (none)
      (option ($ data (- N 1)))))

(define (%slice from until . args)
  (chain-apply args
    (let* ((start (max 0 from))
           (end (min N until)))
      (cond ((and (zero? start) (= end N))
             (%this))
            ((>= start end)
             (rich-string :empty))
            (else
             (rich-string (u8-substring data start end)))))))

(define (%take n . args)
  (chain-apply args
    (%slice 0 n)))

(define (%take-right n . args)
  (chain-apply args
    (%slice (- N n) N)))

(define (%drop n . args)
  (chain-apply args
    (%slice n N)))

(define (%drop-right n . args)
  (chain-apply args
    (%slice 0 (- N n))))

(define (%empty?)
  (string-null? data))

(define (%starts-with prefix)
  (string-starts? data prefix))

(define (%ends-with suffix)
  (string-ends? data suffix))

(define (%forall pred)
  ((%to-rich-vector) :forall pred))

(define (%exists pred)
  ((%to-rich-vector) :exists pred))

(define (%contains elem)
  (cond ((rich-string :is-type-of elem)
         (string-contains data (elem :get)))
    
        ((string? elem)
         (string-contains data elem))
        
        ((rich-char :is-type-of elem)
         (string-contains data (elem :make-string)))
        
        ((char? elem)
         (string-contains data (string elem)))
        
        (else (type-error "elem must be char or string"))))

(define* (%index-of str/char (start-index 0))
  (define (bytes-match? data-bv data-pos str-bv str-size data-size)
    (let loop ((i 0))
      (if (>= i str-size)
          #t
          (and (< (+ data-pos i) data-size)
               (= (bytevector-u8-ref data-bv (+ data-pos i))
                  (bytevector-u8-ref str-bv i))
               (loop (+ i 1))))))

  (define (char-index->byte-pos bv size char-index)
    (let loop ((i 0) (pos 0))
      (if (>= i char-index)
          pos
          (loop (+ i 1) (bytevector-advance-u8 bv pos size)))))
  
  (define* (inner-index-of str start-index)
    (if (or (string-null? data) (string-null? str))
        -1
        (let* ((data-bv (string->utf8 data))
               (str-bv (string->utf8 str))
               (data-size (bytevector-length data-bv))
               (str-size (bytevector-length str-bv)))
          (if (or (negative? start-index)
                  (>= start-index N))
              -1
              (let ((start-byte-pos (char-index->byte-pos data-bv data-size start-index)))
                (let search ((byte-pos start-byte-pos) (current-char-index start-index))
                  (cond
                    ((> (+ byte-pos str-size) data-size) -1)
                    ((bytes-match? data-bv byte-pos str-bv str-size data-size)
                     current-char-index)
                    (else
                     (search (bytevector-advance-u8 data-bv byte-pos data-size)
                             (+ current-char-index 1))))))))))

  (unless (integer? start-index)
    (type-error "rich-string%index-of: the second parameter must be integer"))
  
  (let ((positive-start-index (max 0 start-index)))
    (cond ((string? str/char)
           (inner-index-of str/char positive-start-index))
          ((rich-string :is-type-of str/char)
           (inner-index-of (str/char :get) positive-start-index))
          ((char? str/char)
           (inner-index-of (string str/char) positive-start-index))
          ((rich-char :is-type-of str/char)
           (inner-index-of (str/char :make-string) positive-start-index))
          (else (type-error "rich-string%index-of: first parameter must be string/rich-string/char/rich-char")))))

(define (%map f . args)
  (chain-apply args
    (rich-string ((%to-rich-vector)
                  :map f
                  :map (lambda (x) (x :make-string))
                  :make-string))))

(define (%filter pred . args)
  (chain-apply args
    (rich-string ((%to-rich-vector)
                  :filter pred
                  :map (lambda (x) (x :make-string))
                  :make-string))))

(define (%reverse . args)
  (chain-apply args
    (rich-string ((%to-rich-vector)
                  :reverse
                  :map (lambda (x) (x :make-string))
                  :make-string))))

(define (%for-each f)
  ((%to-rich-vector) :for-each f))

(define (%count pred?)
  ((%to-rich-vector) :count pred?))

(define (%index-where pred)
  (let ((bytes (string->utf8 data))
        (len (bytevector-length (string->utf8 data))))
    (let loop ((byte-pos 0) (char-index 0))
      (cond
        ((>= byte-pos len) -1)
        (else
         (let* ((next-pos (bytevector-advance-u8 bytes byte-pos len))
                (char-bytes (bytevector-copy bytes byte-pos next-pos))
                (char (rich-char :from-bytevector char-bytes)))
           (if (pred char)
               char-index
               (loop next-pos (+ char-index 1)))))))))

(define (%take-while pred . args)
  (chain-apply args
    (let ((stop-index (%index-where (lambda (c) (not (pred c))))))
      (if (= stop-index -1)
          (%this)
          (%slice 0 stop-index)))))

(define (%drop-while pred . args)
  (chain-apply args
    (let ((index (%index-where (lambda (c) (not (pred c))))))
      (if (= index -1)
          (rich-string "")
          (%slice index N)))))

(define (%to-string)
  data)

(define (%to-vector)
  (if (string-null? data)
      (vector)
      (let* ((bv (string->utf8 data))
             (bv-size (length bv))
             (result (make-vector N)))
        (let loop ((i 0) (j 0))
          (if (>= i N)
              result
              (let* ((next-j (bytevector-advance-u8 bv j bv-size))
                     (ch (rich-char :from-bytevector (bytevector-copy bv j next-j))))
                (vector-set! result i ch)
                (loop (+ i 1) next-j)))))))

(define (%to-rich-vector)
  (rich-vector (%to-vector)))

(define (%+ s . args)
  (chain-apply args
    (cond
      ((string? s)
       (rich-string (string-append data s)))
      ((rich-string :is-type-of s)
       (rich-string (string-append data (s :get))))
      ((number? s)
       (rich-string (string-append data (number->string s))))
      (else
        (type-error (string-append (object->string s) "is not string or rich-string or number"))))))

(define (%strip-left . args)
  (chain-apply args
    (rich-string (string-trim data))))

(define (%strip-right . args)
  (chain-apply args
    (rich-string (string-trim-right data))))

(define (%strip-both . args)
  (chain-apply args
    (rich-string (string-trim-both data))))

(define (%strip-prefix prefix . args)
  (chain-apply args
    (rich-string (string-remove-prefix data prefix))))

(define (%strip-suffix suffix . args)
  (chain-apply args
    (rich-string (string-remove-suffix data suffix))))

(define (%replace-first old new . args)
  (chain-apply args
    (let ((next-pos (%index-of old)))
      (if (= next-pos -1)
          (%this)
          ((%slice 0 next-pos)
           :+ new
           :+ (%drop (+ next-pos ($ old :length))))))))

(define (%replace old new . args)
  (define (replace-helper str old new start)
    (let ((next-pos ((rich-string str) :index-of old start)))
      (if (= next-pos -1)
          str
          (replace-helper ((rich-string str) :replace-first old new :get)
                          old new next-pos))))
  (chain-apply args
    (rich-string (replace-helper data old new 0))))

(define* (%pad-left len (char #\space) . args)
  (let ((result (rich-string (string-pad data len char))))
    (if (null? args)
        result
        (apply result args))))

(define* (%pad-right len (char #\space) . args)
  (let ((result (rich-string (string-pad-right data len char))))
    (if (null? args)
        result
        (apply result args))))

(define (%split sep)
  (let ((str-len ($ data :length))
        (sep-len ($ sep :length)))
    
    (define (split-helper start acc)
      (let ((next-pos (%index-of sep start)))
        (if (= next-pos -1)
            (cons (%drop start :get) acc)
            (split-helper (+ next-pos sep-len) (cons (%slice start next-pos :get) acc)))))
    
    (if (zero? sep-len)
        ((%to-rich-vector) :map (lambda (c) (c :make-string)))
        (rich-vector (reverse-list->vector (split-helper 0 '()))))))

)

(define-case-class option ((value any?))

(define (%get)
  (if (null? value)
      (value-error "option is empty, cannot get value")
      value))

(define (%get-or-else default)
  (cond ((not (null? value)) value)
        ((and (procedure? default) (not (case-class? default)))
         (default))
        (else default)))

(define (%or-else default . args)
  (when (not (option :is-type-of default))
    (type-error "The first parameter of option%or-else must be a option case class"))
  
  (chain-apply args
    (if (null? value)
        default
        (option value))))

(define (%equals that)
  (== value (that 'value)))

(define (%defined?) (not (null? value)))
  
(define (%empty?)
  (null? value))

(define (%forall f)
  (if (null? value)
      #f
      (f value)))

(define (%exists f)
  (if (null? value)
      #f
      (f value)))

(define (%contains pred?)
  (if (null? value)
      #f
      (pred? value)))

(define (%for-each f)
  (when (not (null? value))
        (f value)))

(define (%map f . args)
  (chain-apply args
    (if (null? value)
        (option '())
        (option (f value)))))

(define (%flat-map f . args)
  (chain-apply args
    (if (null? value)
        (option '())
        (f value))))

(define (%filter pred . args)
  (chain-apply args
    (if (or (null? value) (not (pred value)))
        (option '())
        (option value))))

)

(define (none) (option '()))

(define-case-class either
  ((type symbol?)
   (value any?))

(define (%left?)
  (eq? type 'left))

(define (%right?)
  (eq? type 'right))

(define (%get)
  value)

(define (%or-else default)
  (unless (case-class? default) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %or-else '(default) 'default "case-class" (object->string default))))  
  
  (when (not (default :is-instance-of 'either))
    (type-error "The first parameter of either%or-else must be a either case class"))

  (if (%right?)
      (%this)
      default))

(define (%get-or-else default)
  (cond ((%right?) value)
        ((and (procedure? default) (not (case-class? default)))
         (default))
        (else default)))

(define (%filter-or-else pred zero)
  (unless (procedure? pred) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %filter-or-else '(pred zero) 'pred "procedure" (object->string pred))))
  
  (unless (any? zero) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %filter-or-else '(pred zero) 'zero "any" (object->string zero))))  
  (if (%right?)
      (if (pred value)
          (%this)
          (left zero))
      (%this)))

(define (%contains x)
  (and (%right?)
       (== x value)))

(define (%for-each f)
  (when (%right?)
    (f value)))

(define (%to-option)
  (if (%right?)
      (option value)
      (none)))

(define (%map f . args)
  (chain-apply args
    (if (%right?)
      (right (f value))
      (%this))))

(define (%flat-map f . args)
  (chain-apply args
    (if (%right?)
      (f value)
      (%this))))

(define (%forall pred)
  (unless (procedure? pred) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %forall '(pred) 'pred "procedure" (object->string pred))))
  (if (%right?)
      (pred value)
      #t))

(define (%exists pred)
  (unless (procedure? pred) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %exists '(pred) 'pred "procedure" (object->string pred))))
  (if (%right?)
      (pred value)
      #f))

)

(define (left v)
  (either 'left v))

(define (right v)
  (either 'right v))

(define-case-class rich-list ((data list?))

(define (@range start end . step-and-args)
  (chain-apply (if (null? step-and-args) 
                   step-and-args 
                   (if (number? (car step-and-args))
                       (cdr step-and-args)
                       step-and-args))
    (let ((step-size 
            (if (null? step-and-args) 
                1
                (if (number? (car step-and-args))
                    (car step-and-args)
                    1))))
      (cond
        ((and (positive? step-size) (>= start end))
         (rich-list '()))
        ((and (negative? step-size) (<= start end))
         (rich-list '()))
        ((zero? step-size)
         (value-error "Step size cannot be zero"))
        (else
         (let ((cnt (ceiling (/ (- end start) step-size))))
           (rich-list (iota cnt start step-size))))))))

(define (@empty . args)
  (chain-apply args
    (rich-list (list ))))

(define (@concat lst1 lst2 . args)
  (chain-apply args
    (rich-list (append (lst1 :collect) (lst2 :collect)))))

(define (@fill n elem)
  (cond
    ((< n 0)
      (value-error "n cannot be negative"))
    ((= n 0)
      (rich-list '()))
    (else
      (rich-list (make-list n elem)))))

(define (%collect) data)

(define (%apply n)
  (list-ref data n))

(define (%find pred)
  (let loop ((lst data))
    (cond
      ((null? lst) (none))
      ((pred (car lst)) (option (car lst)))
      (else (loop (cdr lst))))))

(define (%find-last pred)
  (let ((reversed-list (reverse data)))  ; 先反转列表
    (let loop ((lst reversed-list))
      (cond
        ((null? lst) (none))  ; 遍历完未找到
        ((pred (car lst)) (option (car lst)))  ; 找到第一个匹配项（即原列表最后一个）
        (else (loop (cdr lst)))))))  ; 继续查找

(define (%head)
  (if (null? data)
      (error 'out-of-range "rich-list%head: list is empty")
      (car data)))

(define (%head-option)
  (if (null? data)
      (none)
      (option (car data))))


(define (%last)
  (if (null? data)
      (index-error "rich-list%last: empty list")
      (car (reverse data))))

(define (%last-option)
  (if (null? data)
      (none)
      (option (car (reverse data)))))

(define (%slice from until . args)
  (chain-apply args
    (let* ((len (length data))
          (start (max 0 (min from len)))
          (end (max 0 (min until len))))
      (if (< start end)
          (rich-list (take (drop data start) (- end start)))
          (rich-list '())))))

(define (%empty?)
  (null? data))

(define (%equals that)
  (let* ((l1 data)
         (l2 (that 'data))
         (len1 (length l1))
         (len2 (length l2)))
    (if (not (eq? len1 len2))
        #f
        (let loop ((left l1) (right l2))
          (cond ((null? left) #t)
                ((!= (car left) (car right)) #f)
                (else (loop (cdr left) (cdr right))))))))

(define (%forall pred)
  (every pred data))

(define (%exists pred)
  (any pred data))

(define (%contains elem)
  (%exists (lambda (x) (equal? x elem))))

(define (%map x . args)
  (chain-apply args
    (rich-list (map x data))))

(define (%flat-map x . args)
  (chain-apply args
    (rich-list (flat-map x data))))

(define (%filter x . args)
  (chain-apply args
    (rich-list (filter x data))))

(define (%for-each x)
  (for-each x data))

(define (%reverse . args)
  (chain-apply args
    (rich-list (reverse data))))
    
(define (%take x . args)
  (chain-apply args
    (begin 
      (define (scala-take data n)
        (unless (list? data) 
            (type-error 
               (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                              scala-take '(data n) 'data "list" (object->string data))))
        (unless (integer? n) 
            (type-error 
               (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                              scala-take '(data n) 'n "integer" (object->string n))))
      
        (cond ((< n 0) '())
              ((>= n (length data)) data)
              (else (take data n))))
    
      (rich-list (scala-take data x)))))

(define (%drop x . args)
  (chain-apply args
    (begin 
      (define (scala-drop data n)
        (unless (list? data) 
            (type-error 
               (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                              scala-drop '(data n) 'data "list" (object->string data))))
        (unless (integer? n) 
            (type-error 
               (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                              scala-drop '(data n) 'n "integer" (object->string n))))
      
        (cond ((< n 0) data)
              ((>= n (length data)) '())
              (else (drop data n))))
    
      (rich-list (scala-drop data x)))))

(define (%take-right x . args)
  (chain-apply args
    (begin 
      (define (scala-take-right data n)
        (unless (list? data) 
            (type-error 
               (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                              scala-take-right '(data n) 'data "list" (object->string data))))
        (unless (integer? n) 
            (type-error 
               (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                              scala-take-right '(data n) 'n "integer" (object->string n))))
      
        (cond ((< n 0) '())
              ((>= n (length data)) data)
              (else (take-right data n))))
    
      (rich-list (scala-take-right data x)))))

(define (%drop-right x . args)
  (chain-apply args
    (begin 
      (define (scala-drop-right data n)
        (unless (list? data) 
            (type-error 
               (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                              scala-drop-right '(data n) 'data "list" (object->string data))))
        (unless (integer? n) 
            (type-error 
               (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                              scala-drop-right '(data n) 'n "integer" (object->string n))))
      
        (cond ((< n 0) data)
              ((>= n (length data)) '())
              (else (drop-right data n))))
    
      (rich-list (scala-drop-right data x)))))

(define (%count . xs)
  (cond ((null? xs) (length data))
        ((length=? 1 xs) (count (car xs) data))
        (else (error 'wrong-number-of-args "rich-list%count" xs))))

(define (%length)
  (length data))

(define (%fold initial f)
  (fold f initial data))

(define (%fold-right initial f)
  (fold-right f initial data))

(define (%sort-with less-p . args)
  (chain-apply args
    (let ((sorted-data (list-stable-sort less-p data)))
        (rich-list sorted-data))))

(define (%sort-by f . args)
  (chain-apply args
    (let ((sorted-data (list-stable-sort (lambda (x y) (< (f x) (f y))) data)))
    (rich-list sorted-data))))

(define (%group-by func)
  (let ((group (make-hash-table)))
    (for-each
      (lambda (elem) 
        (let ((key (func elem)))
          (hash-table-update!/default
            group
            key
            (lambda (current-list) (cons elem current-list))
            '())))
      data)
    (hash-table-for-each 
      (lambda (k v) (hash-table-set! group k (reverse v))) 
      group)
  (rich-hash-table group)))

(define (%sliding size . step-arg)
  (unless (integer? size) (type-error "rich-list%sliding: size must be an integer " size))
  (unless (> size 0) (value-error "rich-list%sliding: size must be a positive integer " size))

  (let ((N (length data)))
    (if (null? data)
        #()
        (let* ((is-single-arg-case (null? step-arg))
               (step (if is-single-arg-case 1 (car step-arg))))

          (when (and (not is-single-arg-case)
                     (or (not (integer? step)) (<= step 0)))
            (if (not (integer? step))
                (type-error "rich-list%sliding: step must be an integer " step)
                (value-error "rich-list%sliding: step must be a positive integer " step)))
          
          (if (and is-single-arg-case (< N size))
              (vector data)
              (let collect-windows ((current-list-segment data) (result-windows '()))
                (cond
                  ((null? current-list-segment) (list->vector (reverse result-windows)))
                  ((and is-single-arg-case (< (length current-list-segment) size))
                   (list->vector (reverse result-windows)))
                  (else
                   (let* ((elements-to-take (if is-single-arg-case
                                                size
                                                (min size (length current-list-segment))))
                          (current-window (take current-list-segment elements-to-take))
                          (next-list-segment (if (>= step (length current-list-segment))
                                                 '()
                                                 (drop current-list-segment step))))
                     (collect-windows next-list-segment
                                      (cons current-window result-windows)))))))))))

(define (%zip l . args)
  (chain-apply args
    (rich-list (apply map cons (list data l)))))

(define (%zip-with-index . args)
  (chain-apply args
    (let loop ((lst data) (idx 0) (result '()))
      (if (null? lst)
          (rich-list (reverse result))  
          (loop (cdr lst) 
                (+ idx 1) 
                (cons (cons idx (car lst)) result))))))

(define (%distinct . args)
  (chain-apply args
    (let loop
      ((result '()) 
      (data data) 
      (ht (make-hash-table)))
      (cond
        ((null? data) (rich-list (reverse result)))  
        (else
         (let ((elem (car data)))
           (if (eq? (hash-table-ref ht elem) #f) 
               (begin
                 (hash-table-set! ht elem #t)  
                 (loop (cons elem result) (cdr data) ht))
               (loop result (cdr data) ht))))))))

(define (%reduce f)
  (if (null? data)
      (value-error "rich-list%reduce: empty list is not allowed to reduce")
      (reduce f '() data)))

(define (%reduce-option f)
  (if (null? data)
      (none)
      (option (reduce f '() data))))

(define (%take-while pred . args)
  (chain-apply args
    (let ((result (take-while pred data)))
      (rich-list result))))

(define (%drop-while pred . args)
  (chain-apply args
    (let ((result (drop-while pred data)))
      (rich-list result))))

(define (%index-where pred)
  (list-index pred data))

(define (%max-by f)
  (unless (procedure? f) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %max-by '(f) 'f "procedure" (object->string f))))              
  (if (null? data)
      (value-error "rich-list%max-by: empty list is not allowed")
      (let loop ((rest (cdr data))
                 (max-elem (car data))
                 (max-val (let ((val (f (car data))))
                           (unless (real? val)
                             (type-error "rich-list%max-by: procedure must return real number but got"
                                        (object->string val)))
                           val)))
        (if (null? rest)
            max-elem
            (let* ((current (car rest))
                   (current-val (let ((val (f current)))
                                 (unless (real? val)
                                   (type-error "rich-list%max-by: procedure must return real number but got"
                                              (object->string val)))
                                 val)))
              (if (> current-val max-val)
                  (loop (cdr rest) current current-val)
                  (loop (cdr rest) max-elem max-val)))))))

(define (%min-by f)
  (unless (procedure? f) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %min-by '(f) 'f "procedure" (object->string f))))              
  (if (null? data)
      (value-error "rich-list%min-by: empty list is not allowed")
      (let loop ((rest (cdr data))
                 (min-elem (car data))
                 (min-val (let ((val (f (car data))))
                            (unless (real? val)
                              (type-error "rich-list%min-by: procedure must return real number but got"
                                         (object->string val)))
                            val)))
        (if (null? rest)
            min-elem
            (let* ((current (car rest))
                   (current-val (let ((val (f current)))
                                  (unless (real? val)
                                    (type-error "rich-list%min-by: procedure must return real number but got"
                                               (object->string val)))
                                  val)))
              (if (< current-val min-val)
                  (loop (cdr rest) current current-val)
                  (loop (cdr rest) min-elem min-val)))))))

(define (%append l)
  (rich-list (append data l)))

(define (%max-by-option f)
  (if (null? data)
      (none)
      (option (%max-by f))))

(define (%min-by-option f)
  (if (null? data)
      (none)
      (option (%min-by f))))

(define (%to-string)
  (object->string data))

(define (%make-string . xs)
  (define (parse-args xs)
    (cond
      ((null? xs) (values "" "" ""))
      ((length=? 1 xs)
       (let ((sep (car xs)))
         (if (string? sep)
             (values "" sep "")
             (type-error "rich-list%make-string: separator must be a string" sep))))
      ((length=? 2 xs)
       (error 'wrong-number-of-args "rich-list%make-string: expected 0, 1, or 3 arguments, but got 2" xs))
      ((length=? 3 xs)
       (let ((start (car xs))
             (sep (cadr xs))
             (end (caddr xs)))
         (if (and (string? start) (string? sep) (string? end))
             (values start sep end)
             (error 'type-error "rich-list%make-string: prefix, separator, and suffix must be strings" xs))))
      (else (error 'wrong-number-of-args "rich-list%make-string: expected 0, 1, or 3 arguments" xs))))

  (receive (start sep end) (parse-args xs)
    (let ((as-string (lambda (x) (if (string? x) x (object->string x)))))
      (string-append start (string-join (map as-string data) sep) end))))

(define (%to-vector)
  (list->vector data))

(define (%to-rich-vector)
  (rich-vector (list->vector data)))

)

(define-case-class rich-vector ((data vector?))

(define (@range start end . step)
  (let ((step-size (if (null? step) 1 (car step))))
    (cond
      ((and (positive? step-size) (>= start end))
       (rich-vector #()))
      ((and (negative? step-size) (<= start end))
       (rich-vector #()))
      ((zero? step-size)
       (value-error "Step size cannot be zero"))
      (else
       (let ((cnt (ceiling (/ (- end start) step-size))))
         (rich-vector (list->vector (iota cnt start step-size))))))))

(define (@empty . args)
  (chain-apply args
    (rich-vector #())))

(define (@fill n elem . args)
  (unless (integer? n)
    (type-error "n must be integer" n))
  (when (< n 0)
    (value-error "n must be non-negative" n))
  (chain-apply args
    (rich-vector (make-vector n elem))))

(define (%collect) data)

(define (%length)
  (vector-length data))

(define (%size)
  (vector-length data))

(define (%apply i)
  (when (or (< i 0) (>= i (vector-length data)))
    (index-error "rich-vector%apply: out of range with index" i))
  (vector-ref data i))

(define (%index-of x)
  (or (vector-index (lambda (y) (== x y)) data)
      -1))

(define (%last-index-of x)
  (or (vector-index-right (lambda (y) (== x y)) data)
      -1))

(define (%find p)
  (let loop ((i 0))
    (cond
     ((>= i (vector-length data)) (none))
     ((p (vector-ref data i)) (option (vector-ref data i)))
     (else (loop (+ i 1))))))

(define (%find-last pred)
  (let loop ((i (- (vector-length data) 1)))
    (cond
      ((< i 0) (none))  ; 遍历完所有元素未找到
      ((pred (vector-ref data i)) (option (vector-ref data i)))  ; 找到符合条件的元素
      (else (loop (- i 1))))))  ; 继续向前查找

(define (%head)
  (if (> (vector-length data) 0)
      (vector-ref data 0)
      (error 'out-of-range "out-of-range")))

(define (%head-option)
  (if (> (vector-length data) 0)
      (option (vector-ref data 0))
      (none)))

(define (%last)
  (let ((len (vector-length data)))
    (if (> len 0)
      (vector-ref data (- len 1))
      (index-error "rich-vector%last: empty vector"))))

(define (%last-option)
  (let ((len (vector-length data)))
    (if (> len 0)
      (option (vector-ref data (- len 1)))
      (none))))

(define (%slice from until . args)
  (chain-apply args
    (let* ((len (vector-length data))
           (start (max 0 from))
           (end (min len until)))
      (if (< start end)
          (rich-vector (vector-copy data start end))
          (rich-vector :empty)))))

(define (%empty?)
  (= (length data) 0))

(define (%equals that)
  (and (that :is-instance-of 'rich-vector)
       (vector= == data (that 'data))))

(define (%forall p)
  (vector-every p data))

(define (%exists p)
  (vector-any p data))

(define (%contains elem)
  (%exists (lambda (x) (equal? x elem))))

(define (%map x . args)
  (chain-apply args
    (rich-vector (vector-map x data))))

(define (%flat-map f . args)
  (chain-apply args
    (rich-vector (%map f :reduce vector-append))))

(define (%filter x . args)
  (chain-apply args
    (rich-vector (vector-filter x data))))

(define (%for-each x)
  (vector-for-each x data))

(define (%reverse . args)
  (chain-apply args
    (rich-vector (reverse data))))

(define (%count . xs)
  (cond ((null? xs) (vector-length data))
        ((length=? 1 xs) (vector-count (car xs) data))
        (else (error 'wrong-number-of-args "rich-vector%count" xs))))

(define (%take n . args)
  (define (scala-take data n)
    (cond
      ((< n 0) (vector))
      ((>= n (vector-length data)) data)
      (else
        (let ((new-vec (make-vector n)))
          (do ((i 0 (+ i 1)))
              ((>= i n) new-vec)
            (vector-set! new-vec i (vector-ref data i)))))))
  
  (chain-apply args
    (rich-vector (scala-take data n))))

(define (%take-right n . args)
  (define (scala-take-right data n)
    (let ((len (vector-length data)))
      (cond
        ((< n 0) (vector))
        ((>= n len) data)
        (else
          (let ((new-vec (make-vector n)))
            (do ((i (- len n) (+ i 1))
                 (j 0 (+ j 1)))
                ((>= j n) new-vec)
              (vector-set! new-vec j (vector-ref data i))))))))

  (chain-apply args
    (rich-vector (scala-take-right data n))))

(define (%drop n . args)
  (define (scala-drop data n)
    (cond
      ((< n 0) data)
      ((>= n (vector-length data)) (vector))
      (else (vector-copy data n))))
  (chain-apply args
    (rich-vector (scala-drop data n))))

(define (%drop-right n . args)
  (define (scala-drop-right data n)
    (cond
      ((< n 0) data)
      ((>= n (vector-length data)) (vector))
      (else (vector-copy data 0 (- (vector-length data) n)))))
  
  (chain-apply args
    (rich-vector (scala-drop-right data n))))

(define (%drop-while pred . args)
  (chain-apply args
    (let ((len (vector-length data)))
      (let loop ((i 0))
        (cond
          ((>= i len) (rich-vector :empty))  ; 所有元素都被丢弃
          ((pred (vector-ref data i)) (loop (+ i 1)))  ; 继续丢弃
          (else (rich-vector (vector-copy data i)))))))) ; 返回剩余部分

(define (%fold initial f)
  (vector-fold f initial data))

(define (%fold-right initial f)
  (vector-fold-right f initial data))

(define (%count . xs)
  (cond ((null? xs) (vector-length data))
        ((length=? 1 xs) (count (car xs) (vector->list data)))
        (else (error 'wrong-number-of-args "rich-vector%count" xs))))

(define (%sort-with less-p . args)
  (chain-apply args
    (rich-vector (vector-stable-sort less-p data))))

(define (%sort-by f . args)
  (chain-apply args
    (let ((sorted-data (vector-stable-sort (lambda (x y) (< (f x) (f y))) data)))
      (rich-vector sorted-data))))

(define (%group-by func)
  (let ((group (make-hash-table)))
    (for-each
      (lambda (elem) 
        (let ((key (func elem)))
          (hash-table-update!/default
            group
            key
            (lambda (current-list) (cons elem current-list))
            '())))
      (vector->list data))
    (hash-table-for-each 
      (lambda (k v) (hash-table-set! group k (reverse-list->vector v))) 
      group)
    (rich-hash-table group)))

(define (%sliding size . step-arg)
  (unless (integer? size) (type-error "rich-vector%sliding: size must be an integer " size))
  (unless (> size 0) (value-error "rich-vector%sliding: size must be a positive integer " size))

  (let ((N (vector-length data)))
    (if (zero? N)
        #()
        (let* ((is-single-arg-case (null? step-arg))
               (step (if is-single-arg-case 1 (car step-arg))))

          ;; Validate step if provided
          (when (and (not is-single-arg-case)
                     (or (not (integer? step)) (<= step 0)))
            (if (not (integer? step))
                (type-error "rich-vector%sliding: step must be an integer " step)
                (value-error "rich-vector%sliding: step must be a positive integer " step)))
          
          ;; single-argument version when N < size
          (if (and is-single-arg-case (< N size))
              (vector data)
              (let collect-windows ((current-idx 0) (result-windows '()))
                (cond
                  ;; Stop if current_idx is out of bounds
                  ((>= current-idx N) (list->vector (reverse result-windows)))
                  ;; For single-arg case
                  ((and is-single-arg-case (> (+ current-idx size) N))
                   (list->vector (reverse result-windows)))
                  (else
                   (let* ((window-end (if is-single-arg-case
                                          (+ current-idx size)      ;; Single-arg: always takes full 'size'
                                          (min (+ current-idx size) N))) ;; Two-arg: can be partial
                          (current-window (vector-copy data current-idx window-end)))
                     (collect-windows (+ current-idx step) (cons current-window result-windows)))))))))))

(define (%zip-with-index . args)
  (chain-apply args
    (let* ((n (vector-length data))
           (result (make-vector n)))
      (let loop ((idx 0))
        (if (>= idx n)
            (rich-vector result)
            (begin
              (vector-set! 
                  result 
                  idx 
                  (cons idx (vector-ref data idx)))
              (loop (+ idx 1))))))))

(define (%distinct . args)
  (chain-apply args
    (let ((ht (make-hash-table))
          (length (vector-length data)))
      (let loop ((result '())
                (index 0))
        (if (>= index length)
            (rich-vector (list->vector (reverse result)))
            (let ((elem (vector-ref data index)))
              (if (eq? (hash-table-ref ht elem) #f)
                  (begin
                    (hash-table-set! ht elem #t)
                    (loop (cons elem result) (+ index 1)))
                   (loop result (+ index 1)))))))))

(define (%reduce f)
  (let ((len (vector-length data)))
    (if (zero? len)
        (value-error "rich-vector%reduce: empty vector is not allowed to reduce")
        (let loop ((acc (vector-ref data 0))
                   (i 1))
          (if (>= i len)
              acc
              (loop (f acc (vector-ref data i)) (+ i 1)))))))

(define (%index-where pred)
  (or (vector-index pred data)
      -1))

(define (%last-index-where pred)
  (or (vector-index-right pred data)
      -1))

(define (%take-while pred . args)
  (chain-apply args
    (let* ((vec data)
           (len (vector-length vec))
           (idx (vector-index (lambda (x) (not (pred x))) vec)))
      (rich-vector (vector-copy vec 0 (or idx len))))))

(define (%max-by f)
  (when (not (procedure? f))
    (type-error "rich-vector%max-by: f must be a procedure"))
  
  (let ((vec data)
        (len (length data)))
    (if (zero? len)
        (value-error "rich-vector%max-by: empty list is not allowed")
        (let loop ((i 1)
                   (max-elem (vector-ref vec 0))
                   (max-val (f (vector-ref vec 0))))
          (if (>= i len)
              max-elem
              (let* ((current-elem (vector-ref vec i))
                     (current-val (f current-elem)))
                (unless (number? current-val)
                  (type-error "f must return a number"))
                (if (< current-val max-val)
                    (loop (+ i 1) max-elem max-val)
                    (loop (+ i 1) current-elem current-val))))))))

(define (%min-by f)
  (when (not (procedure? f))
    (type-error "rich-vector%min-by: f must be a procedure"))

  (let ((vec data)
        (len (length data)))
    (if (zero? len)
        (value-error "rich-vector%min-by: empty list is not allowed")
        (let loop ((i 1)
                   (min-elem (vector-ref vec 0))
                   (min-val (f (vector-ref vec 0))))
          (if (>= i len)
              min-elem
              (let* ((current-elem (vector-ref vec i))
                     (current-val (f current-elem)))
                (unless (number? current-val)
                  (type-error "f must return a number"))
                (if (> current-val min-val)
                    (loop (+ i 1) min-elem min-val)
                    (loop (+ i 1) current-elem current-val))))))))

(define (%max-by-option f)
  (when (not (procedure? f))
    (type-error "rich-vector%max-by-option: f must be a procedure"))

  (if (zero? (vector-length data))
      (none)
      (option (%max-by f))))

(define (%min-by-option f)
  (when (not (procedure? f))
    (type-error "rich-vector%min-by-option: f must be a procedure"))

  (if (zero? (vector-length data))
      (none)
      (option (%min-by f))))

(define (%to-string)
  ((%map object->string)
   :make-string "#(" " " ")"))

(define (%make-string . xs)
  (define (parse-args xs)
    (cond
      ((null? xs) (values "" "" ""))
      ((length=? 1 xs)
       (let ((sep (car xs)))
         (if (string? sep)
             (values "" sep "")
             (type-error "rich-vector%make-string: separator must be a string" sep))))
      ((length=? 2 xs)
       (error 'wrong-number-of-args "rich-vector%make-string: expected 0, 1, or 3 arguments, but got 2" xs))
      ((length=? 3 xs)
       (let ((start (car xs))
             (sep (cadr xs))
             (end (caddr xs)))
         (if (and (string? start) (string? sep) (string? end))
             (values start sep end)
             (type-error "rich-vector%make-string: prefix, separator, and suffix must be strings" xs))))
      (else (error 'wrong-number-of-args "rich-vector%make-string: expected 0, 1, or 3 arguments" xs))))

  (receive (start sep end) (parse-args xs)
    (let* ((as-string (lambda (x) (if (string? x) x (object->string x))))
           (middle (string-join (map as-string (vector->list data)) sep)))
      (string-append start middle end))))

(define (%to-list)
  (vector->list data))

(define (%to-rich-list)
  (rich-list (vector->list data)))

(define (%set! i x)
  (when (or (< i 0) (>= i (length data)))
    (index-error "rich-vector%set! out of range at index" i))
  (vector-set! data i x))

(define (%append v)
  (when (not (or (vector? v) (rich-vector :is-type-of v)))
    (type-error "rich-vector%append: input is not vector or rich-vector"))
  
  (if (vector? v)
      (rich-vector (vector-append data v))
      (rich-vector (vector-append data (v :collect)))))

)

(define array rich-vector)

(define-case-class rich-hash-table ((data hash-table?))
  (define (%collect) data)

(chained-define (@empty)
  (rich-hash-table (make-hash-table)))

(define (%find pred?)
  (define iter (make-iterator data))
  (let loop ((kv (iter)))
    (cond 
        ((eof-object? kv) (none))
        ((and (pair? kv) (pred? (car kv) (cdr kv))) (option kv))
        (else (loop (iter))))))

(define (%get k)
  (option (hash-table-ref/default data k '())))

(define (%remove k)
  (rich-hash-table
   (let ((new (make-hash-table)))
     (hash-table-for-each
      (lambda (key val)
       (unless (equal? key k)
        (hash-table-set! new key val)))
       data)
      new)))

(chained-define (%remove! k)
  (hash-table-delete! data k)
  %this)

(define (%contains k)
  (hash-table-contains? data k))

(define (%forall pred?)
  (let ((all-kv (map identity data)))
    (let loop ((kvs all-kv))  
      (if (null? kvs)
          #t  
          (let ((kv (car kvs)))
            (if (pred? (car kv) (cdr kv))
                (loop (cdr kvs))  
                #f))))))  

(define (%exists pred?)
  (define iter (make-iterator data))
  (let loop ((kv (iter)))
    (cond 
        ((eof-object? kv) #f)
        ((and (pair? kv) (pred? (car kv) (cdr kv))) #t)
        (else (loop (iter))))))

(define (%map f . args)
  (chain-apply args
    (let ((r (make-hash-table)))
      (hash-table-for-each
         (lambda (k v)
           (receive (k1 v1) (f k v)
             (hash-table-set! r k1 v1)))
         data)
      (rich-hash-table r))))

(define (%count pred)
  (hash-table-count pred data))

(define (%for-each proc)
  (hash-table-for-each proc data))

(define (%filter f . args)
  (chain-apply args
    (let ((r (make-hash-table)))
      (hash-table-for-each
         (lambda (k v)
           (when (f k v) (hash-table-set! r k v)))
         data)
      (rich-hash-table r))))

)

) ; end of begin
) ; end of library

