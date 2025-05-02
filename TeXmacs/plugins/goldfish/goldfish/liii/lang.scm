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
(import (liii base) (liii string) (liii vector) (liii sort)
        (liii list) (liii hash-table) (liii bitwise))
(export
  @ typed-define
  define-case-class case-class? == != chained-define display* object->string
  option none either left right
  rich-integer rich-float rich-char rich-string
  rich-list rich-vector array rich-hash-table
  box $
)
(begin

(define-macro (@ . paras)
  (letrec*
    (
      (slot? (lambda (x) (equal? '_ x)))
      (exprs (filter (lambda (x) (not (slot? x))) paras))
      (slots (filter slot? paras))

      (exprs-sym-list (map (lambda (x) (gensym)) exprs))  
      (slots-sym-list (map (lambda (x) (gensym)) slots))

      (lets (map list exprs-sym-list exprs))

      (parse
        (lambda (exprs-sym-list slots-sym-list paras)
          (cond
            ((null? paras) paras)
            ((not (list? paras)) paras)
            ((slot? (car paras)) 
              `(,(car slots-sym-list) 
                ,@(parse exprs-sym-list (cdr slots-sym-list) (cdr paras))))
            (else 
              `(,(car exprs-sym-list) 
                ,@(parse (cdr exprs-sym-list) slots-sym-list (cdr paras))))))))
                
  `(let ,lets 
        (lambda ,slots-sym-list 
                ,(parse exprs-sym-list slots-sym-list paras)))))

(define-macro (typed-define name-and-params body . rest)
  (let* ((name (car name-and-params))
          (params (cdr name-and-params))
          (param-names (map car params)))

        `(define* 
            (,name 
            ,@(map  
              (lambda (param)
                (let  ((param-name (car param))
                      (type-pred (cadr param))
                      (default-value (cddr param)))
                      (if (null? default-value)
                          param-name
                          `(,param-name ,(car default-value)))))
              params))

        ;; Runtime type check                    
        ,@(map (lambda (param)
                (let* ((param-name (car param))
                      (type-pred (cadr param))
                      ;;remove the '?' in 'type?'
                      (type-name-str 
                         (let ((s (symbol->string type-pred)))
                           (if (and (positive? (string-length s))
                                    (char=? (string-ref s (- (string-length s) 1)) #\?))
                               (substring s 0 (- (string-length s) 1))
                               s))))

                  `(unless 
                      (,type-pred ,param-name)
                      (type-error 
                          (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**"
                                ,name
                                ',param-names
                                ',param-name
                                ,type-name-str
                                (object->string ,param-name))))))
              params)
       ,body
       ,@rest)))

(define-macro (define-case-class class-name fields . private-fields-and-methods)
  (let* ((key-fields
         (map (lambda (field) (string->symbol (string-append ":" (symbol->string (car field)))))
              fields))
        
         (field-names (map car fields))

         (private-fields (filter (lambda (x)
                                   (and (list? x)
                                        (>= (length x) 2)
                                        (symbol? (x 1))))
                                 private-fields-and-methods))

         (methods (filter (lambda (x)
                            (and (list? x)
                                 (>= (length x) 2)
                                 (pair? (x 1))))
                          private-fields-and-methods))
         
         (method-names
           (map (lambda (method)
                  (let* ((method-sym (caadr method))
                         (method-name (symbol->string method-sym)))
                    (cond
                      ((string-starts? method-name "@")
                       (string-remove-prefix method-name "@"))
                      ((string-starts? method-name "%")
                       (string-remove-prefix method-name "%"))
                      (else method-name))))
                methods))
         
         (conflicts-names
          (filter (lambda (method-name)
                    (let ((name (string->symbol method-name)))
                      (member name field-names)))
                  method-names))
         
         (check-conflicts-names (unless (null? conflicts-names)
              (let ((conflict-str (apply string-append 
                                        (map (lambda (c) (string-append " <" c ">"))
                                             conflicts-names))))
                (error 'syntax-error (string-append "In class ["
                                          (symbol->string class-name)
                                          "]: Method name" 
                                          (if (= (length conflicts-names) 1) "" "s")
                                          conflict-str
                                          " conflicts with field name"
                                          (if (= (length conflicts-names) 1) "" "s"))))))
         
         (instance-methods
          (filter (lambda (method) (string-starts? (symbol->string (caadr method)) "%"))
                  methods))
         (instance-method-symbols (map caadr instance-methods))
         (instance-messages
          (map (lambda (method)
                 (let1 name (string-remove-prefix (symbol->string method) "%")
                   (string->symbol (string-append ":" name))))
               instance-method-symbols))
         (static-methods
          (filter (lambda (method) (string-starts? (symbol->string (caadr method)) "@"))
                  methods))
         (static-method-symbols (map caadr static-methods))
         (static-messages
          (map (lambda (method)
                 (let1 name (string-remove-prefix (symbol->string method) "@")
                   (string->symbol (string-append ":" name))))
               static-method-symbols))
         (internal-methods
           (filter (lambda (method) (not (or (string-starts? (symbol->string (caadr method)) "%")
                                             (string-starts? (symbol->string (caadr method)) "@"))))
                   methods))
         (this-symbol (gensym))
         (f-make-case-class (string->symbol (string-append "make-case-class-" (symbol->string class-name)))))

`(define (,class-name msg . args)

(define (@is-type-of obj)
  (and (case-class? obj)
       (obj :is-instance-of ',class-name)))
   
,@static-methods

(define (is-normal-function? msg)
  (and  (symbol? msg) 
        (char=? (string-ref (symbol->string msg) 0) #\:)))

(define (static-dispatcher msg . args)
    (cond
     ((eq? msg :is-type-of) (apply @is-type-of args))
     ,@(map (lambda (method expected) `((eq? msg ,expected) (apply ,method args)))
            static-method-symbols static-messages)
     (else (value-error "No such static method " msg))))

(typed-define (,f-make-case-class ,@fields)
  (define ,this-symbol #f)
  (define (%this . xs)
    (if (null? xs)
      ,this-symbol
      (apply ,this-symbol xs)))

  (define (%is-instance-of x)
    (eq? x ',class-name))
         
  (typed-define (%equals (that case-class?))
    (and (that :is-instance-of ',class-name)
         ,@(map (lambda (field) `(equal? ,(car field) (that ',(car field))))
                fields)))
         
  (define (%apply . args)
    (cond ((null? args)
           (value-error ,class-name "Apply on zero args is not implemented"))
          ((equal? ((symbol->string (car args)) 0) #\:)
           (value-error ,class-name "No such method: " (car args)))
          (else (value-error ,class-name "No such field: " (car args)))))
         
  (define (%to-string)
    (let ((field-strings
           (list ,@(map (lambda (field key-field)
                          `(string-append
                            ,(symbol->string key-field) " "
                            (object->string ,(car field))))
                        fields key-fields))))
      (let loop ((strings field-strings)
                 (acc ""))
        (if (null? strings)
            (string-append "(" ,(symbol->string class-name) " " acc ")")
            (loop (cdr strings)
                  (if (zero? (string-length acc))
                      (car strings)
                      (string-append acc " " (car strings))))))))

  ,@private-fields
  ,@internal-methods
  ,@instance-methods
 
  (define (instance-dispatcher)
    (lambda (msg . args)
      (cond
        ((eq? msg :is-instance-of) (apply %is-instance-of args))
        ((eq? msg :equals) (apply %equals args))
        ((eq? msg :to-string) (%to-string))
        ((eq? msg :this) (apply %this args))
        ,@(map (lambda (field key-field)
            `((eq? msg ,key-field)
              (,class-name
              ,@(map (lambda (f) (if (eq? (car f) (car field)) '(car args) (car f)))
                      fields))))
          fields key-fields)
        ((is-normal-function? msg)
          (cond
              ,@(map (lambda (method expected) `((eq? msg ,expected) (apply ,method args)))
               instance-method-symbols instance-messages)
              (else (value-error ,class-name "No such method: " msg))))
        
        ,@(map (lambda (field) `((eq? msg ',(car field)) ,(car field))) fields)
        (else (apply %apply (cons msg args))))))

  (set! ,this-symbol (instance-dispatcher))
  ,this-symbol
) ; end of the internal typed define

(if (in? msg (list ,@static-messages :is-type-of))
    (apply static-dispatcher (cons msg args))
    (apply ,f-make-case-class (cons msg args)))

) ; end of define
) ; end of let
) ; end of define-macro

(define (case-class? x)
  (and-let* ((is-proc? (procedure? x))
             (source (procedure-source x))
             (source-at-least-3? (and (list? source) (>= (length source) 3)))
             (body (source 2))
             (body-at-least-3? (and (list? body) (>= (length body) 3)))
             (is-cond? (eq? (car body) 'cond))
             (pred1 ((body 1) 0))
             (pred2 ((body 2) 0)))
    (and (equal? pred1 '(eq? msg :is-instance-of))
         (equal? pred2 '(eq? msg :equals)))))

(define (== left right)
  (cond
    ((and (case-class? left) (case-class? right))
     (left :equals right))
    ((case-class? left)
     (left :equals ($ right)))
    ((case-class? right)
     ($ left :equals right))
    (else
     (equal? left right))))

(define (!= left right)
  (not (== left right)))

(define-macro (chained-define head . body)
  (let ((xs (gensym))
        (result (gensym)))
    `(define ,(append head xs)
       (let ((,result (begin ,@body)))
         (if (null? ,xs)
           ,result
           (apply ,result ,xs))))))

(define (display* . params)
  (define (%display x)
    (if (case-class? x)
        (display (x :to-string))
        (display x)))
  (for-each %display params))

(define s7-object->string object->string)

(define (object->string x)
  (if (case-class? x)
      (x :to-string)
      (s7-object->string x)))

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

(typed-define (%to (n integer?))
  (if (< n data)
      (rich-list (list))
      (rich-list (iota (+ (- n data) 1) data))))

(typed-define (%until (n integer?))
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
  
(chained-define (%to-upper)
  (rich-char
    (if (and (>= code-point #x61) (<= code-point #x7A))
        (bitwise-and code-point #b11011111)
        code-point)))

(chained-define (%to-lower)
  (rich-char
    (if (and (>= code-point #x41) (<= code-point #x5A))
        (bitwise-ior code-point #b00100000)
        code-point)))

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

(chained-define (@from-integer x)
  (rich-char x))

(define (%to-integer)
  code-point)

)

(define-case-class rich-string
  ((data string?))
  
(define N (u8-string-length data))

(chained-define (@empty)
  (rich-string ""))

(chained-define (@value-of v) 
  (cond ((char? v) (rich-string (string v)))
        ((number? v) (rich-string (number->string v)))
        ((symbol? v) (rich-string (symbol->string v)))
        ((string? v) (rich-string v))
        ((rich-char :is-type-of v)
         (rich-string (v :make-string)))
        (else (type-error "Expected types are char, rich-char, number, symbol or string"))))

(define (%get) data)

(define (%length)
  N)

(define (%char-at index)
  (let* ((start index)
         (end (+ index 1))
         (byte-seq (string->utf8 data start end)))
    (rich-char :from-bytevector byte-seq)))

(typed-define (%apply (i integer?))
  (%char-at i))

(chained-define (%slice from until)
  (let* ((start (max 0 from))
         (end (min N until)))
    (cond ((and (zero? start) (= end N))
           (%this))
          ((>= start end)
           (rich-string :empty))
          (else
           (rich-string (u8-substring data start end))))))

(chained-define (%take n)
  (%slice 0 n))

(chained-define (%take-right n)
  (%slice (- N n) N))

(chained-define (%drop n)
  (%slice n N))

(chained-define (%drop-right n)
  (%slice 0 (- N n)))

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
  (cond ((string? elem)
         (string-contains data elem))
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
  
  (let1 positive-start-index (max 0 start-index)
    (cond ((string? str/char)
           (inner-index-of str/char positive-start-index))
          ((rich-string :is-type-of str/char)
           (inner-index-of (str/char :get) positive-start-index))
          ((char? str/char)
           (inner-index-of (string str/char) positive-start-index))
          ((rich-char :is-type-of str/char)
           (inner-index-of (str/char :make-string) positive-start-index))
          (else (type-error "rich-string%index-of: first parameter must be string/rich-string/char/rich-char")))))

(chained-define (%map f)
  (box ((%to-rich-vector)
        :map f
        :map (@ _ :make-string)
        :make-string)))

(define (%count pred?)
  ((%to-rich-vector) :count pred?))

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

(chained-define (%+ s)
  (cond
    ((string? s)
     (rich-string (string-append data s)))
    ((rich-string :is-type-of s)
     (rich-string (string-append data (s :get))))
    ((number? s)
     (rich-string (string-append data (number->string s))))
    (else
      (type-error (string-append (object->string s) "is not string or rich-string or number")))))

(chained-define (%strip-left)
  (rich-string (string-trim data)))

(chained-define (%strip-right)
  (rich-string (string-trim-right data)))

(chained-define (%strip-both)
  (rich-string (string-trim-both data)))

(chained-define (%strip-prefix prefix)
  (rich-string (string-remove-prefix data prefix)))

(chained-define (%strip-suffix suffix)
  (rich-string (string-remove-suffix data suffix)))

(chained-define (%replace-first old new)
  (let ((next-pos (%index-of old)))
    (if (= next-pos -1)
        (%this)
        ((%slice 0 next-pos)
         :+ new
         :+ (%drop (+ next-pos ($ old :length)))))))

(chained-define (%replace old new)
  (define (replace-helper str old new start)
    (let ((next-pos ((rich-string str) :index-of old start)))
      (if (= next-pos -1)
          str
          (replace-helper ((rich-string str) :replace-first old new :get)
                          old new next-pos))))
  (rich-string (replace-helper data old new 0)))

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

(typed-define (%or-else (default case-class?))
  (when (not (default :is-instance-of 'option))
    (type-error "The first parameter of option%or-else must be a option case class"))

  (if (null? value)
      default
      (option value)))

(define (%equals that)
  (== value (that 'value)))

(define (%defined?) (not (null? value)))
  
(define (%empty?) (null? value))

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

(chained-define (%map f)
  (if (null? value)
      (option '())
      (option (f value))))

(chained-define (%flat-map f)
  (if (null? value)
      (option '())
      (f value)))

(chained-define (%filter pred)
  (if (or (null? value) (not (pred value)))
      (option '())
      (option value)))

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

(typed-define (%or-else (default case-class?))
  (when (not (default :is-instance-of 'either))
    (type-error "The first parameter of either%or-else must be a either case class"))

  (if (%right?)
      (%this)
      default))

(define (%get-or-else default)
  (if (%right?)
      value
      default))

(typed-define (%filter-or-else (pred procedure?) (zero any?))
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

(chained-define (%map f)
  (if (%right?)
      (right (f value))
      (%this)))

(chained-define (%flat-map f)
  (if (%right?)
      (f value)
      (%this)))

(typed-define (%forall (pred procedure?))
  (if (%right?)
      (pred value)
      #t))

(typed-define (%exists (pred procedure?))
  (if (%right?)
      (pred value)
      #f))

)

(define (left v)
  (either 'left v))

(define (right v)
  (either 'right v))

(define-case-class rich-list ((data list?))

(define (@range start end . step)
  (let ((step-size (if (null? step) 1 (car step))))
    (cond
      ((and (positive? step-size) (>= start end))
       (rich-list '()))
      ((and (negative? step-size) (<= start end))
       (rich-list '()))
      ((zero? step-size)
       (value-error "Step size cannot be zero"))
      (else
       (let1 cnt (ceiling (/ (- end start) step-size))
         (rich-list (iota cnt start step-size)))))))

(chained-define (@empty)
  (rich-list (list )))

(chained-define (@concat lst1 lst2)
  (rich-list (append (lst1 :collect) (lst2 :collect))))

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

(define (%head)
  (if (null? data)
      (error 'out-of-range "rich-list%head: list is empty")
      (car data)))


(define (%head-option)
  (if (null? data)
      (none)
      (option (car data))))


(chained-define (%slice from until)
  (let* ((len (length data))
         (start (max 0 (min from len)))
         (end (max 0 (min until len))))
    (if (< start end)
        (rich-list (take (drop data start) (- end start)))
        (rich-list '()))))

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

(chained-define (%map x)
  (rich-list (map x data)))

(chained-define (%flat-map x)
  (rich-list (flat-map x data)))

(chained-define (%filter x)
  (rich-list (filter x data)))

(define (%for-each x)
  (for-each x data))

(chained-define (%reverse)
  (rich-list (reverse data)))
    
(chained-define (%take x)
  (typed-define (scala-take (data list?) (n integer?))
    (cond ((< n 0) '())
          ((>= n (length data)) data)
          (else (take data n))))
  (rich-list (scala-take data x)))

(chained-define (%drop x)
  (typed-define (scala-drop (data list?) (n integer?))
    (cond ((< n 0) data)
          ((>= n (length data)) '())
          (else (drop data n))))
  (rich-list (scala-drop data x)))

(chained-define (%take-right x)
  (typed-define (scala-take-right (data list?) (n integer?))
    (cond ((< n 0) '())
          ((>= n (length data)) data)
          (else (take-right data n))))
  (rich-list (scala-take-right data x)))

(chained-define (%drop-right x)
  (typed-define (scala-drop-right (data list?) (n integer?))
    (cond ((< n 0) data)
          ((>= n (length data)) '())
          (else (drop-right data n))))
  (rich-list (scala-drop-right data x)))

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

(chained-define (%sort-with less-p)
  (let ((sorted-data (list-stable-sort less-p data)))
        (rich-list sorted-data)))

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

(chained-define (%zip l) (box (apply map cons (list data l))))

(chained-define (%zip-with-index)
  (let loop ((lst data) (idx 0) (result '()))
    (if (null? lst)
        (rich-list (reverse result))  
        (loop (cdr lst) 
              (+ idx 1) 
              (cons (cons idx (car lst)) result)))))

(chained-define (%distinct)
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
             (loop result (cdr data) ht)))))))

(define (%reduce f)
  (if (null? data)
      (value-error "rich-list%reduce: empty list is not allowed to reduce")
      (reduce f '() data)))

(define (%reduce-option f)
  (if (null? data)
      (none)
      (option (reduce f '() data))))

(define (%to-string)
  (object->string data))

(define (%make-string . xs)
  (define (parse-args xs)
    (cond
      ((null? xs) (values "" "" ""))
      ((length=? 1 xs)
       (let1 sep (car xs)
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
    (let1 as-string (lambda (x) (if (string? x) x (object->string x)))
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
       (let1 cnt (ceiling (/ (- end start) step-size))
         (rich-vector (list->vector (iota cnt start step-size))))))))

(chained-define (@empty)
  (rich-vector #()))

(chained-define (@fill n elem)
  (unless (integer? n)
    (type-error "n must be integer" n))
  (when (< n 0)
    (value-error "n must be non-negative" n))
  (rich-vector (make-vector n elem)))

(define (%collect) data)

(define (%length)
  (vector-length data))

(define (%size)
  (vector-length data))

(define (%apply n)
  (vector-ref data n))

(define (%find p)
  (let loop ((i 0))
    (cond
     ((>= i (vector-length data)) (none))
     ((p (vector-ref data i)) (option (vector-ref data i)))
     (else (loop (+ i 1))))))

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
      (error 'out-of-range "out-of-range"))))

(define (%last-option)
  (let ((len (vector-length data)))
    (if (> len 0)
      (option (vector-ref data (- len 1)))
      (none))))

(chained-define (%slice from until)
  (let* ((len (vector-length data))
         (start (max 0 from))
         (end (min len until)))
    (if (< start end)
        (rich-vector (vector-copy data start end))
        (rich-vector :empty))))

(define (%empty?)
  (= (length data) 0))

(define (%equals that)
  (and (that :is-instance-of 'rich-vector)
       (vector= == data (that 'data))))

(define (%forall p)
  (vector-every p data))

(define (%exists p)
  (vector-any p data))

(chained-define (%map x)
  (rich-vector (vector-map x data)))

(chained-define (%filter x)
  (rich-vector (vector-filter x data)))

(define (%for-each x)
  (vector-for-each x data))

(define (%count . xs)
  (cond ((null? xs) (vector-length data))
        ((length=? 1 xs) (vector-count (car xs) data))
        (else (error 'wrong-number-of-args "rich-vector%count" xs))))

(chained-define (%take n)
  (typed-define (scala-take (data vector?) (n integer?))
    (cond
      ((< n 0) (vector))
      ((>= n (vector-length data)) data)
      (else
        (let ((new-vec (make-vector n)))
          (do ((i 0 (+ i 1)))
              ((>= i n) new-vec)
            (vector-set! new-vec i (vector-ref data i)))))))
  
  (rich-vector (scala-take data n)))

(chained-define (%take-right n)
  (typed-define (scala-take-right (data vector?) (n integer?))
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

  (rich-vector (scala-take-right data n)))

(chained-define (%drop n)
  (typed-define (scala-drop (data vector?) (n integer?))
    (cond
      ((< n 0) data)
      ((>= n (vector-length data)) (vector))
      (else (vector-copy data n))))
  
  (rich-vector (scala-drop data n)))

(chained-define (%drop-right n)
  (typed-define (scala-drop-right (data vector?) (n integer?))
    (cond
      ((< n 0) data)
      ((>= n (vector-length data)) (vector))
      (else (vector-copy data 0 (- (vector-length data) n)))))
  
  (rich-vector (scala-drop-right data n)))

  (define (%fold initial f)
    (vector-fold f initial data))

  (define (%fold-right initial f)
    (vector-fold-right f initial data))

  (define (%count . xs)
    (cond ((null? xs) (vector-length data))
          ((length=? 1 xs) (count (car xs) (vector->list data)))
          (else (error 'wrong-number-of-args "rich-vector%count" xs))))

(chained-define (%sort-with less-p)
  (rich-vector (vector-stable-sort less-p data)))

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

(chained-define (%zip-with-index)
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
            (loop (+ idx 1)))))))

(chained-define (%distinct)
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
                (loop result (+ index 1))))))))

(define (%to-string)
  ((%map object->string)
   :make-string "#(" " " ")"))

(define (%make-string . xs)
  (define (parse-args xs)
    (cond
      ((null? xs) (values "" "" ""))
      ((length=? 1 xs)
       (let1 sep (car xs)
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
    (error 'out-of-range "rich-vector%set! out of range at index" i))
  (vector-set! data i x))

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

(define (%contains k)
  (hash-table-contains? data k))

(define (%forall pred?)
  (let ((all-kv (map identity data)))
    (let loop ((kvs all-kv))  
      (if (null? kvs)
          #t  
          (let1 kv (car kvs)
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

(chained-define (%map f)
  (let1 r (make-hash-table)
    (hash-table-for-each
       (lambda (k v)
         (receive (k1 v1) (f k v)
           (hash-table-set! r k1 v1)))
       data)
    (rich-hash-table r)))

(define (%count pred)
  (hash-table-count pred data))

(define (%for-each proc)
  (hash-table-for-each proc data))

)

) ; end of begin
) ; end of library

