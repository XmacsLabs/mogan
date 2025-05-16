;
; Copyright (C) 2025 The Goldfish Scheme Authors
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

(define-library (liii oop)
(import (liii base) (liii list) (liii string))
(export
  @ typed-define define-case-class define-object define-class
  case-class? == != chained-define display* object->string
  chain-apply
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
         (field-count (length field-names))

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
         ;(default-static-messages '(:is-type-of))
         (internal-methods
           (filter (lambda (method) (not (or (string-starts? (symbol->string (caadr method)) "%")
                                             (string-starts? (symbol->string (caadr method)) "@"))))
                   methods))
         (this-symbol (gensym))
         (f-make-case-class (string->symbol (string-append "make-case-class-" (symbol->string class-name)))))

`(define (,class-name . args)

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

(define* (,f-make-case-class 
  ,@(map  
    (lambda (param)
      (let  ((param-name (car param))
            (type-pred (cadr param))
            (default-value (cddr param)))
            (if (null? default-value)
                param-name
                `(,param-name ,(car default-value)))))
    fields))
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
                        ,f-make-case-class
                        ',field-names
                        ',param-name
                        ,type-name-str
                        (object->string ,param-name))))))
      fields)

  (define ,this-symbol #f)
  (define (%this . xs)
    (if (null? xs)
      ,this-symbol
      (apply ,this-symbol xs)))

  (define (%is-instance-of x)
    (eq? x ',class-name))
         
  (define (%equals that)
    (unless (case-class? that) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %equals '(that) 'that "case-class" (object->string that))))
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
         (case msg
           ,@(map (lambda (method expected)
                    `((,expected) (apply ,method args)))
                  instance-method-symbols instance-messages)
           (else (value-error ,class-name "No such method: " msg))))
        ,@(map (lambda (field) `((eq? msg ',(car field)) ,(car field))) fields)
        (else (apply %apply (cons msg args))))))

  (set! ,this-symbol (instance-dispatcher))
  ,this-symbol
) ; end of the internal typed define

(if (null? args)
    (,f-make-case-class)
    (let ((msg (car args)))
      (cond ((member msg (list ,@static-messages :is-type-of))
             (apply static-dispatcher args))
            ((and (zero? ,field-count) (member :apply (list ,@static-messages)))
             (apply static-dispatcher (cons :apply args)))
            (else
             (apply ,f-make-case-class args)))))

) ; end of define
) ; end of let
) ; end of define-macro

(define-macro (define-object object-name . methods)
  `(define-case-class ,object-name () ,@methods))

(define-macro (define-class class-name private-fields . private-fields-and-methods)
  (let* ((field-defs '())
         (getter-defs '())
         (setter-defs '())
         
         ;; generate define, getter, setter
         (process-fields
          (map (lambda (field-spec)
                 (let* ((field-name (car field-spec))
                        (type-pred (cadr field-spec))
                        (default-value (if (>= (length field-spec) 3)
                                          (caddr field-spec)
                                          ''()))
                        (getter-name (string->symbol 
                                      (string-append "%get-" (symbol->string field-name))))
                        (setter-name (string->symbol 
                                      (string-append "%set-" (symbol->string field-name) "!"))))
                   
                   (set! field-defs 
                         (cons `(define ,field-name ,default-value) field-defs))
                   
                   (set! getter-defs 
                         (cons `(define (,getter-name) ,field-name) getter-defs))
                   
                   (set! setter-defs 
                         (cons `(typed-define (,setter-name (x ,type-pred))
                                  (set! ,field-name x))
                               setter-defs))))
               private-fields)))
    
    `(define-case-class ,class-name ()
       ;; define
       ,@(reverse field-defs)
       
       ;; Getter
       ,@(reverse getter-defs)
       
       ;; Setter
       ,@(reverse setter-defs)
       
       ;; else
       ,@private-fields-and-methods)))

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

(define (chain-apply args r)
  (if (null? args)
      r
      (apply r args)))

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

) ; end of begin
) ; end of define-library
