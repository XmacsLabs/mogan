(define-library (liii pretty-print)
(export pretty-print pp pp-post pp-multi-comment pp-single-comment)
(import (liii string))
(begin

(define pretty-print  ; (lambda* (obj (port (current-output-port)) (column 0))

  (let ((*pretty-print-length* 100)
        (*pretty-print-spacing* 2)
        (*pretty-print-float-format* "~,4F")
        (*pretty-print-left-margin* 0)
        (*pretty-print-cycles* #t)) ; if this is #f, you're guaranteeing that there won't be any circular structures
    ;; e.g. (let-temporarily ((((outlet (funclet pretty-print)) '*pretty-print-cycles*) #f)) ...)
    (define pretty-print-1
      (letrec ((messy-number (lambda (z)
                               (if (real? z)
                                   (if (or (nan? z)
                                           (infinite? z))
                                       (object->string z)
                                       (if (= z pi)
                                           (copy "pi")
                                           (format #f *pretty-print-float-format* z)))
                                   (format #f "~A~A~Ai"
                                           (messy-number (real-part z))
                                           (if (negative? (imag-part z)) "-" "+")
                                           (messy-number (abs (imag-part z)))))))

               (any-keyword? (lambda (lst)
                               (and (pair? lst)
                                    (or (keyword? (car lst))
                                        (any-keyword? (cdr lst))))))

               (any-let-or-hash-table? (lambda (sequence)
                                         (and (pair? sequence)
                                              (or (let? (car sequence))
                                                  (hash-table? (car sequence))
                                                  (any-let-or-hash-table? (cdr sequence)))))))
        (let ((newlines 0))

          (define (spaces port n)
            (set! newlines (+ newlines 1))
            (format port "~%~NC" (+ n *pretty-print-left-margin*) #\space))

          (define (stacked-list port lst col)
            (do ((p lst (cdr p))
                 (added 0 0))
                ((not (pair? p)))
              (let ((obj (car p)))
                (if (not (eq? p lst))
                    (spaces port col))
                (let ((len (length (object->string obj))))
                  (when (and (keyword? obj)
                             (pair? (cdr p)))
                    (write obj port)
                    (write-char #\space port)
                    (set! added (+ 1 len))
                    (set! p (cdr p))
                    (set! obj (car p))) ; pair? cdr p above

                  ;; 处理 PP_SINGLE_COMMENT 的特殊逻辑

                  (cond ((or (hash-table? obj)
                             (let? obj))
                         (pretty-print-1 obj port col))

                        ((and (pair? obj)
                              (pair? (cdr obj))
                              (null? (cddr obj))
                              (> len (/ *pretty-print-length* 2))
                              (not (eq? (car obj) '*PP_SINGLE_COMMENT*)))  ; PP_SINGLE_COMMENT 不应该应用长对格式化
                         (if (eq? (car obj) 'quote)
                             (write-char #\' port)
                             (begin
                               (write-char #\( port)
                               (pretty-print-1 (car obj) port col)
                               (spaces port (+ col 1))))
                         (pretty-print-1 (cadr obj) port (+ col 1))
                         (if (not (eq? (car obj) 'quote))
                             (write-char #\) port)))

                        (else
                         (pretty-print-1 obj port (+ col added))))))))

          (define (stacked-split-list port lst col)
            (if (not (pair? lst))
                (write lst port)
                (do ((p lst (cdr p)))
                    ((not (pair? p)))
                  (if (not (eq? p lst)) (spaces port col))
                  (if (pair? (car p))
                      (begin
                        (format port "(~S " (caar p))
                        (if (and (pair? (cdar p))
                                 (symbol? (caar p)))
                            (pretty-print-1 (cadar p) port (+ col (length (symbol->string (caar p))) 2))
                            (write (cdar p) port))
                        (write-char #\) port))
                      (write (car p) port))))) ; pretty-print? (it's always a symbol)

          (let ((writers
                 (let ((h (make-hash-table)))

                   ;; -------- quote
                   (define (w-quote obj port column)
                     (if (not (pair? (cdr obj))) ; (quote) or (quote . 1)
                         (write obj port)
                         (begin
                           (write-char #\' port)
                           (pretty-print-1 (cadr obj) port column))))
                   (hash-table-set! h 'quote w-quote) ; what about #_quote?

                  ;; -------- define
      (define (w-define obj port column)
        (if (not (pair? (cdr obj)))
            (write obj port)
            (begin
              (format port "(~A ~A" (car obj) (cadr obj))
              (if (pair? (cadr obj))
                  (begin
                    (spaces port (+ column *pretty-print-spacing*))
                    (stacked-list port (cddr obj) (+ column *pretty-print-spacing*)))
                  (if (pair? (cddr obj))
                      (let ((str (object->string (caddr obj))))
                        (if (> (length str) 60)
                            (begin
                              (spaces port (+ column *pretty-print-spacing*))
                              (pretty-print-1 (caddr obj) port (+ column *pretty-print-spacing*)))
                            (begin
                              (write-char #\space port)
                              (write (caddr obj) port)))
                        (if (pair? (cdddr obj))
                            (begin
                              (write-char #\space port)
                              (write (cadddr obj) port)
                              (if (pair? (cddddr obj))
                                  (begin
                                    (spaces port (+ column *pretty-print-spacing*))
                                    (stacked-list port (cddddr obj) (+ column *pretty-print-spacing*)))))))
                      (write (cddr obj) port)))
            (write-char #\) port))))
      (hash-table-set! h 'define w-define)

                  ;; -------- define-constant
      (define (w-define-constant obj port column)
        (if (not (and (pair? (cdr obj))
                      (pair? (cddr obj))))
            (write obj port)
            (begin
              (format port "(~A ~A" (car obj) (cadr obj))
              (if (pair? (cddr obj))
                  (let ((str (object->string (caddr obj))))
                    (if (> (length str) 60)
                        (begin
                          (spaces port (+ column *pretty-print-spacing*))
                          (pretty-print-1 (caddr obj) port (+ column *pretty-print-spacing*)))
                        (begin
                          (write-char #\space port)
                          (write (caddr obj) port))))
                  (write (cddr obj) port))
              (write-char #\) port))))
      (hash-table-set! h 'define-constant w-define-constant)
      (hash-table-set! h #_define-constant w-define-constant)

                   ;; -------- if
                   (define (w-if obj port column if-str)
                     (let ((objstr (object->string obj))
                           (ifcol (+ column 4)))
                       (if (< (length objstr) 40)
                           (display objstr port)
                           (begin
                             (format port "(~A " if-str)
                             (pretty-print-1 (cadr obj) port ifcol)
                             (when (pair? (cddr obj)) ; might be a messed-up if
                               (spaces port ifcol)
                               (pretty-print-1 (caddr obj) port ifcol)
                               (when (pair? (cdddr obj))
                                 (spaces port ifcol)
                                 (pretty-print-1 (cadddr obj) port ifcol)))
                             (write-char #\) port)))))
                   (hash-table-set! h 'if (lambda (obj port col) (w-if obj port col "if")))
                   (hash-table-set! h #_if (lambda (obj port col) (w-if obj port col "#_if")))

                   ;; -------- when unless
                   (define (w-when obj port column str)
                     (let ((objstr (object->string obj)))
                       (if (< (length objstr) 40)
                           (display objstr port)
                           (begin
                             (format port "(~A " str)
                             (pretty-print-1 (cadr obj) port (+ column (+ 2 (string-length str)))) ;(if (eq? (car obj) 'when) 6 8)))
                             (spaces port (+ column *pretty-print-spacing*))
                             (when (pair? (cddr obj))
                               (stacked-list port (cddr obj) (+ column *pretty-print-spacing*)))
                             (write-char #\) port)))))
                   (hash-table-set! h 'when (lambda (obj port col) (w-when obj port col "when")))
                   (hash-table-set! h 'unless (lambda (obj port col) (w-when obj port col "unless")))
                   (hash-table-set! h #_when (lambda (obj port col) (w-when obj port col "#_when")))
                   (hash-table-set! h #_unless (lambda (obj port col) (w-when obj port col "#_unless")))

                   ;; -------- let let* letrec letrec*
                   (define (w-let obj port column let-str)
                     (if (not (and (pair? (cdr obj))
                                   (pair? (cddr obj))))
                         (write obj port)
                         (begin
                           (let ((head-len (length let-str)))
                             (if (symbol? (cadr obj))
                                 (begin
                                   (format port "(~A ~A (" let-str (cadr obj))
                                   (if (pair? (cddr obj))
                                       (if (pair? (caddr obj)) ; (let x () ...)
                                           (stacked-split-list port (caddr obj) (+ column head-len (length (symbol->string (cadr obj))) 4))
                                           (if (not (null? (caddr obj)))
                                               (write (caddr obj) port))) ; () is already being written
                                       (if (not (null? (cddr obj)))
                                           (format port " . ~S" (cddr obj)))))
                                 (begin
                                   (format port "(~A (" let-str)
                                   (if (pair? (cadr obj))
                                       (stacked-split-list port (cadr obj) (+ column head-len 3))))))
                           (write-char #\) port)
                           (spaces port (+ column *pretty-print-spacing*))
                           (if (pair? ((if (symbol? (cadr obj)) cdddr cddr) obj))
                               (stacked-list port ((if (symbol? (cadr obj)) cdddr cddr) obj) (+ column *pretty-print-spacing*)))
                           (write-char #\) port))))
                   ;; -------- let1
                   (define (w-let1 obj port column let1-str)
                     (if (not (and (pair? (cdr obj))
                                   (pair? (cddr obj))
                                   (pair? (cdddr obj))))
                         (write obj port)
                         (begin
                           (format port "(~A ~A ~A" let1-str (cadr obj) (caddr obj))
                           (spaces port (+ column *pretty-print-spacing*))
                           (if (pair? (cdddr obj))
                               (stacked-list port (cdddr obj) (+ column *pretty-print-spacing*))
                               (write (cdddr obj) port))
                           (write-char #\) port))))
                   (hash-table-set! h 'let1 (lambda (obj port col) (w-let1 obj port col "let1")))
                   (hash-table-set! h #_let1 (lambda (obj port col) (w-let1 obj port col "#_let1")))

                   (for-each
                    (lambda (f str)
                      (hash-table-set! h f (lambda (obj port col) (w-let obj port col str))))
                    (list 'let 'let* 'letrec 'letrec* #_let #_let* #_letrec #_letrec*)
                    (list "let" "let*" "letrec" "letrec" "#_let" "#_let*" "#_letrec" "#_letrec*"))

                   ;; -------- set!
                   (define (w-set obj port column set-str)
                     (let ((str (object->string obj)))

                       (if (<= (length str) 60)
                           (display str port)
                           (let ((settee (object->string (cadr obj))))
                             (format port "(~A ~A" set-str settee)
                             (if (pair? (cddr obj))
                                 (if (> (length settee) 20)
                                     (begin
                                       (spaces port (+ column 6))
                                       (pretty-print-1 (caddr obj) port (+ column 6)))
                                     (begin
                                       (write-char #\space port)
                                       (pretty-print-1 (caddr obj) port (+ column 7 (length settee))))))
                             (write-char #\) port)))))
                   (hash-table-set! h 'set! (lambda (obj port col) (w-set obj port col "set!")))
                   (hash-table-set! h #_set! (lambda (obj port col) (w-set obj port col "#_set!")))

                   ;; -------- cond
                   (define (w-cond obj port column cond-str)
                     (format port "(~A " cond-str)
                     (do ((lst (cdr obj) (cdr lst)))
                         ((not (pair? lst)))
                       (if (not (eq? lst (cdr obj)))
                           (spaces port (+ column 6)))
                       (if (not (pair? (car lst)))
                           (write (car lst) port)
                           (let ((has=> (and (pair? (cdar lst))
                                             (eq? (cadar lst) '=>))))
                             (let ((extras (and (pair? (cdar lst))
                                                (pair? (cddar lst))
                                                (or (not has=>)
                                                    (pair? (cdddar lst)))))
                                   (too-long (> (length (object->string (cdar lst))) 50)))
                               (write-char #\( port)
                               (let ((oldlines newlines))
                                 (pretty-print-1 (caar lst) port (+ column 7))
                                 (if (or extras
                                         (not (= oldlines newlines))
                                         too-long)
                                     (spaces port (+ column 7))
                                     (if (and (pair? (cdar lst))
                                              (or (not has=>)
                                                  (= oldlines newlines)))
                                         (write-char #\space port)))
                                 (if (and (pair? (cdar lst))
                                          (not extras)
                                          (not too-long))
                                     (begin
                                       (write (cadar lst) port)
                                       (when (and has=>
                                                  (pair? (cddar lst)))
                                         (write-char #\space port)
                                         (write (caddar lst) port)))
                                     (if (not (null? (cdar lst)))
                                         (stacked-list port (cdar lst) (+ column 7)))))
                               (write-char #\) port)))))
                     (write-char #\) port))
                   (hash-table-set! h 'cond (lambda (obj port col) (w-cond obj port col "cond")))
                   (hash-table-set! h #_cond (lambda (obj port col) (w-cond obj port col "#_cond")))

                   ;; -------- and or
                   (define (w-and obj port column and-str)
                     (if (> (length (object->string obj)) 40)
                         (begin
                           (format port "(~A " and-str)
                           (stacked-list port (cdr obj) (+ column *pretty-print-spacing* (length (symbol->string (car obj)))))
                           (write-char #\) port))
                         (write obj port)))
                   (hash-table-set! h 'and (lambda (obj port col) (w-and obj port col "and")))
                   (hash-table-set! h 'or (lambda (obj port col) (w-and obj port col "or")))
                   (hash-table-set! h #_and (lambda (obj port col) (w-and obj port col "#_and")))
                   (hash-table-set! h #_or (lambda (obj port col) (w-and obj port col "#_or")))

                   ;; -------- case
                   (define (w-case obj port column case-str)
                     (if (not (and (pair? (cdr obj))
                                   (pair? (cddr obj))))
                         (write obj port)
                         (begin
                           (format port "(~A ~A" case-str (cadr obj)) ; send out the selector
                           (for-each
                            (lambda (lst)
                              (spaces port (+ column *pretty-print-spacing*))
                              (if (not (pair? lst))
                                  (write lst port)
                                  (begin
                                    (write-char #\( port)
                                    (if (not (pair? (car lst)))
                                        (write (car lst) port)
                                        (let ((len (length (car lst))))
                                          (if (< len 6)
                                              (write (car lst) port)
                                              (let ((p (car lst)))
                                                (write-char #\( port)
                                                (do ((i 0 (+ i 6)))
                                                    ((>= i len))
                                                  (do ((j 0 (+ j 1)))
                                                      ((or (= j 6) (null? p)) (if (pair? p) (spaces port (+ column 4))))
                                                    (write (car p) port)
                                                    (set! p (cdr p))
                                                    (if (pair? p)
                                                        (write-char #\space port))))
                                                (write-char #\) port)))))
                                    (if (not (null? (cdr lst)))
                                        (if (and (pair? (cdr lst))
                                                 (or (and (null? (cddr lst))
                                                          (< (length (object->string (cadr lst))) 60))
                                                     (and (eq? (cadr lst) '=>)
                                                          (null? (cdddr lst))
                                                          (< (length (object->string (caddr lst))) 60))))
                                            (begin
                                              (write-char #\space port)
                                              (write (cadr lst) port)
                                              (when (and (eq? (cadr lst) '=>)
                                                         (pair? (cddr lst)))
                                                (write-char #\space port)
                                                (write (caddr lst) port)))
                                            (begin
                                              (spaces port (+ column 3))
                                              (stacked-list port (cdr lst) (+ column 3)))))
                                    (write-char #\) port))))
                            (cddr obj))
                           (write-char #\) port))))
                   (hash-table-set! h 'case (lambda (obj port col) (w-case obj port col "case")))
                   (hash-table-set! h #_case (lambda (obj port col) (w-case obj port col "#_case")))

                   ;; -------- map for-each
                   (define (w-map obj port column map-str)
                     (let* ((objstr (object->string obj))
                            (strlen (length objstr)))
                       (if (< (+ column strlen) *pretty-print-length*)
                           (display objstr port)
                           (begin
                             (format port "(~A" map-str)
                             (when (pair? (cdr obj))
                               (write-char #\space port)
                               (stacked-list port (cdr obj) (+ column *pretty-print-spacing*)))
                             (write-char #\) port)))))
                   (hash-table-set! h 'map (lambda (obj port col) (w-map obj port col "map")))
                   (hash-table-set! h 'for-each (lambda (obj port col) (w-map obj port col "for-each")))
                   (hash-table-set! h #_map (lambda (obj port col) (w-map obj port col "#_map")))
                   (hash-table-set! h #_for-each (lambda (obj port col) (w-map obj port col "#_for-each")))

                   ;; -------- do
                   (define (w-do obj port column do-str)
                     (if (not (pair? (cdr obj)))
                         (write obj port)
                         (begin
                           (format port "(~A " do-str)
                           (if (list? (cadr obj))
                               (write-char #\( port)
                               (display (cadr obj) port))
                           (if (pair? (cadr obj))
                               (stacked-list port (cadr obj) (+ column 5)))
                           (if (list? (cadr obj))
                               (write-char #\) port))
                           (when (pair? (cddr obj))
                             (spaces port (+ column 4))
                             (let ((end (caddr obj)))
                               (if (< (length (object->string end)) (- *pretty-print-length* column))
                                   (write end port)
                                   (begin
                                     (write-char #\( port)
                                     (pretty-print-1 (car end) port (+ column 4))
                                     (spaces port (+ column 5))
                                     (stacked-list port (cdr end) (+ column 5))
                                     (write-char #\) port))))
                             (when (pair? (cdddr obj))
                               (spaces port (+ column *pretty-print-spacing*))
                               (stacked-list port (cdddr obj) (+ column *pretty-print-spacing*))))
                           (write-char #\) port))))
                   (hash-table-set! h 'do (lambda (obj port col) (w-do obj port col "do")))
                   (hash-table-set! h #_do (lambda (obj port col) (w-do obj port col "#_do")))

                   ;; -------- begin etc
                   (define (w-begin obj port column begin-str)
                     (format port "(~A" begin-str)
                     (when (pair? (cdr obj))
                       (spaces port (+ column *pretty-print-spacing*))
                       (stacked-list port (cdr obj) (+ column *pretty-print-spacing*)))
                     (write-char #\) port))
                   (for-each
                    (lambda (f str)
                      (hash-table-set! h f (lambda (obj port col) (w-begin obj port col str))))
                    (list 'begin 'call-with-exit 'call/cc 'call-with-current-continuation 'with-baffle
                          'with-output-to-string 'call-with-output-string 'hash-table 'inlet
                          #_begin #_call-with-exit #_call/cc #_call-with-current-continuation #_with-baffle
                          #_with-output-to-string #_call-with-output-string #_hash-table #_inlet)
                    (list "begin" "call-with-exit" "call/cc" "call-with-current-continuation" "with-baffle"
                          "with-output-to-string" "call-with-output-string" "hash-table" "inlet"
                          "#_begin" "#_call-with-exit" "#_call/cc" "#_call-with-current-continuation" "#_with-baffle"
                          "#_with-output-to-string" "#_call-with-output-string" "#_hash-table" "#_inlet"))

                   ;; -------- dynamic-wind call-with-values
                   (define (w-dynwind obj port column str)
                     (format port "(~A" str)
                     (spaces port (+ column *pretty-print-spacing*))
                     (stacked-list port (cdr obj) (+ column *pretty-print-spacing*))
                     (write-char #\) port))
                   (hash-table-set! h 'dynamic-wind (lambda (obj port col) (w-dynwind obj port col "dynamic-wind")))
                   (hash-table-set! h 'call-with-values (lambda (obj port col) (w-dynwind obj port col "call-with-values")))
                   (hash-table-set! h #_dynamic-wind (lambda (obj port col) (w-dynwind obj port col "#_dynamic-wind")))
                   ;(hash-table-set! h #_call-with-values (lambda (obj port col) (w-dynwind obj port col "#_call-with-values")))

                   ;; -------- lambda etc
                   (define (w-lambda obj port column str)
                     (if (not (and (pair? (cdr obj))
                                   (pair? (cddr obj))))
                         (write obj port)
                         (begin
                           (format port "(~A " str)
                           (pretty-print-1 (cadr obj) port (+ column *pretty-print-spacing* (length str)))
                           (spaces port (+ column *pretty-print-spacing*))
                           (stacked-list port (cddr obj) (+ column *pretty-print-spacing*))
                           (write-char #\) port))))
                   ;; -------- typed-lambda
                   (define (w-typed-lambda obj port column str)
                     (if (not (and (pair? (cdr obj))
                                   (pair? (cddr obj))))
                         (write obj port)
                         (begin
                           (format port "(~A " str)
                           (pretty-print-1 (cadr obj) port (+ column *pretty-print-spacing* (length str)))
                           (spaces port (+ column *pretty-print-spacing*))
                           (stacked-list port (cddr obj) (+ column *pretty-print-spacing*))
                           (write-char #\) port))))
                   (hash-table-set! h 'typed-lambda (lambda (obj port col) (w-typed-lambda obj port col "typed-lambda")))
                   (hash-table-set! h #_typed-lambda (lambda (obj port col) (w-typed-lambda obj port col "#_typed-lambda")))

                   (for-each
                    (lambda (f str)
                      (hash-table-set! h f (lambda (obj port col) (w-lambda obj port col str))))
                    (list 'lambda 'lambda* 'define* 'define-library 'define-macro 'define-macro* 'define-bacro 'define-bacro* 'with-let 
                          'call-with-input-string 'call-with-input-file 'call-with-output-file 'with-input-from-file 
                          'with-input-from-string 'with-output-to-file 'let-temporarily
                          #_lambda #_lambda* #_define* #_define-macro #_define-macro* #_define-bacro #_define-bacro* #_with-let 
                          #_call-with-input-string #_call-with-input-file #_call-with-output-file #_with-input-from-file 
                          #_with-input-from-string #_with-output-to-file #_let-temporarily)
                    (list "lambda" "lambda*" "define*" "define-library" "define-macro" "define-macro*" "define-bacro" "define-bacro*" "with-let" 
                          "call-with-input-string" "call-with-input-file" "call-with-output-file" "with-input-from-file" 
                          "with-input-from-string" "with-output-to-file" "let-temporarily"
                          "#_lambda" "#_lambda*" "#_define*" "#_define-macro" "#_define-macro*" "#_define-bacro" "#_define-bacro*" "#_with-let" 
                          "#_call-with-input-string" "#_call-with-input-file" "#_call-with-output-file" "#_with-input-from-file" 
                          "#_with-input-from-string" "#_with-output-to-file" "#_let-temporarily"))

                   ;; -------- catch
                   (define (w-catch obj port column str)
                     (if (not (pair? (cdr obj))) ; (catch) or (catch . 1)
                         (write obj port)
                         (begin
                           (format port "(~A ~S" str (cadr obj))
                           (spaces port (+ column *pretty-print-spacing*))
                           (stacked-list port (cddr obj) (+ column *pretty-print-spacing*))
                           (write-char #\) port))))
                   (hash-table-set! h 'catch (lambda (obj port col) (w-catch obj port col "catch")))
                   (hash-table-set! h #_catch (lambda (obj port col) (w-catch obj port col "#_catch")))

                   ;; -------- PP_NEWLINE
                   (define (w-pp-newline obj port column)
                     (let ((n (cadr obj)))  ; 获取数字参数
                       (if (>= n 2)
                           (display (make-string (- n 2) #\newline) port)
                           (display "" port))))  ; n<2时输出空字符串
                   (hash-table-set! h '*PP_NEWLINE* w-pp-newline)

                   ;; -------- PP_MULTI_COMMENT
                   (define (w-pp-multi-comment obj port column)
                     (let ((comment-texts (cdr obj)))  ; 获取所有注释内容
                       (display "#|" port)
                       (if (> (length comment-texts) 0)
                           (let loop ((lines comment-texts) (first? #t))
                             (if (not (null? lines))
                                 (let ((line (car lines)))
                                   (if (or (not first?) (not (string-null? line)))
                                       (begin
                                         (if (not first?) (newline port))
                                         (display line port)
                                         (loop (cdr lines) #f))
                                       (begin
                                         (if first? (newline port))  ; 第一个参数为空字符串时添加换行
                                         (loop (cdr lines) first?)))))))
                       (display "|#" port)))
                   (hash-table-set! h '*PP_MULTI_COMMENT* w-pp-multi-comment)

                   ;; -------- PP_SINGLE_COMMENT
                   (define (w-pp-single-comment obj port column)
                     (let ((comment-text (cadr obj)))  ; 获取注释内容
                       (display ";" port)
                       (if (not (string-null? comment-text))
                           (begin
                             (display " " port)
                             (display comment-text port)))))
                   (hash-table-set! h '*PP_SINGLE_COMMENT* w-pp-single-comment)

                   h)))

            ;; pretty-print-1
            (lambda (obj port column)

              (cond ((number? obj)
                     (if (rational? obj)
                         (write obj port)
                         (display (messy-number obj) port)))

                    ((or (int-vector? obj)
                         (float-vector? obj))
                     (if (> (vector-rank obj) 1)
                         (write obj port)
                         (let* ((objstr (object->string obj))
                                (strlen (length objstr)))
                           (if (< (+ column strlen) *pretty-print-length*)
                               (display objstr port)
                               (let ((name-len (if (int-vector? obj) 10 12)))
                                 (display (if (int-vector? obj) "(int-vector " "(float-vector ") port)
                                 (set! column (+ column 2))
                                 (if (< (- *pretty-print-length* column) 30)
                                     (spaces port column)
                                     (set! column (+ column name-len)))
                                 (do ((len (min (length obj) (*s7* 'print-length)))
                                      (col column)
                                      (i 0 (+ i 1)))
                                     ((= i len)
                                      (if (> len (*s7* 'print-length))
                                          (write-string " ...)" port)
                                          (write-char #\) port)))
                                   (let* ((numstr (number->string (obj i)))
                                          (numlen (length numstr)))
                                     (if (not (= col column))
                                         (if (> (+ col numlen) *pretty-print-length*)
                                             (begin
                                               (spaces port column)
                                               (set! col column))
                                             (display #\space port)))
                                     (display numstr port)
                                     (set! col (+ col numlen 1)))))))))

                    ((and *pretty-print-cycles*
                          (pair? (cyclic-sequences obj)))
                     (format port "~W" obj))

                    ((hash-table? obj)
                     (display "(hash-table" port)
                     (for-each (lambda (field)
                                 (let ((symstr (object->string (car field))))
                                   (spaces port (+ column 2))
                                   (format port "'~A " symstr)
                                   (pretty-print-1 (cdr field) port (+ column 4 (length symstr)))))
                               obj)
                     (write-char #\) port))

                    ((let? obj)
                     (if (and (openlet? obj)
                              (defined? 'pretty-print obj #t))  ; #t = locally defined
                         ((obj 'pretty-print) obj port column)
                         (begin
                           (display "(inlet" port)
                           (for-each (lambda (field)
                                       (let ((symstr (symbol->string (car field))))
                                         (spaces port (+ column 2))
                                         (format port ":~A " symstr)
                                         (pretty-print-1 (cdr field) port (+ column 4 (length symstr)))))
                                     obj)
                           (write-char #\) port))))

                    ((vector? obj)
                     (if (> (vector-rank obj) 1)
                         (write obj port)
                         (let* ((objstr (object->string obj))
                                (strlen (length objstr)))
                           (if (< (+ column strlen) *pretty-print-length*)
                               (display objstr port)
                               (begin
                                 (display "(vector " port)
                                 (set! column (+ column 2))
                                 (if (< (- *pretty-print-length* column) 30)
                                     (spaces port column)
                                     (set! column (+ column 6)))
                                 (do ((len (min (length obj) (*s7* 'print-length)))
                                      (col column)
                                      (i 0 (+ i 1)))
                                     ((= i len)
                                      (if (> len (*s7* 'print-length))
                                          (write-string " ...)" port)
                                          (write-char #\) port)))
                                   (let ((olen (length (object->string (obj i)))))
                                     (if (not (= col column))
                                         (if (> (+ col olen) *pretty-print-length*)
                                             (begin
                                               (spaces port column)
                                               (set! col column))
                                             (display #\space port)))
                                     (pretty-print-1 (obj i) port col)
                                     (set! col (+ col olen 1)))))))))

                    ((not (pair? obj))
                     (write obj port))

                    ((let ((val (hash-table-ref writers (car obj))))
                       (and (not (eq? val (*s7* 'hash-table-missing-key-value)))
                            val))
                     => (lambda (f) (f obj port column)))

                    ((any-let-or-hash-table? obj)
                     (let ((first #t))
                       (write-char #\( port)
                       (for-each (lambda (p)
                                   (if first (set! first #f) (spaces port (+ column 4)))
                                   (pretty-print-1 p port (+ column 4)))
                                 obj)
                       (write-char #\) port)))

                    (else
                     (let* ((objstr (object->string obj))
                            (strlen (length objstr)))
                       ;; 处理对象字符串
                       (if (< (+ column strlen) *pretty-print-length*)
                           (display objstr port)

                           ;; (format port "~A~A" (cond ((pair-line-number obj) => (lambda (line) (format #f "[~D] " line))) (else "")) objstr)
                           ;; the other case is above (writers)
                           ;;    (lambda (f) (format port "[~D]" line) (f obj port column))
                           ;;    with column check, line check etc
                           ;; so, the pp/profile hook only needs two calls
                           ;; (lambda (f obj port column) (f obj port column))
                           ;; and here (lambda (f obj port column) (f=display obj=objstr port)?

                           (let ((lstlen (length obj)))

                             (cond ((or (infinite? lstlen)
                                        (not (positive? lstlen)))
                                    (display objstr port))

                                   ((and (symbol? (car obj))
                                         (> (length (symbol->string (car obj))) 12)
                                         (pair? (cdr obj))
                                         (pair? (cadr obj))
                                         (memq (caadr obj) '(lambda lambda* let let* cond case letrec)))
                                    (write-char #\( port)
                                    (pretty-print-1 (car obj) port column)
                                    (spaces port (+ column 2))
                                    (stacked-list port (cdr obj) (+ column 2))
                                    (write-char #\) port))

                                   ((= lstlen 1)
                                    (if (pair? (car obj))
                                        (begin
                                          (write-char #\( port)
                                          (pretty-print-1 (car obj) port (+ column 1))
                                          (write-char #\) port))
                                        (display objstr port)))

                                   ((and (pair? (car obj))
                                         (memq (caar obj) '(lambda lambda* let let* letrec letrec* cond if case)))
                                    (write-char #\( port)
                                    (pretty-print-1 (car obj) port column)
                                    (spaces port (+ column 1))
                                    (if (and (memq (caar obj) '(cond if case))
                                             (do ((p (cdr obj) (cdr p)))
                                                 ((or (null? p)
                                                      (pair? (car p)))
                                                  (null? p))))
                                        (do ((p (cdr obj) (cdr p)))
                                            ((null? p))
                                          (display (car p) port)
                                          (if (pair? (cdr p))
                                              (write-char #\space port)))
                                        (stacked-list port (cdr obj) (+ column 1)))
                                    (write-char #\) port))

                                   (else
                                    (let* ((carstr (object->string (car obj)))
                                           (carstrlen (length carstr)))
                                      (if (eq? (car obj) 'quote)
                                          (write-char #\' port)
                                          (format port "(~A" carstr))
                                      (if (any-keyword? (cdr obj))
                                          (begin
                                            (spaces port (+ column *pretty-print-spacing*))
                                            (stacked-list port (cdr obj) (+ column *pretty-print-spacing*)))
                                          (let ((line-start (+ column *pretty-print-spacing*
                                                               (if (> carstrlen 16) 0 carstrlen))))
                                            (case lstlen
                                              ((2)
                                               (write-char #\space port)
                                               (pretty-print-1 (cadr obj) port line-start))

                                              ((3)
                                               (write-char #\space port)
                                               (stacked-list port (cdr obj) line-start))

                                              (else
                                               (do ((obj-start line-start)
                                                    (lst (cdr obj) (cdr lst)))
                                                   ((null? lst))
                                                 (let* ((str (object->string (car lst)))
                                                        (strlen1 (length str)))
                                                   (if (and (> strlen1 (- *pretty-print-length* obj-start))
                                                            (not (eq? lst (cdr obj))))
                                                       (begin
                                                         (set! obj-start (+ line-start 1 strlen1))
                                                         (spaces port line-start)
                                                         (pretty-print-1 (car lst) port line-start))
                                                       (let ((at-line-start (= line-start obj-start)))
                                                         (set! obj-start (+ obj-start 1 strlen1))
                                                         (if (> strlen1 40)
                                                             (begin
                                                               (if at-line-start
                                                                   (write-char #\space port)
                                                                   (spaces port line-start))
                                                               (pretty-print-1 (car lst) port line-start))
                                                             (begin
                                                               (write-char #\space port)
                                                               (display str port)))))))))))
                                      (if (not (eq? (car obj) 'quote))
                                          (write-char #\) port)))))))))))))))

    ;; pretty-print
    (lambda* (obj (port (current-output-port)) (column 0))
      (let ((old-port port))
        (if (boolean? old-port)
            (set! port (open-output-string)))
        (pretty-print-1 obj port column)
        (flush-output-port port)
        (if (not (boolean? old-port))
            (values)
            (let ((str (get-output-string port)))
              (close-output-port port)
              (if (eq? old-port #t)
                  (display str))
              str))))))

(define (pp obj)
  (call-with-output-string
   (lambda (p)
     ((if (keyword? obj) display pretty-print) obj p))))

) ; end of begin
) ; end of define-library
