
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : data/mgs.scm
;; DESCRIPTION : Mogan Scheme (.mgs) data format
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data mgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme format for TeXmacs (no information loss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mgs-recognizes? s)
  (and (string? s) (string-starts? s "(document (TeXmacs")))

(define-format mgs
  (:name "Mogan Scheme")
  (:suffix "mgs")
  (:must-recognize mgs-recognizes?))

(define (format-mgs-string str)
  ;; Parsing the string to an S-expression
  (define (parse-string s)
    (call-with-input-string s (lambda (port) (read port))))
  
  (define indent-cache (make-vector 50 #f))
  
  (define (get-indent n)
    (if (< n 50)
        (let ((cached (vector-ref indent-cache n)))
          (if cached
              cached
              (let ((spaces (make-string n #\space)))
                (vector-set! indent-cache n spaces)
                spaces)))
        (make-string n #\space)))
  
  (define (has-more-than-2? lst)
    (and (pair? lst) (pair? (cdr lst))))
  
  (define (every pred lst)
    (or (null? lst)
        (and (pred (car lst))
             (every pred (cdr lst)))))
  
  (define (simple-structure? expr)
    (define (simple-element? x)
      (or (not (pair? x)) 
          (every (lambda (e) (not (pair? e))) (cdr x))))
    (cond
      ((not (pair? expr)) #t)  
      (else (every simple-element? (cdr expr)))))  
  
  (define (expr-total-string-length expr)
    (cond
      ((null? expr) 0)
      ((string? expr) (string-length expr))
      ((pair? expr) (+ (expr-total-string-length (car expr))
                       (expr-total-string-length (cdr expr))))
      (else 0)))
  
  (define (format-expr expr indent)
    (cond
      ((null? expr) "()")
      ((string? expr) (format #f "~s" expr))
      ((number? expr) (number->string expr))
      ((symbol? expr) (symbol->string expr))
      ((pair? expr)
        (let* ((head (car expr))
              (body (cdr expr))
              (is-symbol-head? (symbol? head))
              (has-enough-elems? (has-more-than-2? body))
              (is-simple-struct? (simple-structure? expr))
              (simple-and-short? (and is-simple-struct?
                                     (<= (expr-total-string-length expr) 100)))              
              (should-format? (and is-symbol-head? 
                                  has-enough-elems?
                                  (not simple-and-short?))))
          (string-append
            "(" (format-expr head indent)
            (if (null? body) 
                ")" 
                (let ((new-indent (+ indent 2)))
                  (string-append
                    (if should-format? "\n" " ")
                    (format-body body should-format? new-indent)
                    ")"))))))))
  
  (define (format-body body should-format? indent)
    (string-join 
      (map 
        (lambda (rest-expr)
          (string-append
            (if should-format? (get-indent indent) "")
            (format-expr rest-expr indent)))
        body) 
      (if should-format? "\n" " ")))

  ;; Return the original string when an error occurs
  (catch #t
    (lambda () 
      (let ((expr (parse-string str)))
        (format-expr expr 0)))
    (lambda args str)))  

(define (texmacs->mgs t . opts)
  (if (null? opts) (set! opts '()))
  (let* ((options (if (list-1? opts) (car opts) opts))
         (do-format? (== (and (assoc-ref options "texmacs->mgs:formatted")
                             (get-preference "texmacs->mgs:formatted"))
                        "on"))
         (stm-str (texmacs->stm (herk-tree->utf8-tree t))))
    (if do-format?
        (format-mgs-string stm-str)
        stm-str)))

(define (mgs->texmacs text)
  (utf8-tree->herk-tree (stm->texmacs text)))

(define (mgs-snippet->texmacs text)
  (utf8-tree->herk-tree (stm-snippet->texmacs text)))

(converter texmacs-tree mgs-document
  (:function-with-options texmacs->mgs)
  (:option "texmacs->mgs:formatted" "off"))

(converter mgs-document texmacs-tree
  (:function mgs->texmacs))

(converter texmacs-tree mgs-snippet
  (:function-with-options texmacs->mgs)
  (:option "texmacs->mgs:formatted" "off"))

(converter mgs-snippet texmacs-tree
  (:function mgs-snippet->texmacs))
