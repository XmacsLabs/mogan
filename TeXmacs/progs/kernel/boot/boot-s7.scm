
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot-s7.scm
;; DESCRIPTION : some global variables, public macros, on-entry, on-exit and
;;               initialization of the TeXmacs module system
;; COPYRIGHT   : (C) 2020  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define has-look-and-feel? (lambda (x) (== x "emacs")))

(define list? proper-list?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redirect standard output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define original-display display)
(define original-write write)

(define (display . l)
  "display one object on the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-display l)
      (tm-output (display-to-string (car l)))))

(define (write . l)
  "write an object to the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-write l)
      (tm-output (object->string (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(varlet (rootlet) 'temp-value #f)

;; setup the main modules
(with-let (rootlet)
  (define *texmacs-module* (rootlet))
  ;(define *current-module* (rootlet))
  (define *module-name* '(texmacs))
  (define *modules* (make-hash-table)))

(define *texmacs-user-module* (curlet))
(set! *current-module* *texmacs-user-module*)
(set! *module-name* '(texmacs-user))
(define *exports* '())

(set! (*modules* '(texmacs)) *texmacs-module*)
(set! (*modules* '(texmacs-user)) *texmacs-user-module*)

(define (current-module) *current-module*)

(define-macro (export . symbols)
    `(set! *exports* (append ',symbols *exports*)))

(define-macro (with-module module . body)
  `(let ((m ,module)) (with-let m
     (let-temporarily (((*texmacs-module* '*current-module*) (curlet)))
     ,@body))))

(define-macro (define-public head . body)
    `(begin
        (define ,head ,@body)
        (export ,(if (pair? head) (car head) head))))
        
        
(define-macro (provide-public head . body)
  (if (or (and (symbol? head) (not (defined? head)))
	  (and (pair? head) (symbol? (car head)) (not (defined? (car head)))))
      `(define-public ,head ,@body)
      '(noop)))

(define-macro (define-public-macro head . body)
    `(begin
	   (define-macro ,head ,@body)
	   (export ,(if (pair? head) (car head) head))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (module-available? module)
  (if (hash-table-ref *modules* module) #t #f))

(define (list->module module)
  (let* ((aux (lambda (s) (string-append "/" (symbol->string s))))
     (name* (apply string-append (map aux module)))
     (name (substring name* 1 (string-length name*)))
     (u (url-unix "$GUILE_LOAD_PATH" (string-append name ".scm")))
     ;; FIXME: should use %load-path instead of $GUILE_LOAD_PATH
     )
    (url-materialize u "r")))

(define (module-load module)
  (if (list? module)
      (let ((module-file (list->module module))
             (loaded (hash-table-ref *modules* module)))
    (when (not loaded)
        ;;(display "TeXmacs] Loading module ") (display module) (display "\n")
        (with-module (sublet (hash-table-ref *modules* '(texmacs-user))
                             '*exports* ()
                             '*module-file* module-file)
                (load *module-file* (curlet)))))))

(define (module-provide module)
  (if (not (module-available? module)) (module-load module)))

(define (resolve-module module)
  (module-provide module)
    (hash-table-ref *modules* module))

(define-macro (use-modules . modules)
  `(map (lambda (module)
    (let* ((m (resolve-module module))
           (ex (m '*exports*))
           (exx (map (lambda (entry) (if (member (car entry) ex) entry (values))) m))
           (en (apply inlet exx)))
        (varlet (*texmacs-module* '*current-module*) en)))
      ',modules))

(define-macro (import-from . modules)
  `(use-modules ,@modules))

(define-macro (re-export . symbols)
  `(export ,@symbols))

(define-macro (inherit-modules . which-list)
  (define (module-exports which)
    (let* ((m (resolve-module which)))
        (m '*exports*)))
  (let ((l (apply append (map module-exports which-list))))
    `(begin
       (use-modules ,@which-list)
       (re-export ,@l))))

(define-macro (texmacs-module name . options)
  (define (transform action)
    (cond ((not (pair? action)) (noop))
	  ((equal? (car action) :use) (cons 'use-modules (cdr action)))
	  ((equal? (car action) :inherit) (cons 'inherit-modules (cdr action)))
	  ((equal? (car action) :export)
	   (display "Warning] The option :export is no longer supported\n")
	   (display "       ] Please use tm-define instead\n"))
	  (else '(noop))))
  (let ((l (map transform options)))
    ;;(display "loading ") (display name) (display "\n")
    `(begin
        (define *module-name* ',name)
        (define *exports* ())
        (hash-table-set! *modules* ',name (current-module))
       ,@l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-entry and on-exit macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quit-TeXmacs-scheme) (noop))

(define-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))

(define-macro (define-library libname . body) ; |(lib name)| -> environment
  `(define ,(symbol (object->string libname))
     (with-let (sublet (unlet) 
			 (cons 'import import)
			 (cons '*export* ())
			 (cons 'export (define-macro (,(gensym) . names) 
					 `(set! *export* (append ',names *export*)))))
       ,@body
       (apply inlet
	      (map (lambda (entry)
		     (if (or (member (car entry) '(*export* export import))
			     (and (pair? *export*)
				  (not (member (car entry) *export*))))
			 (values)
			 entry))
		   (curlet))))))

(unless (defined? 'r7rs-import-library-filename)
  (define (r7rs-import-library-filename libs)    ; this turns (A B) into "A/B.scm", then loads it if needed
    (when (pair? libs)
      (let ((lib-filename (let loop ((lib (if (memq (caar libs) '(only except prefix rename))
						(cadar libs)
						(car libs)))
				       (name ""))
			      (set! name (string-append name (symbol->string (car lib))))
			      (if (null? (cdr lib))
				  (string-append name ".scm")
				  (begin
				    (set! name (string-append name "/")) ; this follows Guile, Chibi, and Racket
				    (loop (cdr lib) name))))))
        (unless (member lib-filename (*s7* 'file-names))
	      (load lib-filename)))
      (r7rs-import-library-filename (cdr libs)))))
  
(define-macro (import . libs)
  `(begin
     (r7rs-import-library-filename ',libs)
     (varlet (curlet)
       ,@(map (lambda (lib)
		(case (car lib)
		  ((only) 
		   `((lambda (e names)
		       (apply inlet
			      (map (lambda (name)
				     (cons name (e name)))
				   names)))
		     (symbol->value (symbol (object->string (cadr ',lib))))
		     (cddr ',lib)))
		  
		  ((except)
		   `((lambda (e names)
		       (apply inlet
			      (map (lambda (entry)
				     (if (member (car entry) names)
					 (values)
					 entry))
				   e)))
		     (symbol->value (symbol (object->string (cadr ',lib))))
		     (cddr ',lib)))
		  
		  ((prefix)
		   `((lambda (e prefx)
		       (apply inlet
			      (map (lambda (entry)
				     (cons (string->symbol 
					    (string-append (symbol->string prefx) 
							   (symbol->string (car entry)))) 
					   (cdr entry)))
				   e)))
		     (symbol->value (symbol (object->string (cadr ',lib))))
		     (caddr ',lib)))
		  
		  ((rename)
		   `((lambda (e names)
		       (apply inlet
			      (map (lambda (entry)
				     (let ((info (assoc (car entry) names)))
				       (if info
					   (cons (cadr info) (cdr entry))
					   entry))) ; I assume the un-renamed ones are included
				   e)))
		     (symbol->value (symbol (object->string (cadr ',lib))))
		     (cddr ',lib)))

		  (else
		   `(let ((sym (symbol (object->string ',lib))))
		      (if (not (defined? sym))
			  (format () "~A not loaded~%" sym)
			  (symbol->value sym))))))
	      libs))))

(set! *load-path*
  (cons (string-append (url->system (get-texmacs-path)) "/plugins/s7/")
        *load-path*))
