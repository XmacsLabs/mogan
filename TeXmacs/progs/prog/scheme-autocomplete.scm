
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-autocomplete.scm
;; DESCRIPTION : Autocompletion in scheme sessions
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We provide very rudimentary autocompletion in scheme sessions using a prefix
;; tree of all publicly defined symbols. Things TO-DO are:
;;
;;  - Truly index all code with an indexer, instead of patching scheme's read.
;;  - Be aware of context: create new ptrees on the fly based on the
;;    environment. This needs online parsing and will be difficult. 
;;  - Suggest parameters in function calls.
;;  - Provide an alternative interface using (non-modal) popups or greyed out
;;    text after the cursor.
;;  - Add a layer decoupling from the specific scheme implementation for
;;    better portability.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog scheme-autocomplete)
  (:use (utils library ptrees) (prog glue-symbols)))

(define completions (make-ptree))
(define completions-cache (make-ahash-table))
(define-public scheme-completions-built? #f)
(define scheme-completions-initialized? #f) 

(define (clear-completions-cache)
  (set! completions-cache (make-ahash-table)))



(define core-symbols-loaded? #f)
(define full-load-scheduled? #f)

(define (load-core-symbols)
  (when (not core-symbols-loaded?)
    (display "Texmacs] Loading core symbols... ")
    (let ((start (texmacs-time))
          (core-symbols (all-glued-symbols)))
      (scheme-completions-add-list core-symbols)
      (display* (length core-symbols) " symbols in " 
                (- (texmacs-time) start) " msec\n")
      (set! core-symbols-loaded? #t))))

(define (load-full-symbols)
  (when (and core-symbols-loaded? (not scheme-completions-built?))
    (display "Texmacs] Background loading all symbols... ")
    (let ((start (texmacs-time)))
      (catch #t
        (lambda ()
          (let* ((tm-symbols (map (lambda (entry) (symbol->string (car entry)))
                                 tm-defined-table))
                 (all-symbols (append tm-symbols (all-used-symbols))))
            (scheme-completions-add-list all-symbols)
            (display* (length all-symbols) " total symbols in " 
                     (- (texmacs-time) start) " msec\n")
            (set! scheme-completions-built? #t)))
        (lambda (err) 
          (display* "Error loading symbols: " err "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (scheme-completions-add str)
  (set! completions (pt-add completions str))
  (clear-completions-cache))

(tm-define (scheme-completions-add-list lst)
  (let ((valid-strings (filter (lambda (s) 
                                (and (string? s) (> (string-length s) 0)))
                              lst)))
    (for-each (lambda (str) 
                (set! completions (pt-add completions str)))
              valid-strings))
  (clear-completions-cache))

(tm-define (scheme-completions-rebuild)
  (load-core-symbols)
  (when (not full-load-scheduled?)
    (set! full-load-scheduled? #t)
    (delayed 
      (:idle 500)
      (load-full-symbols))))

(tm-define (scheme-completions root)
  (:synopsis "Provide the completions for @root with caching")
  (unless scheme-completions-initialized?
    (set! scheme-completions-initialized? #t)
    (scheme-completions-rebuild))
  
  (let ((root-str (tmstring->string root)))
    (let ((cached (ahash-ref completions-cache root-str)))
      (if cached
          `(tuple ,root ,@(map string->tmstring cached))
          (let ((results (pt-words-below (pt-find completions root-str))))
            (when (<= (string-length root-str) 2)
              (ahash-set! completions-cache root-str results))
            `(tuple ,root ,@(map string->tmstring results)))))))

(tm-define (scheme-completions-dump)
  (pt-words-below (pt-find completions "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 兼容原始接口
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (all-used-symbols)
  (catch #t
    (lambda ()
      (map symbol->string 
           (append
            (map car tm-defined-table)
            (apply append 
                   (map (lambda (m)
                          (let ((e ((cdr m) '*exports*)))
                            (if (undefined? e) '() e))) 
                        *modules*)))))
    (lambda (err) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook for new-read. See init-texmacs.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%read-symbol-hook sym)
  (scheme-completions-add (symbol->string sym)))

(if developer-mode? (set! %new-read-hook %read-symbol-hook ))
