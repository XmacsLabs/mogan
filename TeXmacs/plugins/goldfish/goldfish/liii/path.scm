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

(define-library (liii path)
(export
  path-dir? path-file? path-exists?
  path-getsize path-read-text path-read-bytes path-write-text
  path
)
(import (liii base) (liii error) (liii vector) (liii string) (liii list)
        (liii os))
(begin

(define (path-dir? path)
  (g_isdir path))

(define (path-file? path)
  (g_isfile path))

(define (path-exists? path)
  (file-exists? path))

(define path-getsize
  (typed-lambda ((path string?))
    (if (not (file-exists? path))
      (file-not-found-error
        (string-append "No such file or directory: '" path "'"))
      (g_path-getsize path))))

(define path-read-text
  (typed-lambda ((path string?))
    (if (not (file-exists? path))
      (file-not-found-error
        (string-append "No such file or directory: '" path "'"))
      (g_path-read-text path))))

(define path-read-bytes
  (typed-lambda ((path string?))
    (if (not (file-exists? path))
      (file-not-found-error
        (string-append "No such file or directory: '" path "'"))
      (g_path-read-bytes path))))

(define path-write-text
  (typed-lambda ((path string?) (content string?))
    (g_path-write-text path content)))

(define-case-class path ()
  (define parts #("."))
  (define type 'posix)
  (define drive "")
  
  (define (%set-parts! v)
    (if (rich-vector :is-type-of v)
        (set! parts (v :collect))
        (set! parts v)))
  
  (define (%set-type! s)
    (set! type s))
  
  (define (%set-drive! s)
    (set! drive s))
  
  (define (%get-parts) parts)
  (define (%get-type) type)
  (define (%get-drive) drive)
  
  (define (%copy)
    (let1 p (path)
      (p :set-parts! parts)
      (p :set-type! type)
      (p :set-drive! drive)
      p))


(chained-define (@of-drive ch)
  (when (not (char? ch))
    (type-error "path@of-drive must take char? as input"))
    (let1 r (path)
        (r :set-type! 'windows)
        (r :set-drive! ($ ch :to-upper :make-string))
        (r :set-parts! #())
        r))

(chained-define (@root)
  (let1 r (path)
        (r :set-parts! #("/"))
        r))

(chained-define (@from-parts x) 
   (let1 r (path)
     (r :set-parts! x)
     r))

(chained-define (@/ x) 
   (if (path :is-type-of x)
       (path :root :/ x)
       (cond ((and (string-ends? x ":") (= (string-length x) 2))
               (path :of-drive (x 0)))
            
             ((string=? x "/") (path :root))
            
             (else
               (path :from-parts (vector-append (vector (string (os-sep))) (vector x)))))))

(chained-define (@apply s)
  (cond ((and (or (os-linux?) (os-macos?))
              (string-starts? s "/"))
         (path :/ (@apply ($ s :drop 1 :get))))
        ((and (os-windows?)
              (= (string-length s) 2)
              (char=? (s 1) #\:))
         (path :of-drive (s 0)))
        ((and (os-windows?) (>= (string-length s) 3)
              (char=? (s 1) #\:)
              (char=? (s 2) #\\))
         (path :of-drive (s 0)
               :/ (@apply ($ s :drop 3 :get))))
        (else
         (let loop ((iter s))
           (cond ((or (string-null? iter) (string=? iter "."))
                  (path))
                 
                 ((not (char=? (iter 0) (os-sep)))
                  (path :from-parts ($ iter :split (string (os-sep)))))
                 
                 (else
                  (loop ($ iter :drop 1 :get))))))))

(chained-define (@from-env name)
  (path (getenv name)))

(define (%file?)
  (path-file? (%to-string)))

(define (%dir?)
  (path-dir? (%to-string))) 

(define (%absolute?)
   (case type
     ((posix) (string-starts? (parts 0) "/"))
    
     ((windows) (not ($ drive :empty?)))
    
     (else
       (value-error
         (string-append "path%absolute?: unknown type" (symbol->string type))))))

(define (%relative)
  (not (%absolute?)))

(define (%exists?)
  (path-exists? (%to-string)))

(define (%to-string)
  (case type
    ((posix)
     (let1 s ($ parts :make-string (string (os-sep)))
        (if (and (> ($ s :length) 1) (string-starts? s (string (os-sep))))
            (string-drop s 1)
            s)))
    ((windows)
     (let1 s ($ parts :make-string "\\")
       (if (string-null? drive)
           s
           (string-append drive ":\\" s))))
    (else (value-error "path%to-string: unknown type" type))))

(define (%read-text)
  (path-read-text (%to-string)))

(typed-define (%write-text (content string?))
  (path-write-text (%to-string) content))

(define (%list)
  (box (listdir (%to-string))))

(define (%list-path)
  ((box (listdir (%to-string)))
   :map (lambda (x) ((%this) :/ x))))

(chained-define (%/ x)
  (cond ((string? x)
         (let1 new-path (%copy)
            (new-path :set-parts! (vector-append parts (vector x)))
            new-path))
        
        ((path :is-type-of x)
         (cond ((x :absolute?)
                (value-error "path to append must not be absolute path: " (x :to-string)))
               ((string=? (x :to-string) ".")
                (%this))
               (else (let ((new-path (%copy))
                           (x-parts (x :get-parts)))
                       (if (os-windows?)
                           (new-path :set-parts! x-parts)
                           (new-path :set-parts! (vector-append (vector (string (os-sep))) x-parts)))
                       new-path))))
        
        (else (type-error "only string?, path is allowed"))))

(chained-define (%parent)   
  (define (parts-drop-right parts x)
     (let1 path-vec ($ parts :drop-right x)
       (let1 new-path (%copy)
         (if (path-vec :empty?)
             (if (os-windows?)
                 (new-path :set-parts! #(""))
                 (new-path :set-parts! #(".")))
             (new-path :set-parts! (path-vec :append #(""))))
         new-path)))
                
  (cond
    ((or (equal? #("/") parts) (equal? #(".") parts))
     (%this))
    ((or (os-macos?) (os-linux?))
     (let1 last-part (($ parts) :take-right 1 :collect)
           (if (equal? last-part #(""))
               (parts-drop-right parts 2)
               (parts-drop-right parts 1))))
    ((os-windows?)
     (if ($ parts :empty?)
         (%this)
         (let1 last-part (($ parts) :take-right 1 :collect)
           (if (equal? last-part #(""))
               (parts-drop-right parts 2)
               (parts-drop-right parts 1)))))
    
    (else (??? "Unsupported platform"))))

(chained-define (@./ x)
  (let1 p (path x)
        (if (p :absolute?)
            (value-error "path@./: only accecpt relative path")
            (path x))))

(chained-define (@cwd)
  (path (getcwd)))

(chained-define (@home)
  (cond ((or (os-linux?) (os-macos?))
         (path (getenv "HOME")))
        ((os-windows?)
         (path :of-drive ((getenv "HOMEDRIVE") 0)
               :/ (path (getenv "HOMEPATH"))))
        (else (value-error "path@home: unknown type"))))

)

) ; end of begin
) ; end of define-library

