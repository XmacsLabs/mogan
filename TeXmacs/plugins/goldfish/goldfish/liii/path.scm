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

(define-case-class path
  ((parts vector?)
   (type symbol? 'posix)
   (drive string? ""))

(define (%file?)
  (path-file? (%to-string)))

(define (%dir?)
  (path-dir? (%to-string)))

(define (%absolute?)
  (case type
    ((posix)
     (string-starts? (parts 0) "/"))
    ((windows)
     (not ($ drive :empty?)))
    (else
     (value-error
      (string-append "path%absolute?: unknown type" (symbol->string type))))))

(define (%relative)
  (not (%absolute?)))

(define (%exists?)
  (path-exists? (%to-string)))

(define (@from-vector v)
  (define (check-posix-parts parts)
    (when (vector-empty? parts)
      (value-error "make-path: parts must not be emtpy for posix path"))
    (let1 N (vector-length parts)
      (let loop ((i 0))
        (when (< i (- N 1))
              (when (string-null? (parts i))
                    (value-error "make-path: part of path must not be empty string, index" i))
              (loop (+ i 1))))
      (let loop ((i 1))
        (when (< i N)
              (when (string-index (parts i) #\/)
                    (value-error "make-path: non-first part of path must not contains /"))
              (loop (+ i 1))))))
  
  (cond ((vector? v)
         (begin
           (check-posix-parts v)
           (path v)))
        ((rich-vector :is-type-of v)
         (@from-vector (v :collect)))
        (else (type-error "input must be vector or rich-vector"))))

(chained-define (@from-string s)
  (cond ((and (or (os-linux?) (os-macos?))
              (string-starts? s "/"))
         (path :/ (@from-string ($ s :drop 1 :get))))
        ((and (os-windows?)
              (>= (string-length s) 2)
              (char=? (s 1) #\:))
         (path :of-drive (s 0)
               :/ (@from-string ($ s :drop 2 :get))))
        (else
         (let loop ((iter s))
           (cond ((or (string-null? iter) (string=? iter "."))
                  (@from-vector (vector ".")))
                 ((not (char=? (iter 0) (os-sep)))
                  (@from-vector ($ iter :split (string (os-sep)))))
                 (else
                  (loop ($ iter :drop 1 :get))))))))

(define (%to-string)
  (case type
    ((posix)
     (let1 s ($ parts :make-string "/")
        (if (string-starts? s "//")
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
         ((%this) :parts (vector-append parts (vector x))))
        ((path :is-type-of x)
         (cond ((x :absolute?)
                (value-error "path to append must not be absolute path: " (x :to-string)))
               ((x :equals (path (vector ".")))
                (%this))
               (else ((%this) :parts (vector-append parts (x 'parts))))))
        (else (type-error "only string?, path is allowed"))))

(chained-define (@of-drive ch)
  (when (not (char? ch))
    (type-error "path@of-drive must take char? as input"))

  (path #() 'windows ($ ch :to-upper :make-string)))

(chained-define (@root)
  (path #("/")))

(chained-define (@/ x)
  (if (path :is-type-of x)
      (path :root :/ x)
      (cond ((and (string-ends? x ":") (= (string-length x) 2))
             (path :of-drive (x 0)))
            ((string=? x "/")
             (path :root))
            (else (path (vector-append #("/") (vector x)))))))

(chained-define (@./ x)
  (path (vector x)))

(chained-define (@cwd)
  (@from-string (getcwd)))

(chained-define (@home)
  (cond ((or (os-linux?) (os-macos?))
         (path :from-string (getenv "HOME")))
        ((os-windows?)
         (path :of-drive ((getenv "HOMEDRIVE") 0)
               :/ (path :from-string (getenv "HOMEPATH"))))
        (else (value-error "path@home: unknown type"))))

)

) ; end of begin
) ; end of define-library

