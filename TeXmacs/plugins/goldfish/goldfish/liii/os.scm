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

(define-library (liii os)
(export
  os-arch os-type os-windows? os-linux? os-macos? os-temp-dir
  os-sep pathsep
  os-call
  mkdir chdir rmdir getenv unsetenv getcwd listdir access getlogin getpid)
(import (scheme process-context)
        (liii base)
        (liii error)
        (liii string))
(begin

(define (os-arch)
  (g_os-arch))

(define (os-type)
  (g_os-type))

(define (os-linux?)
  (let ((name (os-type)))
    (and name (string=? name "Linux"))))

(define (os-macos?)
  (let ((name (os-type)))
    (and name (string=? name "Darwin"))))

(define (os-windows?)
  (let ((name (os-type)))
    (and name (string=? name "Windows"))))

(define (os-sep)
  (if (os-windows?)
    #\\
    #\/))

(define (pathsep)
  (if (os-windows?)
    #\;
    #\:))

(define (%check-dir-andthen path f)
  (cond ((not (file-exists? path))
         (file-not-found-error
           (string-append "No such file or directory: '" path "'")))
        ((not (g_isdir path))
         (not-a-directory-error
           (string-append "Not a directory: '" path "'")))
        (else (f path))))

(define (os-call command)
  (g_os-call command))

(define (system command)
  (g_system command))

(define (access path mode)
  (cond ((eq? mode 'F_OK) (g_access path 0))
        ((eq? mode 'X_OK) (g_access path 128))
        ((eq? mode 'W_OK) (g_access path 2))
        ((eq? mode 'R_OK) (g_access path 1))
        (else (error 'value-error "Allowed mode 'F_OK, 'X_OK,'W_OK, 'R_OK"))))

(define (getenv key)
  (get-environment-variable key))

(define (unsetenv key)
  (g_unsetenv key))

(define (os-temp-dir)
  (let1 temp-dir (g_os-temp-dir)
    (string-remove-suffix temp-dir (string (os-sep)))))

(define (mkdir path)
  (if (file-exists? path)
    (file-exists-error (string-append "File exists: '" path "'"))
    (g_mkdir path)))

(define (rmdir path)
  (%check-dir-andthen path delete-file))

(define (chdir path)
  (if (file-exists? path)
    (g_chdir path)
    (file-not-found-error (string-append "No such file or directory: '" path "'"))))

(define (listdir path)
  (%check-dir-andthen path g_listdir))

(define (getcwd)
  (g_getcwd))

(define (getlogin)
  (if (os-windows?)
      (getenv "USERNAME")
      (g_getlogin)))

(define (getpid)
  (g_getpid))

) ; end of begin
) ; end of define-library

