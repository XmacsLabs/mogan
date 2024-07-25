
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chez.scm
;; DESCRIPTION : chez Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary chez)
  (:use (binary common)))

(define (chez-binary-candidates)
  (cond ((os-macos?)
         (list "/opt/homebrew/bin/chez"
               "/usr/local/bin/chez"))
        ((os-win32?)
         (list "C:\\Program Files*\\Chez Scheme*\\bin\\a6nt\\scheme.exe"))
        (else
         (list "/usr/bin/chezscheme"))))

(tm-define (find-binary-chez)
  (:synopsis "Find the url to the Chez Scheme binary, return (url-none) if not found")
  (find-binary (chez-binary-candidates) "chezscheme"))

(tm-define (has-binary-chez?)
  (not (url-none? (find-binary-chez))))

(tm-define (version-binary-chez)
  (version-binary (find-binary-chez)))
