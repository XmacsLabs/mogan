
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : guile.scm
;; DESCRIPTION : guile Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary guile)
  (:use (binary common)))

(define (guile-binary-candidates)
  (cond ((os-macos?)
         (list "/opt/homebrew/bin/guile"
               "/usr/local/bin/guile"))
        (else
         (list "/usr/bin/guile"))))

(tm-define (find-binary-guile)
  (:synopsis "Find the url to the guile binary, return (url-none) if not found")
  (find-binary (guile-binary-candidates) "guile"))

(tm-define (has-binary-guile?)
  (not (url-none? (find-binary-guile))))

(tm-define (version-binary-guile)
  (version-binary (find-binary-guile)))
