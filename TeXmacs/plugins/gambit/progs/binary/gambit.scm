
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gambit.scm
;; DESCRIPTION : gambit Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary gambit)
  (:use (binary common)))

(define (gambit-binary-candidates)
  (cond ((os-macos?)
         (list "/opt/homebrew/bin/gsi"
               "/usr/local/bin/gsi"))
        (else
         (list "/usr/bin/gsi"))))

(tm-define (find-binary-gambit)
  (:synopsis "Find the url to the gambit binary, return (url-none) if not found")
  (find-binary (gambit-binary-candidates) "gsi"))

(tm-define (has-binary-gambit?)
  (not (url-none? (find-binary-gambit))))

(tm-define (version-binary-gambit)
  (version-binary (find-binary-gambit)))
