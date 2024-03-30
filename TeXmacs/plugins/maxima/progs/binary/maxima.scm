
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : maxima.scm
;; DESCRIPTION : maxima Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary maxima)
  (:use (binary common)))

(define (maxima-binary-candidates)
  (cond ((os-macos?)
         (list "/opt/homebrew/bin/maxima"
               "/usr/local/bin/maxima"))
        ((os-win32?)
         (list ))
        (else
         (list "/usr/bin/maxima"))))

(tm-define (find-binary-maxima)
  (:synopsis "Find the url to the maxima binary, return (url-none) if not found")
  (find-binary (maxima-binary-candidates) "maxima"))

(tm-define (has-binary-maxima?)
  (not (url-none? (find-binary-maxima))))

(tm-define (version-binary-maxima)
  (check-stdout (string-append (url->system (find-binary-maxima)) " --version")))
