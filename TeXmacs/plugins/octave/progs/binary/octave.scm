
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : octave.scm
;; DESCRIPTION : octave Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary octave)
  (:use (binary common)))

(define (octave-binary-candidates)
  (cond ((os-macos?)
         (list ))
        ((os-win32?)
         (list ))
        (else
         (list "/usr/bin/octave-cli"))))

(tm-define (find-binary-octave)
  (:synopsis "Find the url to the octave binary, return (url-none) if not found")
  (find-binary (octave-binary-candidates) "octave-cli"))

(tm-define (has-binary-octave?)
  (not (url-none? (find-binary-octave))))

(tm-define (version-binary-octave)
  (with msg (check-stdout (string-append (url->system (find-binary-octave)) " --version"))
    (car (string-split msg #\newline))))
