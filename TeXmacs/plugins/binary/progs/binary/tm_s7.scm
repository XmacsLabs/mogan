
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : aspell.scm
;; DESCRIPTION : aspell Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary tm_s7)
  (:use (binary common)))

(define (tm_s7-binary-candidates)
  (cond ((os-win32?)
         (list (string-append (url->system (get-texmacs-path)) "/plugins/s7/bin/tm_s7.exe")))
        (else
         (list (string-append (url->system (get-texmacs-path)) "/plugins/s7/bin/tm_s7")))))

(tm-define (find-binary-tm_s7)
  (:synopsis "Find the url to the tm_s7 binary, return (url-none) if not found")
  (find-binary (tm_s7-binary-candidates) "tm_s7"))

(tm-define (has-binary-tm_s7?)
  (not (url-none? (find-binary-tm_s7))))

(tm-define (version-binary-tm_s7)
  (version-binary (find-binary-tm_s7)))

