
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : python3.scm
;; DESCRIPTION : python3 Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary python3))

(define (python3-binary-candidates)
  (cond ((os-macos?)
         (list "/usr/bin/python3"
               "/opt/homebrew/bin/python3"
               "/usr/local/bin/python3"))
        ((os-win32?)
         (list "$LOCALAPPDATA/Programs/Python/Python3*/python.exe"))
        (else
         (list "/usr/bin/python3"))))

(tm-define (find-binary-python3)
  (:synopsis "Find the url to the python3 binary, return (url-none) if not found")
  (with u (or (list-find (python3-binary-candidates)
                (lambda (x) (url-exists? (url-resolve x "r"))))
              (url-resolve-in-path "python3"))
    (url-resolve u "r")))

(tm-define (has-binary-python3?)
  (not (url-none? (find-binary-python3))))
