
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : inkscape.scm
;; DESCRIPTION : inkscape Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary inkscape))

(define (inkscape-binary-candidates)
  (cond ((os-macos?)
         (list "/Applications/Inkscape.app/Contents/MacOS/inkscape"))
        ((os-win32?)
         (list "C:\\Program Files*\\Inkscape\\bin\\inkscape.exe"))
        (else
         (list "/usr/bin/inkscape"))))

(tm-define (find-binary-inkscape)
  (:synopsis "Find the url to the inkscape binary, return (url-none) if not found")
  (with u (or (list-find (inkscape-binary-candidates)
                (lambda (x) (url-exists? (url-resolve x "r"))))
              (url-resolve-in-path "inkscape"))
    (url-resolve u "r")))

(tm-define (has-binary-inkscape?)
  (not (url-none? (find-binary-inkscape))))
