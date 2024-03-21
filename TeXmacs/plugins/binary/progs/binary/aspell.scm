
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

(texmacs-module (binary aspell))

(define (aspell-binary-candidates)
  (cond ((os-macos?)
         (list "/opt/homebrew/bin/aspell"
               "/usr/local/bin/aspell"))
        ((os-win32?)
         (list "$USERPROFILE\\scoop\\apps\\aspell\\current\\bin\\aspell.exe"))
        (else
         (list "/usr/bin/aspell"))))

(tm-define (find-binary-aspell)
  (:synopsis "Find the url to the aspell binary, return (url-none) if not found")
  (with u (or (list-find (aspell-binary-candidates)
                (lambda (x) (url-exists? (url-resolve x "r"))))
              (url-resolve-in-path "aspell"))
    (url-resolve u "r")))

(tm-define (has-binary-aspell?)
  (not (url-none? (find-binary-aspell))))

