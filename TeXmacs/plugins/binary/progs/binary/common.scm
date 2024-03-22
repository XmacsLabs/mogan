
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : common.scm
;; DESCRIPTION : routines for Binary plugins
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary common))

(tm-define (find-binary candidates default)
  (or
    (with path (get-preference (string-append "plugin:binary:" default))
      (and (!= path "default")
           (with u (url-resolve path "r")
             (if (and (url-exists? u) (url-regular? u)) u #f))))
    (with u (list-find candidates (lambda (x) (url-exists? (url-resolve x "r"))))
      (and u (url-resolve u "r")))
    (with u (url-resolve-in-path (if (os-win32?) (string-append default ".exe") default))
      (if (and (os-win32?) (url-descends? u (system->url "C:\\Windows\\System32")))
        (url-none)
        u))))
