
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : rsvg-convert.scm
;; DESCRIPTION : rsvg-convert Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary rsvg-convert))

(define (rsvg-convert-binary-candidates)
  (cond ((os-macos?)
         (list "/opt/homebrew/bin/rsvg-convert"
               "/usr/local/bin/rsvg-convert"))
        ((os-win32?)
         (list ))
        (else
         (list "/usr/bin/rsvg-convert"))))

(tm-define (find-binary-rsvg-convert)
  (:synopsis "Find the url to the rsvg-convert binary, return (url-none) if not found")
  (with u (or (list-find (rsvg-convert-binary-candidates)
                (lambda (x) (url-exists? (url-resolve x "r"))))
              (url-resolve-in-path "rsvg-convert"))
    (url-resolve u "r")))

(tm-define (has-binary-rsvg-convert?)
  (not (url-none? (find-binary-rsvg-convert))))

(tm-define (svg2png-by-rsvg-convert x opts)
  (let* ((dest (assoc-ref opts 'dest))
         (fm (url-format (url-concretize dest)))
         (res (get-raster-resolution opts))
         (cmd (url->system (find-binary-rsvg-convert))))
    (system-2 (string-append cmd " -f " fm " -d " res " -o ") dest x)
    (if (url-exists? dest) dest #f)))
