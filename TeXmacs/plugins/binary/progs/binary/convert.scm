
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : convert.scm
;; DESCRIPTION : Imagemagick Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary convert))

(define (convert-binary-candidates)
  (cond ((os-macos?)
         (list "/opt/homebrew/bin/convert"
               "/usr/local/bin/convert"))
        ((os-win?)
         (list ))
        (else
         (list "/usr/bin/convert"))))

(tm-define (find-binary-convert)
  (:synopsis "Find the url to the convert binary, return (url-none) if not found")
  (with u (or (list-find (convert-binary-candidates)
                (lambda (x) (url-exists? (url-resolve x "r"))))
              (url-resolve-in-path "convert"))
    (url-resolve u "r")))

(tm-define (has-binary-convert?)
  (not (url-none? (find-binary-convert))))

(tm-define (pdf-file->imagemagick-raster x opts)
  (let* ((dest (assoc-ref opts 'dest))
         (res (get-raster-resolution opts)))
    ;;(display (string-append "convert -density " res " " x " "  dest))
    (system-2 (string-append (url->system (find-binary-convert)) " -density " res) x dest)
    ;; NOTE: changing the resolution to 300 (the default) causes a problem
    ;; when converting TeXmacs documents to Html with formulas as images:
    ;; the formulas appear way too large...
    ;;(system-2 (string-append "convert ") x dest)
    (if (url-exists? dest) dest #f)))
