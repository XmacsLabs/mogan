
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gs.scm
;; DESCRIPTION : Ghostscript Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary gs))

(define (gs-binary-candidates)
  (cond ((os-macos?)
         (list "/opt/homebrew/Cellar/ghostscript/1*/bin/gs"
               "/usr/local/Cellar/ghostscript/1*/bin/gs"))
        ((os-win?)
         (list ))
        (else
         (list "/usr/bin/gs"))))

(tm-define (find-binary-gs)
  (:synopsis "Find the url to the gs binary, return (url-none) if not found")
  (with u (or (list-find (gs-binary-candidates)
                (lambda (x) (url-exists? (url-resolve x "r"))))
              (url-resolve-in-path "gs"))
    (url-resolve u "r")))

(tm-define (has-binary-gs?)
  (not (url-none? (find-binary-gs))))

(tm-define (gs-convert x opts)
  ;; many options for pdf->ps/eps see http://tex.stackexchange.com/a/20884
  ;; this one does a better rendering than pdf2ps (also based on gs):
  (let* ((dest (assoc-ref opts 'dest))
         (gs (url->system (gs-binary))))
    (system-2 (string-append gs " -q -dNOCACHE -dUseCropBox -dNOPAUSE -dBATCH -dSAFER -sDEVICE=eps2write -sOutputFile=") dest x))
  ;; problem: 
  ;; eps2write available starting with gs  9.14 (2014-03-26)
  ;; epswrite removed in gs 9.16 (2015-03-30)
  )

(tm-define (pdf-file->gs-raster x opts)
  (let* ((dest (assoc-ref opts 'dest))
         (res (get-raster-resolution opts))
         (gs (url->system (gs-binary))))
    (evaluate-system (list gs "-dBATCH" "-dNOPAUSE" "-dQUIET" "-dSAFER"
                           "-dNOPROMPT" "-sDEVICE=pngalpha"
                           (string-append "-r" res)
                           (string-append "-sOutputFile="
                                          (url-concretize dest))
                           (url-concretize x)) '() '() '(1 2))
    (if (url-exists? dest) dest #f)))
