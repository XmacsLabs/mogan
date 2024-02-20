;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-data_pdf.scm
;; DESCRIPTION : PDF Data Plugin
;; COPYRIGHT   : (C) 2023  tangdouer
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (plugins data pdf))

(define (pdf->texmacs x . opts)
  (let* ((tem-dir (url-temp-dir))
          (tem-pdf (url-append tem-dir "tem.pdf"))
          (tem-tm (url-append tem-dir "tem.tm")))
    (string-save x tem-pdf)
    (if (extract-attachments tem-pdf)
        (pdf-replace-linked-path 
          (tree-import (url-relative tem-tm (pdf-get-attached-main-tm tem-pdf)) "texmacs") 
          tem-pdf)
        (begin
          (notify-now "Can not extract attachments from PDF")
          (texmacs-error "pdf" "Can not extract attachments from PDF")))))

(converter pdf-document texmacs-tree
  (:function pdf->texmacs))
