;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-pdf.scm
;; DESCRIPTION : setup pdf converters
;; COPYRIGHT   : (C) 2023  tangdouer
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(texmacs-module (pdf-format))


(define (texmacs->pdf x . opts)
  (let* ((tem-dir (url-temp-dir))
          (tem-pdf (url-append tem-dir "tem.pdf"))
          (tem-tm (url-append tem-dir "tem.tm"))
          (url-list (pdf-get-linked-file-paths x buffer-get)))
  (string-save (serialize-texmacs (pdf-replace-linked-path x buffer-get)) tem-tm)
  
  (let* ((cur (current-buffer))
          (buf (buffer-new)))
    (buffer-set-master buf cur)
    (switch-to-buffer buf)
    (load-buffer tem-tm)
    (set-drd cur)
    (dynamic-make-slides)
    (print-to-file tem-pdf)
    (switch-to-buffer cur)
    (buffer-close buf))
  (if (pdf-make-attachments tem-pdf url-list tem-pdf)
    (string-load tem-pdf)
    (begin
      (notify-now "Can not make attachments to pdf normally")
      (texmacs-error "pdf" "Can not make attachments to pdf normally")))))

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

(converter texmacs-tree pdf-document
  (:function texmacs->pdf))

(converter pdf-document texmacs-tree
  (:function pdf->texmacs))
