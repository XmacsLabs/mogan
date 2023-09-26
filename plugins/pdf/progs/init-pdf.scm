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
          (tem-pdf (url-append tem-dir "/tem.pdf"))
          (tem-tm (url-append tem-dir "/tem.tm"))
          (tem-tm-out (open-output-file (url->string tem-tm)))
          (url-list (pdf-get-linked-file-paths x buffer-get)))
  (display (serialize-texmacs (pdf-replace-linked-path x buffer-get)) tem-tm-out)
  (close-output-port tem-tm-out)

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
  ;;attachment-list need be generated
  (if (pdf-make-attachments tem-pdf url-list tem-pdf)
    (display* "pdf-make-attachments return true\n")
    (display* "pdf-make-attachments return false\n"))
  (string-load tem-pdf)))

(define (pdf->texmacs x . opts)
  (let* ((tem-dir (url-temp-dir))
          (tem-pdf (url-append tem-dir "/tem.pdf"))
          (tem-tm (url-append tem-dir "/tem.tm"))
          (tem-pdf-out (open-output-file (url->string tem-pdf))))
    (display x tem-pdf-out)
    (close-output-port tem-pdf-out)
    (if (extract-attachments tem-pdf)
      (noop)
      (display* "extract-attachments return false\n"))
    (set! tem-tm (url-relative tem-tm (pdf-get-attached-main-tm tem-pdf)))

    (if (url-exists? tem-tm)
      (display* tem-tm " exist\n")
      (display* tem-tm " not exist\n"))
    (pdf-replace-linked-path (tree-import tem-tm (url-format tem-tm)) tem-pdf)))

(converter texmacs-tree pdf-document
  (:function texmacs->pdf))

(converter pdf-document texmacs-tree
  (:function pdf->texmacs))
