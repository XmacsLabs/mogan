(texmacs-module (pdf-format))


(define (texmacs->pdf x . opts)
	(let* ((tem-dir (url-temp-dir))
				(tem-pdf (string->url (string-append (url->string tem-dir) "/tem.pdf")))
				(tem-tm (string->url (string-append (url->string tem-dir) "/tem.tm")))
				(tem-tm-out (open-output-file (url->string tem-tm)))
				(url-list (get-linked-file-paths x buffer-get)))
	(display* tem-dir "\n")
	(display (serialize-texmacs (replace-with-relative-path x buffer-get)) tem-tm-out)
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
	(if(make-attachments tem-pdf url-list tem-pdf)
		(display* "make-attachments return true\n")
		(display* "make-attachments return false\n"))
	(string-load tem-pdf)
	))

(define (pdf->texmacs x . opts)
	(let* ((tem-dir (url-temp-dir))
				(tem-pdf (string->url (string-append (url->string tem-dir) "/tem.pdf")))
				(tem-tm (string->url (string-append (url->string tem-dir) "/tem.tm")))
				(tem-pdf-out (open-output-file (url->string tem-pdf))))
	(display* tem-dir "\n")
	(display x tem-pdf-out)
	(close-output-port tem-pdf-out)
	(if (extract-attachments tem-pdf)
		()(display* "extract-attachments return false\n"))
	(set! tem-tm (url-relative tem-tm (get-main-tm tem-pdf)))

	(if (url-exists? tem-tm)
				(display* tem-tm " exist\n")
				(display* tem-tm " not exist\n"))
	(replace-with-relative-path (tree-import tem-tm (url-format tem-tm)) tem-pdf)
	))

(converter texmacs-tree pdf-document
  (:function texmacs->pdf))

(converter pdf-document texmacs-tree
  (:function pdf->texmacs))
