;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : docx.scm
;; DESCRIPTION : DOCX data format
;; COPYRIGHT   : (C) 2024  ATQlove
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data docx)
  (:use (binary pandoc)
        (texmacs texmacs tm-files)
        (network url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function to export TeXmacs document to DOCX using Pandoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (export-to-docx input-file output-file)
  (:synopsis "Export TeXmacs document to DOCX format using Pandoc")
  (let* ((base-dir (url-head input-file))
         (html-file-url (url-glue (url-head output-file) ".html")))
    ;; First, export the document to HTML
    (tm->html input-file html-file-url)
    ;; Then, use Pandoc to convert the HTML to DOCX
    (if (has-binary-pandoc?)
        (let ((cmd (string-append (url->string (find-binary-pandoc))
                                  " -f html -t docx "
                                  (url->string html-file-url)
                                  " -o "
                                  (url->string output-file))))
          (system cmd)
          ;; Delete the intermediate HTML file
        (system-remove html-file-url)) ;; Expected:$TEXMACS_PATH/tests/tm.html")
        (error "Pandoc binary not found"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper function to export tm/tmu to HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tm->html tm-file-url html-file-url)
  ; Step 1: load the tm file
  (display* "load-buffer: " tm-file-url "\n")
  (load-buffer tm-file-url)
  ; it will crash when loading the buffer in beamer style before this pull request
  (display* "buffer-loaded: " tm-file-url "\n")

  ; Step 2: cleaning for the html file url
  (when (url-exists? html-file-url) (system-remove html-file-url))

  ; Step 3: Export the buffer to the html file url
  (display* "export buffer to: " html-file-url)
  (export-buffer-main (current-buffer) html-file-url "html" ())
  #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converter for exporting TeXmacs files to DOCX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(converter texmacs-file docx-file
  (:require (has-binary-pandoc?))
  (:function-with-options export-to-docx))
