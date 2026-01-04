;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : paste-widgets.scm
;; DESCRIPTION : widgets for paste special
;; COPYRIGHT   : (C) 2025  Mogan STEM authors
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic paste-widget)
  (:use (generic generic-edit)
        (utils edit selections)
        (liii ocr)
        (kernel texmacs tm-convert)))
(import (only (liii list) delete))

;; Helper functions (stateless, can remain at module level)


(define (get-mode)
  (cond ((in-math?)     "Math mode")
        ((in-prog?)     "Program mode")
        ((in-graphics?) "Graphics mode")
        (else           "text mode")))

(define (convert-format-string-to-symbol name)
  (cond ((== name "Markdown")                  "md")
        ((== name "HTML")                      "html")
        ((== name "LaTeX")                     "latex")
        ((== name (translate "Plain text"))    "verbatim")
        ((== name "MathML")                    "mathml")
        ((== name "OCR")                       "ocr")
        ((== name (translate "Image and OCR")) "image_and_ocr")
        ((== name (translate "Only image"))    "image")
        ((== name "Graphics")                  "graphics")
        ((== name (translate "Code"))          "code")))

(define (convert-symbol-to-format-string symbol)
  (cond ((== symbol "md")       "Markdown")
        ((== symbol "html")     "HTML")
        ((== symbol "latex")    "LaTeX")
        ((== symbol "mathml")   "MathML")
        ((== symbol "ocr")      "OCR")
        ((== symbol "graphics") "Graphics")
        ((== symbol "verbatim") (translate "Plain text"))
        ((== symbol "image_and_ocr")      (translate "Image and OCR"))
        ((== symbol "image")       (translate "Only image"))
        ((== symbol "code")     (translate "Code"))))

(define (get-tips fm)
  (cond ((== fm "md")        "Insert clipboard content as 'Markdown'")
        ((== fm "html")      "Insert clipboard content as 'HTML'")
        ((== fm "latex")     "Insert clipboard content as 'LaTeX'")
        ((== fm "verbatim")  "Insert clipboard content as 'plain text'")
        ((== fm "mathml")    "Insert clipboard content as 'MathML'")
        ((== fm "ocr")       "Insert the recognized LaTeX code into the document")
        ((== fm "image_and_ocr")       "Insert the image and the recognized LaTeX code into the document")
        ((== fm "image")        "Insert only the picture into the document")
        ((== fm "code")      "Insert clipboard content as 'code'")
        ((== fm "graphics")  "Insert clipboard content as 'graphics'")
        (else "Please select...")))

(define (get-clipboard-format)
  (convert-symbol-to-format-string (clipboard-format "primary")))

(define (init-choices l)
  (let* ((fm (clipboard-format "primary")))
    (if (or (== fm "") (== fm "verbatim") (== fm "image"))
        l
        (let* ((name (convert-symbol-to-format-string fm)))
          (cons name (delete name l))))))

(define (shortcut fm)
  (cond ((and (in-math?) (== fm "latex")) "Ctrl+Shift+v")
        ((== fm "verbatim") "Ctrl+Shift+v")
        ((is-clipboard-image?) "Ctrl+Shift+v")
        (else "none")))

(tm-define (is-clipboard-image?)
  (with data (parse-texmacs-snippet (tree->string (tree-ref (clipboard-get "primary") 1)))
    (if (tree-is? (tree-ref data 0) 'image)
      #t
      #f)))

(tm-widget (clipboard-paste-from-widget cmd)
  (let* ((selected-format "verbatim")
         (tips1 "Please select...")
         (tips2 "")
         (tips3 (string-append (translate "shortcut") ": " (translate "none")))
         (plain-format-list    (list "Markdown" "LaTeX" "HTML" (translate "Plain text")))
         (math-format-list     (list "LaTeX" "MathML"))
         (program-format-list  (list (translate "Code")))
         (graphics-format-list (list "graphics"))
         (image-list           (list "OCR" (translate "Image and OCR") (translate "Only image")))
         (name-list (cond ((in-math?)            math-format-list)
                          ((in-prog?)            program-format-list)
                          ((in-graphics?)        graphics-format-list)
                          ((is-clipboard-image?) image-list)
                          (else                  plain-format-list)))
         (l (init-choices name-list)))

    ;; Initialize selection on first display
    (invisible (set! selected-format (convert-format-string-to-symbol (car l))))
    (invisible (set! tips1 (translate (get-tips selected-format))))
    (invisible (set! tips2 (translate "ENTER to confirm, ESC to cancel")))
    (resize "320px" "270px"
      (padded
        (vertical
          (horizontal
            (vertical
                (bold (text "From: "))
                (text (get-clipboard-format))
                (glue #f #t 0 0)
                (bold (text "Mode"))
                (text (get-mode))
                (glue #f #t 0 0))
            ///
            (refreshable "format-selection"
              (resize "200px" "190px"
                (bold (text "As: "))
                ===
                (scrollable
                  (choice (begin
                            (set! selected-format (convert-format-string-to-symbol (translate answer)))
                            (set! tips1 (translate (get-tips selected-format)))
                            (set! tips3 (string-append (translate "shortcut") ": " (shortcut selected-format)))
                            (refresh-now "format-explanation")
                            (refresh-now "paste-shortcut"))
                          l
                          (car l))))))
          ===
          (refreshable "format-explanation"
            (bold (text "Tips"))
            (resize "320px" "90px"
            (texmacs-output
              `(with "bg-color" "white"
                "font-base-size" "18"
                (document ,tips1 ,tips3 ,tips2))
              '(style "generic")))))))
      (bottom-buttons
        >> ("ok" (cmd selected-format)) // ("cancel" (cmd #f)))))

(define (paste-as-html)
  (with source-format (clipboard-format "primary")
    (if (string=? source-format "html")
        (kbd-paste)
        (clipboard-paste-import "html" "primary"))))

(tm-define (open-clipboard-paste-from-widget)
  (:interactive #t)
  
  (define callback
    (lambda (fm)
      (when fm
        (cond ((== fm "md")       (markdown-paste))
              ((== fm "ocr")      (ocr-paste))
              ((== fm "image_and_ocr")      (ocr-and-image-paste))
              ((== fm "image")    (kbd-paste))
              ((== fm "mathml")   (clipboard-paste-import "html" "primary"))
              ((== fm "html")     (paste-as-html))
              (else               (clipboard-paste-import fm "primary"))))))

  (dialogue-window clipboard-paste-from-widget callback "Paste Special"))

(tm-define (interactive-paste-special)
  (:interactive #t)
  (open-clipboard-paste-from-widget))
