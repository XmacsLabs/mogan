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
        (liii ocr)))

(define plain-format-list    (list "LaTeX" "HTML" (translate "Plain text") "Markdown"))
(define math-format-list     (list "LaTeX" "MathML"))
(define program-format-list  (list (translate "Code")))
(define graphics-format-list (list "graphics"))
(define image-list           (list "OCR" (translate "Image and OCR") (translate "Only image")))


;; Helper functions (stateless, can remain at module level)
(define (get-name-list)
  (cond ((in-math?)            math-format-list)
        ((in-prog?)            program-format-list)
        ((in-graphics?)        graphics-format-list)
        ((is-clipboard-image?) image-list)
        (else                  plain-format-list)))

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
        ((== name (translate "Image and OCR")) "iao")
        ((== name (translate "Only image"))    "oi")
        ((== name "Graphics")                  "graphics")
        ((== name (translate "Code"))          "code")))

(define (get-tips fm)
  (cond ((== fm "md")        "Insert clipboard content as 'Markdown'")
        ((== fm "html")      "Insert clipboard content as 'HTML'")
        ((== fm "latex")     "Insert clipboard content as 'LaTeX'")
        ((== fm "verbatim")  "Insert clipboard content as 'plain text'")
        ((== fm "mathml")    "Insert clipboard content as 'MathML'")
        ((== fm "ocr")       "Insert the recognized LaTeX code into the document")
        ((== fm "iao")       "Insert the image and the recognized LaTeX code into the document")
        ((== fm "oi")        "Insert only the picture into the document")
        ((== fm "code")      "Insert clipboard content as 'code'")
        ((== fm "graphics")  "Insert clipboard content as 'graphics'")
        (else "Please select...")))

;; (convert-format-string-to-symbol (car (get-name-list)))
(define (init-choice)
  (with data (parse-texmacs-snippet (tree->string (tree-ref (clipboard-get "primary") 1)))
    (cond ((is-clipboard-image?) (convert-format-string-to-symbol (car image-list)))
          (else (convert-format-string-to-symbol (car (get-name-list)))))))

(tm-define (is-clipboard-image?)
  (with data (parse-texmacs-snippet (tree->string (tree-ref (clipboard-get "primary") 1)))
    (if (tree-is? (tree-ref data 0) 'image)
      #t
      #f)))

(tm-widget (clipboard-paste-from-widget cmd)
  (let* ((selected-format "verbatim")
         (tips "Please select..."))

    ;; Initialize selection on first display
    (invisible (set! selected-format (init-choice)))
    (invisible (set! tips (translate (get-tips selected-format))))

    (padded
      (vertical
        (horizontal
          (vertical
            (refreshable "current-mode"
              (bold (text "Mode"))
              (text (get-mode)))
            (glue #f #t 0 0))
          ///
          (refreshable "format-selection"
            (resize "150px" "150px"
              (bold (text "As: "))
              ===
              (scrollable
                (choice (begin 
                          (set! selected-format (convert-format-string-to-symbol (translate answer)))
                          (set! tips (translate (get-tips selected-format)))
                          (refresh-now "format-explanation"))
                        (get-name-list)
                        (car (get-name-list)))))))
        ===
        (refreshable "format-explanation"
          (bold (text "Tips"))
          (texmacs-output
            `(with "bg-color" "white"
               "font-base-size" "14"
               (document ,tips))
            '(style "generic")))))
      (bottom-buttons
        >> ("ok" (cmd selected-format)) // ("cancel" (cmd #f)))))

(tm-define (open-clipboard-paste-from-widget)
  (:interactive #t)
  (dialogue-window clipboard-paste-from-widget
    (lambda (fm)
      (when fm
        (cond ((== fm "md")       (open-url "https://liiistem.cn/"))
              ((== fm "ocr")      (ocr-paste))
              ((== fm "iao")      (ocr-and-image-paste))
              ((== fm "oi")       (kbd-paste))
              ((== fm "mathml")   (clipboard-paste-import "html" "primary"))
              (else               (clipboard-paste-import format "primary")))))
    "Paste Special"))

(tm-define (interactive-paste-special)
  (:interactive #t)
  (open-clipboard-paste-from-widget))
