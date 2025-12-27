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
        ((== name (translate "Image and OCR")) "iao")
        ((== name (translate "Only image"))    "oi")
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
        ((== symbol "iao")      (translate "Image and OCR"))
        ((== symbol "oi")       (translate "Only image"))
        ((== symbol "code")     (translate "Code"))))

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

(define (get-clipboard-format)
  (let* ((data (clipboard-format "primary"))
         (fm (tree->string (tree-ref data 3))))
    (if (!= fm "")
      (convert-symbol-to-format-string fm)
      (convert-symbol-to-format-string "verbatim"))))

(define (init-choices l)
  (let* ((data (clipboard-format "primary"))
         (fm (tree->string (tree-ref data 3))))
    (if (!= fm "")
      (if (== fm "verbatim")
        l
        (let* ((name (convert-symbol-to-format-string fm)))
          (cons name (delete name l))))
      l)))

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
    (invisible (set! selected-format (car l)))
    (invisible (set! tips1 (translate (get-tips (convert-format-string-to-symbol selected-format)))))
    (invisible (set! tips2 (translate "You can try using Enter, Esc, keys~")))
    (resize "350px" "250px"
      (padded
        (vertical
          (horizontal
            (vertical
              (refreshable "clipboard-format"
                  (bold (text "From: "))
                  (text (get-clipboard-format))
              (glue #f #t 0 0)
                  (bold (text "Mode"))
                  (text (get-mode))
              (glue #f #t 0 0)
                  (bold (text "shortcut"))
                  (text (shortcut selected-format))))
            ///
            (refreshable "format-selection"
              (resize "150px" "150px"
                (bold (text "As: "))
                ===
                (scrollable
                  (choice (begin
                            (set! selected-format (convert-format-string-to-symbol (translate answer)))
                            (set! tips1 (translate (get-tips selected-format)))
                            (refresh-now "format-explanation")
                            (refresh-now "clipboard-format"))
                          l
                          (car l))))))
          ===
          (refreshable "format-explanation"
            (bold (text "Tips"))
            (texmacs-output
              `(with "bg-color" "white"
                "font-base-size" "14"
                (document ,tips1 ,tips2))
              '(style "generic"))))))
      (bottom-buttons
        >> ("ok" (cmd selected-format)) // ("cancel" (cmd #f)))))

(tm-define (open-clipboard-paste-from-widget)
  (:interactive #t)
  (dialogue-window clipboard-paste-from-widget
    (lambda (fm)
      (when fm
        (cond ((== fm "md")       (markdown-paste))
              ((== fm "ocr")      (ocr-paste))
              ((== fm "iao")      (ocr-and-image-paste))
              ((== fm "oi")       (kbd-paste))
              ((== fm "mathml")   (clipboard-paste-import "html" "primary"))
              (else               (clipboard-paste-import fm "primary")))))
    "Paste Special"))

(tm-define (interactive-paste-special)
  (:interactive #t)
  (open-clipboard-paste-from-widget))
