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



(define selected-format "verbatim")

(define tips "Please select...")

(define plain-format-list 
          (list "LaTeX" "HTML" (translate "Plain text") "Markdown"))

(define math-format-list
          (list "LaTeX" "MathML" "OCR"))

(define program-format-list
          (list (translate "Code") "OCR"))

(define graphics-format-list
          (list "graphics"))

(define (get-mode)
  (cond ((in-math?) "Math mode")
        ((in-prog?) "Program mode")
        ((in-graphics?) "Graphics mode")
        (else "text mode")))

(define (get-tips)
  (cond ((== selected-format "md")         (set! tips "Insert clipboard content as 'Markdown'"))
        ((== selected-format "html")       (set! tips "Insert clipboard content as 'HTML'"))
        ((== selected-format "latex")      (set! tips "Insert clipboard content as 'LaTeX'"))
        ((== selected-format "verbatim")   (set! tips "Insert clipboard content as 'plain text'"))
        ((== selected-format "mathml")     (set! tips "Insert clipboard content as 'MathML'"))
        ((== selected-format "ocr")        (set! tips "Recognize clipboard content as an 'image'"))
        ((== selected-format "code")       (set! tips "Insert clipboard content as 'code'"))
        (else (set! tips "Please select...")))
  (set! tips (translate tips))
  (refresh-now "format-explanation"))

(define (get-name-list)
  (cond ((in-math?) math-format-list)
        ((in-prog?) program-format-list)
        ((in-graphics?) graphics-format-list)
        (else plain-format-list)))

(define (convert-format-string-to-symbol)
  (cond ((== selected-format "Markdown")   (set! selected-format "md"))
        ((== selected-format "HTML")       (set! selected-format "html"))
        ((== selected-format "LaTeX")      (set! selected-format "latex"))
        ((== selected-format (translate "Plain text")) (set! selected-format "verbatim"))
        ((== selected-format "MathML")     (set! selected-format "mathml"))
        ((== selected-format "OCR")        (set! selected-format "ocr"))
        ((== selected-format (translate "Code"))       (set! selected-format "code"))))

(tm-widget (convertible-choice*)
  (invisible (begin 
               (set! selected-format (car (get-name-list)))
               (convert-format-string-to-symbol)
               (get-tips)))
    (resize "150px" "150px"
      (scrollable
        (choice (begin 
                  (set! selected-format answer)
                  (convert-format-string-to-symbol)
                  (get-tips))
                (get-name-list)
                (car (get-name-list))))))

(tm-widget (convertible-choice)
  (vertical
    (bold (text "As: "))
    ===
    (dynamic (convertible-choice*))))

(tm-widget (current-mode*)
  (text (get-mode)))

(tm-widget (current-mode)
  (vertical
    (bold (text "Mode"))
    (dynamic (current-mode*))))

(tm-widget (choice-text-widget*)
  (texmacs-output
    `(with "bg-color" "white"
       "font-base-size" "14"
       (document ,tips))
    '(style "generic")))

(tm-widget (choice-text-widget)
  (vertical
    (bold (text "Tips"))
    ===
    (dynamic (choice-text-widget*))))

(tm-widget (clipboard-paste-from-widget cmd)
  (padded
    (vertical
      (horizontal
        (vertical
          (refreshable "current-mode"
            (dynamic (current-mode)))
          (glue #f #t 0 0))
        ///
        (refreshable "format-selection"
          (dynamic (convertible-choice))))
        ===
        (refreshable "format-explanation"
          (dynamic (choice-text-widget))))
    (bottom-buttons
      >> ("ok" (cmd "ok")) // ("cancel" (cmd "cancel")))))

(tm-define (open-clipboard-paste-from-widget)
  (:interactive #t)
  (dialogue-window clipboard-paste-from-widget
    (lambda (args)
      (when (== args "ok")
            (cond 
              ((== selected-format "md") (open-url "https://liiistem.cn/"))
              ((== selected-format "ocr") (ocr-paste))
              ((== selected-format "mathml") (clipboard-paste-import "html" "primary"))
              (else (clipboard-paste-import selected-format "primary")))))
    "Paste Special"))

(tm-define (paste-special-in-math)
  (:interactive #t)
  (clipboard-paste-import "latex" "primary"))

(tm-define (interactive-paste-special)
  (:interactive #t)
  (open-clipboard-paste-from-widget))
