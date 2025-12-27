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

(define plain-format-list (list "LaTeX" "HTML" (translate "Plain text") "Markdown"))
(define math-format-list (list "LaTeX" "MathML" "OCR"))
(define program-format-list (list (translate "Code") "OCR"))
(define graphics-format-list (list "graphics"))


;; Helper functions (stateless, can remain at module level)
(define (get-name-list)
  (cond ((in-math?) math-format-list)
        ((in-prog?) program-format-list)
        ((in-graphics?) graphics-format-list)
        (else plain-format-list)))

(define (get-mode)
  (cond ((in-math?) "Math mode")
        ((in-prog?) "Program mode")
        ((in-graphics?) "Graphics mode")
        (else "text mode")))

(define (convert-format-string-to-symbol name)
  (cond ((== name "Markdown")               "md")
        ((== name "HTML")                   "html")
        ((== name "LaTeX")                  "latex")
        ((== name (translate "Plain text")) "verbatim")
        ((== name "MathML")                 "mathml")
        ((== name "OCR")                    "ocr")
        ((== name "Graphics")               "graphics")
        ((== name (translate "Code"))       "code")))

(define (get-tips fm)
  (cond ((== fm "md")        "Insert clipboard content as 'Markdown'")
        ((== fm "html")      "Insert clipboard content as 'HTML'")
        ((== fm "latex")     "Insert clipboard content as 'LaTeX'")
        ((== fm "verbatim")  "Insert clipboard content as 'plain text'")
        ((== fm "mathml")    "Insert clipboard content as 'MathML'")
        ((== fm "ocr")       "Recognize clipboard content as an 'image'")
        ((== fm "code")      "Insert clipboard content as 'code'")
        ((== fm "graphics")  "Insert clipboard content as 'graphics'")
        (else "Please select...")))

(define (init-choice)
  (convert-format-string-to-symbol (car (get-name-list))))

(tm-widget (clipboard-paste-from-widget cmd)
  (let* ((selected-format "verbatim")
         (on-format-change
           (lambda (name)
             (set! selected-format (convert-format-string-to-symbol name))
             (refresh-now "format-explanation"))))

    ;; Initialize selection on first display
    (invisible (set! selected-format (init-choice)))

    (padded
      (vertical
        (horizontal
          (vertical
            (refreshable "current-mode"
              (text (get-mode)))
            (glue #f #t 0 0))
          ///
          (refreshable "format-selection"
            (resize "150px" "150px"
              (scrollable
                (choice (on-format-change answer)
                        (get-name-list)
                        (car (get-name-list)))))))
        ===
        (refreshable "format-explanation"
          (texmacs-output
            `(with "bg-color" "white"
               "font-base-size" "14"
               (document ,(translate (get-tips selected-format))))
            '(style "generic")))))
      (bottom-buttons
        >> ("ok" (cmd selected-format)) // ("cancel" (cmd #f)))))

(tm-define (open-clipboard-paste-from-widget)
  (:interactive #t)
  (dialogue-window clipboard-paste-from-widget
    (lambda (format)
      (when format
        (cond ((== format "md")       (open-url "https://liiistem.cn/"))
              ((== format "ocr")      (ocr-paste))
              ((== format "mathml")   (clipboard-paste-import "html" "primary"))
              (else                   (clipboard-paste-import format "primary")))))
    "Paste Special"))

(tm-define (interactive-paste-special)
  (:interactive #t)
  (open-clipboard-paste-from-widget))
