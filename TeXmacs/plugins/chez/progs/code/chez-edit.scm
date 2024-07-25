
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chez-edit.scm
;; DESCRIPTION : Editing chez programs
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code chez-edit)
  (:use (prog prog-edit)))

(texmacs-modes
  (in-chez% (== (get-env "prog-language") "chez"))
  (in-prog-chez% #t in-prog% in-chez%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (chez-bracket-open lbr rbr)
  (bracket-open lbr rbr "\\"))

(tm-define (chez-bracket-close lbr rbr)
  (bracket-close lbr rbr "\\"))

(tm-define (kbd-select-enlarge)
  (:require prog-select-brackets?)
  (:mode in-prog-chez?)
  (program-select-enlarge "(" ")"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-chez?)
  (select-brackets-after-movement "([{" ")]}" "\\"))

(kbd-map
  (:mode in-prog-chez?)
  ("{" (chez-bracket-open "{" "}" ))
  ("}" (chez-bracket-close "{" "}" ))
  ("(" (chez-bracket-open "(" ")" ))
  (")" (chez-bracket-close "(" ")" ))
  ("[" (chez-bracket-open "[" "]" ))
  ("]" (chez-bracket-close "[" "]" ))
  ("\"" (chez-bracket-open "\"" "\"" )))
