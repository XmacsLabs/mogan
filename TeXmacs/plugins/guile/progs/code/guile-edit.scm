
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : guile-edit.scm
;; DESCRIPTION : Editing guile programs
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code guile-edit)
  (:use (prog prog-edit)))

(texmacs-modes
  (in-guile% (== (get-env "prog-language") "guile"))
  (in-prog-guile% #t in-prog% in-guile%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (guile-bracket-open lbr rbr)
  (bracket-open lbr rbr "\\"))

(tm-define (guile-bracket-close lbr rbr)
  (bracket-close lbr rbr "\\"))

(tm-define (kbd-select-enlarge)
  (:require prog-select-brackets?)
  (:mode in-prog-guile?)
  (program-select-enlarge "(" ")"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-guile?)
  (select-brackets-after-movement "([{" ")]}" "\\"))

(kbd-map
  (:mode in-prog-guile?)
  ("{" (guile-bracket-open "{" "}" ))
  ("}" (guile-bracket-close "{" "}" ))
  ("(" (guile-bracket-open "(" ")" ))
  (")" (guile-bracket-close "(" ")" ))
  ("[" (guile-bracket-open "[" "]" ))
  ("]" (guile-bracket-close "[" "]" ))
  ("\"" (guile-bracket-open "\"" "\"" )))
