
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : s7-edit.scm
;; DESCRIPTION : Editing s7 programs
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code s7-edit)
  (:use (prog prog-edit)))

(texmacs-modes
  (in-s7% (== (get-env "prog-language") "s7"))
  (in-prog-s7% #t in-prog% in-s7%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (s7-bracket-open lbr rbr)
  (bracket-open lbr rbr "\\"))

(tm-define (s7-bracket-close lbr rbr)
  (bracket-close lbr rbr "\\"))

(tm-define (kbd-select-enlarge)
  (:require prog-select-brackets?)
  (:mode in-prog-s7?)
  (program-select-enlarge "(" ")"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-s7?)
  (select-brackets-after-movement "([{" ")]}" "\\"))