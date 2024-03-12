
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-kbd-utf8.scm
;; DESCRIPTION : keystrokes in text mode
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-kbd-utf8)
  (:use (generic generic-kbd)
	(utils edit auto-close)
	(text text-edit)))

(kbd-map
  (:mode in-text?)
  ("<" "<less>")
  (">" "<gtr>")
  ("< var" "<#2039>")
  ("> var" "<#203A>"))
