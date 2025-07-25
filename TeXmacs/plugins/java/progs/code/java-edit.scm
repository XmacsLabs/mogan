
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : java-edit.scm
;; DESCRIPTION : editing Java programs
;; COPYRIGHT   : (C) 2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code java-edit)
  (:use (prog prog-edit)))

(tm-define (get-tabstop)
  (:mode in-prog-java?)
  4)

(tm-define (program-compute-indentation doc row col)
  (:mode in-prog-java?)
  (get-tabstop))

(tm-define (kbd-paste)
  (:mode in-prog-java?)
  (clipboard-paste-import "java" "primary"))

(kbd-map
  (:mode in-prog-java?)
  ("p s v m var"
   (begin
    (insert "public static void main(String[] args) {}")
    (go-to-previous)
    (insert-return)
    (insert-raw-return)
    (go-to-previous))))
