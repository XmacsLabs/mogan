;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-test.scm
;; DESCRIPTION : test suite for graphics-group
;; COPYRIGHT   : (C) 2023 Jia Zhang
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-group-test)
  (:use (graphics graphics-group)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regtest routines for get-paste-offset-by-pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-get-paste-offset-by-pos)
  (regression-test-group
    "graphics group" "get-paste-offset-by-pos"
    values values
    (test "Left Up" 
      (get-paste-offset-by-pos '(carc (point "-1" "1") (point "-2" "2") (point "-3" "3")))
      (list (+ paste-offset-constant) (- paste-offset-constant)))
    (test "Left Down" 
      (get-paste-offset-by-pos '(carc (point "-1" "-1") (point "-2" "-2") (point "-3" "-3")))
      (list (+ paste-offset-constant) (+ paste-offset-constant)))
    (test "Right Up" 
      (get-paste-offset-by-pos '(carc (point "1" "1") (point "2" "2") (point "3" "3")))
      (list (- paste-offset-constant) (- paste-offset-constant)))
    (test "Right Down" 
      (get-paste-offset-by-pos '(carc (point "1" "-1") (point "2" "-2") (point "3" "-3")))
      (list (- paste-offset-constant) (+ paste-offset-constant)))
    (test "Coordinate Origin" 
      (get-paste-offset-by-pos '(carc (point "-2" "0") (point "-1" "2") (point "3" "-2")))
      (list (- paste-offset-constant) (- paste-offset-constant)))   
      ))

(tm-define (regtest-graphics-group)
  (let ((n (regtest-get-paste-offset-by-pos)))
    (display* "Total: " (number->string n) "tests.\n")
    (display "Test suite of regtest-graphics-group: ok\n")))
