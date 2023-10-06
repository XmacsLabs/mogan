
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : embedded-edit-test.scm
;; DESCRIPTION : Test suite for embedded-edit.scm
;; COPYRIGHT   : (C) 2023  jingkaimori
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic embedded-edit-test)
  (:use (generic embedded-edit)))

(define (regtest-embedded-suffix)
  (regression-test-group
   "embedded edit, test tree with cork" "embedded-edit"
   embedded-suffix :none
   (test "cork encoding" '(image (tuple (raw-data "dummy") "<#672A><#547D><#540D><#7ED8><#56FE>.svg") "100pt" "80pt" "" "")
	 "svg")
   (test "ascii" '(image (tuple (raw-data "dummy") "filename.png") "100pt" "80pt" "" "")
	 "png")
   (test "extension only" '(image (tuple (raw-data "dummy") "png") "100pt" "80pt" "" "")
	 "png")))

(tm-define (regtest-embedded-edit)
  (let ((n (+ (regtest-embedded-suffix))))
        
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tmhtml: ok\n")))