
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : url-test.scm
;; DESCRIPTION : Test suite for url
;; COPYRIGHT   : (C) 2023  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (network url-test)
  (:use (network url)))

(define (regtest-url-host)
  (regression-test-group
   "url" "host of the url"
   url-host :none
   (test "http 1" "http://mogan.app" "mogan.app")
   (test "http 2" "http://git.tmml.wiki/XmacsLabs/mogan" "git.tmml.wiki")
   (test "local file 1" "/tmp" "")))

(tm-define (regtest-url)
  (let ((n (+ (regtest-url-host))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of url: ok\n")))
