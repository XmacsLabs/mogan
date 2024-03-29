
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

(define (regtest-zotero-url)
  (regression-test-group
   "url" "(url-or? u)"
   url-or? :none
   (test "case 1" "zotero://a/b/c" #f))
  (regression-test-group
   "url" "(url-complete u flags)"
   (lambda (x)
     (== (url-complete "zotero://a/b/c" x)
         (string->url "zotero://a/b/c")))
   :none
   (test "r" "r" #t)
   (test "df" "df" #t)
   (test "rf" "rf" #t)))

(define (regtest-url-host)
  (regression-test-group
   "url" "host of the url"
   url-host :none
   (test "http 1" "http://mogan.app" "mogan.app")
   (test "http 2" "http://git.tmml.wiki/XmacsLabs/mogan" "git.tmml.wiki")
   (test "local file 1" "/tmp" "")))

(tm-define (regtest-url)
  (let ((n (+ (regtest-url-host)
              (regtest-zotero-url))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of url: ok\n")))
