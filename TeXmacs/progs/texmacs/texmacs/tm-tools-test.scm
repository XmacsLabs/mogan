
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-tools-test.scm
;; DESCRIPTION : Test suite for tm-tools
;; COPYRIGHT   : (C) 2023  jingkaimori
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-tools-test)
  (:use (texmacs texmacs tm-tools)))

(define (utf8-string-length-test)
  (regression-test-group
    "unicode character count" "utf8-string-length-test"
    utf8-string-length :none
    (test "empty length" "" 0)
    (test "ascii length (one byte in utf-8)" "Hello world!" 12)
    (test "russian length (two byte in utf-8)" "Всем привет!" 12)
    (test "kanji length (three byte in utf-8)" "你好世界！" 5)
    (test "fraktur length (four byte in utf-8)" "𝕳𝖊𝖑𝖑𝖔 𝖜𝖔𝖗𝖑𝖉!" 12)
    (test "hybrid length" "Hello world!Всем привет!你好世界！𝕳𝖊𝖑𝖑𝖔 𝖜𝖔𝖗𝖑𝖉!" 41)))

(tm-define (regtest-tm-tools)
  (let ((n (+ (utf8-string-length-test)
              0)))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-tools: ok\n")))