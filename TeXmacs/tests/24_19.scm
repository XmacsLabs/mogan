;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test_24_19.scm
;; DESCRIPTION : Test suite for utf8->html
;; COPYRIGHT   : (C) 2024  ATQlove
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(define (test-cjk-conversion)
  (check (utf8->html "试试中文是否正常显示") => "试试中文是否正常显示")
  (check (utf8->html "日本語の表示を確かめてみよう") => "日本語の表示を確かめてみよう")
  (check (utf8->html "한국어가 정상적으로 표시되는지 확인해보자") => "한국어가 정상적으로 표시되는지 확인해보자")
)

(define (test-non-cjk-conversion)
  (check (utf8->html "Try to see if the Chinese is displayed correctly") => "Try to see if the Chinese is displayed correctly")
  (check (utf8->html "Check the English output") => "Check the English output")
)

(define (test_24_19)
  (test-cjk-conversion)
  (test-non-cjk-conversion)
  (check-report))
