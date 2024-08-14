;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 19_8.scm
;; DESCRIPTION : Tests for translation
;; COPYRIGHT   : (C) 2024  ATQlove
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(check-set-mode! 'report-failed)

(check (translate-from-to "theorem" "english" "french") => (utf8->cork "théorème"))
(check (translate-from-to "theorem" "english" "chinese") => (utf8->cork "定理"))

(define (test_19_8)
  (check-report))
