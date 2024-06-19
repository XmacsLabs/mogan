
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cork-test.scm
;; DESCRIPTION : Test suite for the cork encoding
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (lolly cork-test))

(define (regtest-alphanum)
  (begin
    (regression-test-group
      "cork" "utf8"
      cork->utf8 :none
      (test "locased alpha" "abcdefghijklmnopgrstuvwxyz" "abcdefghijklmnopgrstuvwxyz")
      (test "number" "0123456789" "0123456789")
    )
    (regression-test-group
      "utf8" "cork"
      utf8->cork :none
      (test "locased alpha" "abcdefghijklmnopgrstuvwxyz" "abcdefghijklmnopgrstuvwxyz")
      (test "number" "0123456789" "0123456789")
    )
  )
)

(define (regtest-angle)
  (begin
    (regression-test-group
      "cork" "utf8"
      cork->utf8 uint32->utf8
      (test "langle" "<langle>" #x27E8)
      (test "rangle" "<rangle>" #x27E9)
    )
    (regression-test-group
      "utf8" "cork"
      (lambda (x) (utf8->cork (uint32->utf8 x))) :none
      (test "langle" #x27E8 "<langle>")
      (test "rangle" #x27E9 "<rangle>")
    )
  )
)

(tm-define (regtest-cork)
  (let ((n (+ (regtest-alphanum)
              (regtest-angle))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of cork encoding ok\n")))
