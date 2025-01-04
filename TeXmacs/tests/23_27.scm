(import (liii check)
        (liii os))

; Because std-arc-helper is using define, it is not using tm-define
; We have to load the module directly
;
; (texmacs-module (tests 23_27)
;   (:use (graphics graphics-markup)))
(load (string-append (getenv "TEXMACS_PATH") "/progs/graphics/graphics-markup.scm"))

(define (point x y)
  (stree->tree `(point ,x ,y)))

(define (test-std-arc-helper)
  (check (std-arc-helper (point "0" "0") (point "1" "0") (point "0" "1") >)
         => `(arc (point "1" "0") (point "-0.7071067811865475" "-0.7071067811865475") (point "0.0" "1.0"))))

(define (test_23_27)
  (test-std-arc-helper)
  (check-report)
  (if (check-failed?) (exit -1)))
