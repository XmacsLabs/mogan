;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 71_41.scm
;; DESCRIPTION : Tests for sections
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./TeXmacs/progs/text/text-menu.scm")
(import (liii string)
        (liii check))

; override the subroutine
(define (is-top-level2 x)
  (string-starts? "chapter" x))

; override the subroutine
(define (is-current-tree2 t)
  (string-ends? "current" t))

(define (test)
  (check (filter-sections
           '() is-current-tree2 is-top-level2)
    => '())
  (check (filter-sections
           '("chapter 1" "section current" "chapter 2" "section 2.1")
           is-current-tree2 is-top-level2)
    => '("chapter 1" "section current" "chapter 2"))
  (check (filter-sections '("section 1" "section current" "chapter 2")
           is-current-tree2 is-top-level2)
    => '("section current" "chapter 2"))
  (check (filter-sections
          '("section 1" "chapter current" "section 1.1" "section 1.2"
            "chapter 2" "section 2.1")
           is-current-tree2 is-top-level2)
    => '("chapter current" "section 1.1" "section 1.2" "chapter 2")))

(define (test_71_41)
  (check-set-mode! 'report-failed)
  (test)
  (check-report))
