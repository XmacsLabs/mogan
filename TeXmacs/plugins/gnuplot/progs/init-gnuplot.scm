
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-gnuplot.scm
;; DESCRIPTION : Initialize Goldfish plugin
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (binary gnuplot)
             (binary goldfish))

(lazy-format (data r7rs) r7rs)

(define (gnuplot-serialize lan t)
  (let* ((u (pre-serialize lan t))
         (s (texmacs->code (stree->tree u) "utf-8")))
    (string-append s "\n<EOF>\n")))

(define (gnuplot-launcher)
  (string-append
    (string-quote (url->system (find-binary-goldfish)))
    " "
    (string-quote
      (string-append (url->system (get-texmacs-path))
                     "/plugins/gnuplot/goldfish/tm-gnuplot.scm"))))

(plugin-configure gnuplot
  (:require (and (has-binary-goldfish?) (has-binary-gnuplot?)))
  (:launch ,(gnuplot-launcher))
  (:serializer ,gnuplot-serialize)
  (:session "Gnuplot"))

