
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
             (binary goldfish)
             (binary gs))

(lazy-format (data r7rs) r7rs)

(define (gnuplot-serialize lan t)
  (let* ((u (pre-serialize lan t))
         (s (texmacs->code (stree->tree u) "utf-8")))
    (string-append s "\n<EOF>\n")))

(define (gen-launcher image-format)
  (string-append
    (string-quote (url->system (find-binary-goldfish)))
    " "
    (string-quote
      (string-append (url->system (get-texmacs-path))
                     "/plugins/gnuplot/goldfish/tm-gnuplot.scm"))
    " "
    (string-quote (url->system (find-binary-gnuplot)))
    " "
    image-format))

(define (gnuplot-launchers)
  (cons (list :launch (gen-launcher "png"))
    (list
      ;(when (has-binary-gs?)
      ;  (list :launch "eps" (gen-launcher "eps")))
      (list :launch "svg" (gen-launcher "svg"))
      (list :launch "png" (gen-launcher "png")))))

(plugin-configure gnuplot
  (:require (and (has-binary-goldfish?) (has-binary-gnuplot?)))
  (:launch ,(gen-launcher "png"))
  (:serializer ,gnuplot-serialize)
  (:session "Gnuplot"))

