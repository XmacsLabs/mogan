
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-guile.scm
;; DESCRIPTION : Initialize GNU Guile plugin
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (binary guile))

(define (guile-serialize lan t)
  (let* ((u (pre-serialize lan t))
         (s (texmacs->code (stree->tree u) "utf-8")))
    (string-append s "\n<EOF>\n")))

(define (guile-launcher)
  (string-append (url->system (find-binary-guile))
    " "
    (url->system (get-texmacs-path))
    "/plugins/guile/guile/tm-guile.scm"))

(plugin-configure guile
  (:require (has-binary-guile?))
  (:launch ,(guile-launcher))
  (:serializer ,guile-serialize)
  (:session "GNU Guile"))
