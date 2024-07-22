
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

(use-modules
  (binary tm_guile))

(lazy-format (data guile) guile)

(define (guile-serialize lan t)
  (let* ((u (pre-serialize lan t))
         (cork-s (texmacs->code (stree->tree u) "Cork"))
         (s (tmstring->string cork-s)))
    (string-append s "\n<EOF>\n")))

(define (guile-launcher)
  (string-append (url->system (find-binary-guile))
    " --texmacs "
    (url->system (get-texmacs-path))
    "/plugins/guile/guile/tm-guile.scm"))

(plugin-configure guile
  (:require (has-binary-tm_guile?))
  (:launch ,(guile-launcher))
  (:serializer ,guile-serialize)
  (:session "S7 Scheme"))

