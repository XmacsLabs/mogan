
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-s7.scm
;; DESCRIPTION : Initialize S7 plugin
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules
  (binary tm_s7))

(lazy-format (data s7) s7)

(define (s7-serialize lan t)
  (let* ((u (pre-serialize lan t))
         (cork-s (texmacs->code (stree->tree u) "Cork"))
         (s (tmstring->string cork-s)))
    (string-append s "\n<EOF>\n")))

(define (s7-launcher)
  (string-append (url->system (find-binary-tm_s7))
    " --texmacs "
    (url->system (get-texmacs-path))
    "/plugins/s7/s7/tm-s7.scm"))

(plugin-configure s7
  (:require (has-binary-tm_s7?))
  (:launch ,(s7-launcher))
  (:serializer ,s7-serialize)
  (:session "S7 Scheme"))
