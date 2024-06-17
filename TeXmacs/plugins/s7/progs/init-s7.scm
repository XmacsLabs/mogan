
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

(lazy-format (data s7) s7)

(define (s7-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code (stree->tree u) "SourceCode")
      (string-append s "\n<EOF>\n"))))

(define (s7-launcher)
  "tm_s7")

(plugin-configure s7
  (:require (url-exists-in-path? "tm_s7"))
  (:launch ,(s7-launcher))
  (:serializer ,s7-serialize)
  (:session "S7 Scheme"))
