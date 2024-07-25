
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-goldfish.scm
;; DESCRIPTION : Initialize S7 plugin
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (binary goldfish))

(lazy-format (data goldfish) goldfish)

(define (goldfish-serialize lan t)
  (let* ((u (pre-serialize lan t))
         (cork-s (texmacs->code (stree->tree u) "Cork"))
         (s (tmstring->string cork-s)))
    (string-append s "\n<EOF>\n")))

(define (goldfish-launcher)
  (string-append (url->system (find-binary-tm_goldfish))
    " --texmacs "
    (url->system (get-texmacs-path))
    "/plugins/goldfish/goldfish/tm-goldfish.scm"))

(plugin-configure goldfish
  (:require (has-binary-goldfish?))
  (:launch ,(goldfish-launcher))
  (:serializer ,goldfish-serialize)
  (:session "Goldfish Scheme"))
