
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-gambit.scm
;; DESCRIPTION : Initialize the gambit plugin
;; COPYRIGHT   : (C) 2024   Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (binary gambit))

(define (gambit-serialize lan t)
  (let* ((u (pre-serialize lan t))
         (s (texmacs->code (stree->tree u) "utf-8")))
    (string-append s "\n<EOF>\n")))

(define (gambit-launcher)
  (string-append (url->system (find-binary-gambit))
    " "
    (string-append (url->system (get-texmacs-path)) "/plugins/gambit/gambit/")
    " "
    (string-append (url->system (get-texmacs-path)) "/plugins/gambit/gambit/tm-gambit.scm")))

(plugin-configure gambit
  (:require (and (has-binary-gambit?)
                 (== (version-binary-gambit) "v4.9.5")))
  (:launch ,(gambit-launcher))
  (:serializer ,gambit-serialize)
  (:session "Gambit Scheme"))
