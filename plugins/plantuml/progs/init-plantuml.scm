
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-plantuml.scm
;; DESCRIPTION : Initialize PlantUML plugin
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (dynamic session-edit) (dynamic program-edit))

(define (plantuml-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (plantuml-entry)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (system-url->string "$TEXMACS_HOME_PATH/plugins/tmpy/session/tm_plantuml.py")
      (system-url->string "$TEXMACS_PATH/plugins/tmpy/session/tm_plantuml.py")))

(define (plantuml-launcher)
  (string-append (python-utf8-command) (plantuml-entry)))

(plugin-configure plantuml
  (:require (url-exists-in-path? "plantuml"))
  (:require (python-command))
  (:launch ,(plantuml-launcher))
  (:serializer ,plantuml-serialize)
  (:session "PlantUML"))
