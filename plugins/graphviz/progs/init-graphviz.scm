
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-graphviz.scm
;; DESCRIPTION : Initialize Graphviz plugin
;; COPYRIGHT   : (C) 2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (dynamic session-edit) (dynamic program-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format dot
  (:name "DOT Source Code")
  (:suffix "gv"))

(define (texmacs->dot x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (dot->texmacs x . opts)
  (code->texmacs x))

(define (dot-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree dot-document
  (:function texmacs->dot))

(converter dot-document texmacs-tree
  (:function dot->texmacs))
  
(converter texmacs-tree dot-snippet
  (:function texmacs->dot))

(converter dot-snippet texmacs-tree
  (:function dot-snippet->texmacs))


(define (graphviz-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (graphviz-entry)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (system-url->string "$TEXMACS_HOME_PATH/plugins/tmpy/session/tm_graphviz.py")
      (system-url->string "$TEXMACS_PATH/plugins/tmpy/session/tm_graphviz.py")))

(define (graphviz-launcher)
  (string-append (python-command) (graphviz-entry)))

(plugin-configure dot 
  (:winpath "Graphviz" "bin")
  (:require (url-exists-in-path? "dot"))
  (:require (python-command))
  (:launch ,(graphviz-launcher))
  (:serializer ,graphviz-serialize)
  (:session "Graphviz"))
