
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-asymptote.scm
;; DESCRIPTION : Initialize Asymptote plugin
;; COPYRIGHT   : (C) Yann Dirson <ydirson at altern dot org>.
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (asy-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (asy-entry)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (system-url->string "$TEXMACS_HOME_PATH/plugins/tmpy/session/tm_asy.py")
      (system-url->string "$TEXMACS_PATH/plugins/tmpy/session/tm_asy.py")))

(define (asy-launcher)
  (string-append (python-command) " " (asy-entry)))

(plugin-configure asymptote
  (:winpath "Asymptote" ".")
  (:require (url-exists-in-path? "asy"))
  (:require (python-command))
  (:launch ,(asy-launcher))
  (:serializer ,asy-serialize)
  (:session "Asymptote")
  (:scripts "Asymptote"))

(when (supports-asymptote?)
  (import-from (asymptote-menus))
  (import-from (utils plugins plugin-convert)))