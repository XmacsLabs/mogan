
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-elvish.scm
;; DESCRIPTION : Elvish plugin initialization
;; COPYRIGHT   : (C) 2024  Liii Network Inc
;; All rights reverved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-format (data elvish) elvish)

(define (elvish-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code (stree->tree u) "SourceCode")
      (string-append  s "\n<EOF>\n"))))

(define (elvish-entry)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/elvish")
      (system-url->string "$TEXMACS_HOME_PATH/plugins/elvish/src/main.elv")
      (system-url->string "$TEXMACS_PATH/plugins/elvish/src/main.elv")))

(define (elvish-launcher)
  (string-append "elvish " (elvish-entry)))

(plugin-configure elvish
  (:require (url-exists-in-path? "elvish"))
  (:launch ,(elvish-launcher))
  (:serializer ,elvish-serialize)
  (:session "Elvish")
  (:scripts "Elvish"))

