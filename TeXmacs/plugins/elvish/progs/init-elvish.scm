;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(use-modules (binary elvish))

(lazy-format (data elvish) elvish)

(define (elvish-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code (stree->tree u) "SourceCode")
      (string-append  s "\n<EOF>\n"))))

(define (elvish-entry)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/elvish")
      (url->string "$TEXMACS_HOME_PATH/plugins/elvish/src/main.elv")
      (url->string "$TEXMACS_PATH/plugins/elvish/src/main.elv")))

(define (elvish-launcher)
  (string-append (string-quote (url->system (find-binary-elvish)))
                 " "
                 (string-quote (elvish-entry))))

(plugin-configure elvish
  (:require (has-binary-elvish?))
  (:launch ,(elvish-launcher))
  (:serializer ,elvish-serialize)
  (:session "Elvish")
  (:scripts "Elvish"))

