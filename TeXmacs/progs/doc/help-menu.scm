
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : help-menu.scm
;; DESCRIPTION : the help menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc help-menu)
  (:use (doc help-funcs))); (doc apidoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Help menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plugin-documented? name)
  (url-exists-in-help? (string-append name ".en.tm")))

(tm-menu (help-configuration-menu)
  ("User preferences"
   (load-local-doc "configuration/conf.userpref"))
  ("Keyboard configuration"
   (load-local-doc "configuration/conf.keyconfig"))
  ("Users of Cyrillic languages"
   (load-local-doc "configuration/conf.cyrillicuser"))
  ("Users of oriental languages"
   (load-local-doc "configuration/conf.eastuser")))

(tm-menu (help-manual-menu)
  ("Getting started"
   (load-local-doc "manual/manu.gettingstarted"))
  ("Typing simple texts"
   (load-local-doc "manual/manu.typing"))
  ("Mathematical formulas"
   (load-local-doc "manual/manu.formulas"))
  ("Tabular material"
   (load-local-doc "manual/manu.tables"))
  ("Automatic content generation"
   (load-local-doc "manual/manu.toc"))
  ("Creating technical pictures"
   (load-local-doc "manual/manu.tables")) 
  ("Advanced layout features"
   (load-local-doc "manual/manu.layout"))
  ("Editing tools"
   (load-local-doc "manual/manu.editing"))
  ("Laptop presentations"
   (load-local-doc "manual/manu.slides"))
  ("TeXmacs as an interface"
   (load-local-doc "manual/manu.interface"))
  ("Writing your own style files"
   (load-local-doc "manual/manu.styles"))
  ("Customizing TeXmacs"
   (load-local-doc "manual/manu.custom"))
  ("The TeXmacs plug-in system"
   (load-local-doc "manual/manu.plugin")))

(menu-bind help-menu
  ("Welcome" (mogan-welcome))
  (-> "Configuration"
    (link help-configuration-menu))
  (-> "Manual"
    (link help-manual-menu))
  ("Template" (load-local-doc "main/template"))
  (-> "Plugins"
    (link help-plugins-menu)))
