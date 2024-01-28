
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

(tm-menu (help-plugins-menu)
  (for (name (list-filter (map symbol->string (plugin-list))
                          plugin-documented?))
    (with menu-name `(verbatim ,(session-name name))
      ((eval menu-name)
       (load-local-plugin-doc name)))))

(menu-bind help-menu
  ("Welcome" (mogan-welcome))
  ("Planet" (xmacs-planet))
  (-> "Plugins"
      (link help-plugins-menu))
  ---
  (group "TeXmacs online docs")
  (if (detailed-menus?)
	  (-> "Configuration"
	      ("Browse" (load-remote-doc "main/config/man-configuration"))
	      ---
	      ("User preferences"
	       (load-remote-doc "main/config/man-preferences"))
	      ("Keyboard configuration"
	       (load-remote-doc "main/config/man-config-keyboard"))
	      ("Users of Cyrillic languages"
	       (load-remote-doc "main/config/man-russian"))
	      ("Users of oriental languages"
	       (load-remote-doc "main/config/man-oriental"))))
  (-> "Manual"
      ("Browse" (load-remote-doc "main/man-manual"))
      ---
      ("Getting started"
       (load-remote-doc "main/start/man-getting-started"))
      ("Typing simple texts"
       (load-remote-doc "main/text/man-text"))
      ("Mathematical formulas"
       (load-remote-doc "main/math/man-math"))
      ("Tabular material"
       (load-remote-doc "main/table/man-table"))
      ("Automatic content generation"
       (load-remote-doc "main/links/man-links"))
      ("Creating technical pictures"
       (load-remote-doc "main/graphics/man-graphics"))
      ("Advanced layout features"
       (load-remote-doc "main/layout/man-layout"))
      ---
      ("Editing tools"
       (load-remote-doc "main/editing/man-editing-tools"))
      ("Laptop presentations"
       (load-remote-doc "main/beamer/man-beamer"))
      ("TeXmacs as an interface"
       (load-remote-doc "main/interface/man-itf"))
      ---
      ("Writing your own style files"
       (load-remote-doc "devel/style/style"))
      ("Customizing TeXmacs"
       (load-remote-doc "main/scheme/man-scheme"))
      ("The TeXmacs plug-in system"
       (load-remote-doc "devel/plugin/plugins")))
  (-> "Reference guide"
      ("Browse" (load-remote-doc "main/man-reference"))
      ---
      ("The TeXmacs format"
       (load-remote-doc "devel/format/basics/basics"))
      ("Standard environment variables"
       (load-remote-doc "devel/format/environment/environment"))
      ("TeXmacs primitives"
       (load-remote-doc "devel/format/regular/regular"))
      ("Stylesheet language"
       (load-remote-doc "devel/format/stylesheet/stylesheet"))
      ("Standard TeXmacs styles"
       (load-remote-doc "main/styles/styles"))
      ("Compatibility with other formats"
       (load-remote-doc "main/convert/man-convert")))
  (if (detailed-menus?)
      (-> "Interfacing"
          ("Browse" (load-remote-doc "devel/interface/interface"))
          ---
          ("Introduction"
           (load-remote-doc "devel/interface/interface-intro"))
          ("Basic communication using pipes"
           (load-remote-doc "devel/interface/interface-pipes"))
          ("Formatted and structured output"
           (load-remote-doc "devel/interface/interface-nested"))
          ("Prompts and default input"
           (load-remote-doc "devel/interface/interface-channels"))
          ("Sending commands to TeXmacs"
           (load-remote-doc "devel/interface/interface-commands"))
          ("Background evaluations"
           (load-remote-doc "devel/interface/interface-background"))
          ("Mathematical and customized input"
           (load-remote-doc "devel/interface/interface-input"))
          ("Tab-completion"
           (load-remote-doc "devel/interface/interface-tab"))
          ("Dynamic libraries"
           (load-remote-doc "devel/interface/interface-dynlibs"))
          ("Miscellaneous features"
           (load-remote-doc "devel/interface/interface-misc"))
          ("Writing documentation"
           (load-remote-doc "devel/interface/interface-documentation"))
          ("Plans for the future"
           (load-remote-doc "devel/interface/interface-plans")))
      (-> "Scheme extensions"
          ("Browse" (load-remote-doc "devel/scheme/scheme"))
          ---
          ("Overview of the scheme extension language"
           (load-remote-doc "devel/scheme/overview/scheme-overview"))
          ("TeXmacs extensions to scheme and utilities"
           (load-remote-doc "devel/scheme/utils/scheme-utils"))
          ("Programming routines for editing documents"
           (load-remote-doc "devel/scheme/edit/scheme-edit"))
          ("Program interface for buffer management"
           (load-remote-doc "devel/scheme/buffer/scheme-buffer"))
          ("Scheme interface for the graphical mode"
           (load-remote-doc "devel/scheme/graphics/scheme-graphics"))
          ("Customizing and extending the user interface"
           (load-remote-doc "devel/scheme/gui/scheme-gui"))
          ("Writing TeXmacs bibliography styles"
           (load-remote-doc "devel/scheme/bibliography/bibliography"))
          ---
          ("Browse modules documentation" (apidoc-all-modules))
          ("Browse symbols documentation" (apidoc-all-symbols)))))
