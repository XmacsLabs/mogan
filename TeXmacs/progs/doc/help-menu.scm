
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
  (and (url-exists-in-help? (string-append name ".en.tm"))
       (url-exists-in-help? (string-append name "-abstract.en.tm"))))

(tm-menu (help-plugins-menu)
  (for (name (list-filter (map symbol->string (plugin-list))
                          plugin-documented?))
    (with menu-name `(verbatim ,(session-name name))
      ((eval menu-name)
       (load-help-article (string-append name))))))

(menu-bind help-menu
  (if (detailed-menus?)
	  (-> "Configuration"
	      ("Browse" (load-help-buffer "main/config/man-configuration"))
	      ---
	      ("User preferences"
	       (load-help-article "main/config/man-preferences"))
	      ("Keyboard configuration"
	       (load-help-article "main/config/man-config-keyboard"))
	      ("Users of Cyrillic languages"
	       (load-help-article "main/config/man-russian"))
	      ("Users of oriental languages"
	       (load-help-article "main/config/man-oriental"))))
  (-> "Manual"
      ("Browse" (load-help-buffer "main/man-manual"))
      ---
      ("Getting started"
       (load-help-article "main/start/man-getting-started"))
      ("Typing simple texts"
       (load-help-article "main/text/man-text"))
      ("Mathematical formulas"
       (load-help-article "main/math/man-math"))
      ("Tabular material"
       (load-help-article "main/table/man-table"))
      ("Automatic content generation"
       (load-help-article "main/links/man-links"))
      ("Creating technical pictures"
       (load-help-article "main/graphics/man-graphics"))
      ("Advanced layout features"
       (load-help-article "main/layout/man-layout"))
      ---
      ("Editing tools"
       (load-help-article "main/editing/man-editing-tools"))
      ("Laptop presentations"
       (load-help-article "main/beamer/man-beamer"))
      ("TeXmacs as an interface"
       (load-help-article "main/interface/man-itf"))
      ---
      ("Writing your own style files"
       (load-help-article "devel/style/style"))
      ("Customizing TeXmacs"
       (load-help-article "main/scheme/man-scheme"))
      ("The TeXmacs plug-in system"
       (load-help-article "devel/plugin/plugins")))
  (-> "Reference guide"
      ("Browse" (load-help-buffer "main/man-reference"))
      ---
      ("The TeXmacs format"
       (load-help-article "devel/format/basics/basics"))
      ("Standard environment variables"
       (load-help-article "devel/format/environment/environment"))
      ("TeXmacs primitives"
       (load-help-article "devel/format/regular/regular"))
      ("Stylesheet language"
       (load-help-article "devel/format/stylesheet/stylesheet"))
      ("Standard TeXmacs styles"
       (load-help-article "main/styles/styles"))
      ("Compatibility with other formats"
       (load-help-article "main/convert/man-convert")))
  ---
  (-> "Plug-ins"
      (link help-plugins-menu))
  (if (detailed-menus?)
      (-> "Interfacing"
          ("Browse" (load-help-buffer "devel/interface/interface"))
          ---
          ("Introduction"
           (load-help-article "devel/interface/interface-intro"))
          ("Basic communication using pipes"
           (load-help-article "devel/interface/interface-pipes"))
          ("Formatted and structured output"
           (load-help-article "devel/interface/interface-nested"))
          ("Prompts and default input"
           (load-help-article "devel/interface/interface-channels"))
          ("Sending commands to TeXmacs"
           (load-help-article "devel/interface/interface-commands"))
          ("Background evaluations"
           (load-help-article "devel/interface/interface-background"))
          ("Mathematical and customized input"
           (load-help-article "devel/interface/interface-input"))
          ("Tab-completion"
           (load-help-article "devel/interface/interface-tab"))
          ("Dynamic libraries"
           (load-help-article "devel/interface/interface-dynlibs"))
          ("Miscellaneous features"
           (load-help-article "devel/interface/interface-misc"))
          ("Writing documentation"
           (load-help-article "devel/interface/interface-documentation"))
          ("Plans for the future"
           (load-help-article "devel/interface/interface-plans")))
      (-> "Scheme extensions"
          ("Browse" (load-help-buffer "devel/scheme/scheme"))
          ---
          ("Overview of the scheme extension language"
           (load-help-article "devel/scheme/overview/scheme-overview"))
          ("TeXmacs extensions to scheme and utilities"
           (load-help-article "devel/scheme/utils/scheme-utils"))
          ("Programming routines for editing documents"
           (load-help-article "devel/scheme/edit/scheme-edit"))
          ("Program interface for buffer management"
           (load-help-article "devel/scheme/buffer/scheme-buffer"))
          ("Scheme interface for the graphical mode"
           (load-help-article "devel/scheme/graphics/scheme-graphics"))
          ("Customizing and extending the user interface"
           (load-help-article "devel/scheme/gui/scheme-gui"))
          ("Writing TeXmacs bibliography styles"
           (load-help-article "devel/scheme/bibliography/bibliography"))
          ---
          ("Browse modules documentation" (apidoc-all-modules))
          ("Browse symbols documentation" (apidoc-all-symbols)))))
