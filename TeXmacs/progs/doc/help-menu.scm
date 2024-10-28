
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
  (-> "Plugins"
    (link help-plugins-menu)))
