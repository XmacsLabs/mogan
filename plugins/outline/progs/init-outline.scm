;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-outline.scm
;; DESCRIPTION : Initialize the ouline module 
;; COPYRIGHT   : (C) 2019-2021  Philippe Joyez
;;
;; This software falls under the GNU general public license version 3 orlater.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the fileLICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This plugin provides an "outline mode" for restructuring documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(plugin-configure outline
  (:require #t))
;(display* "supports-outline? : " (supports-outline?))
(when (supports-outline?)
  (use-modules (outline))
  (kbd-map 
    ("C-O" (toggle-outline-toolbar))
  )
  (delayed
    (:idle 2000)
  (lazy-define-force tools-menu)
  (tm-menu (tools-menu)
  (former)
  ---
      ("Toggle outline Toolbar" (toggle-outline-toolbar))))
)
