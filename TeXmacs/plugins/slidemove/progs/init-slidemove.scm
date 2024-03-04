;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-slidemove.scm
;; DESCRIPTION : Initialize the slidemove module 
;; COPYRIGHT   : (C) 2022  Jeroen Wouters
;;
;; This software falls under the GNU general public license version 3 orlater.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the fileLICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This plugin provides a way to move slides around in beamer presentations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(plugin-configure slidemove
  (:require #t))
  
(when (supports-slidemove?)
;; no dashes seem to be allow in a plugin name!
  (use-modules (slidemove)))
  ;;(kbd-map
  ;;      ("C-A-pageup" (move-slide-up))
  ;;      ("C-A-pagedown" (move-slide-down))))
