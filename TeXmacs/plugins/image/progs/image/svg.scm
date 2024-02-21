
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : svg.scm
;; DESCRIPTION : SVG Image plugin
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;                   2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (image svg)
  (:use (binary rsvg-convert)))

(converter svg-file postscript-file
  (:require (url-exists-in-path? "inkscape"))
  (:shell "inkscape" "-z" "-f" from "-P" to))

(converter svg-file pdf-file
  (:require (url-exists-in-path? "inkscape"))
  (:shell "inkscape" "-z" "-f" from "-A" to))

(converter svg-file png-file
  (:require (url-exists-in-path? "inkscape"))
  (:shell "inkscape" "-z" "-d" "600" from "--export-png" to))

(converter svg-file png-file
  (:require (and (has-binary-rsvg-convert?)
                 (not (url-exists-in-path? "inkscape"))))
    (:function-with-options svg2png-by-rsvg-convert))

(converter svg-file postscript-document
  (:function image->psdoc))
