
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-data.scm
;; DESCRIPTION : various Data formats
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Source Code data
(lazy-format (data cpp) cpp)
(lazy-format (data csv) csv)
(lazy-format (data dot) dot)
(lazy-format (data java) java)
(lazy-format (data javascript) javascript)
(lazy-format (data json) json)
(lazy-format (data julia) julia)
(lazy-format (data python) python)
(lazy-format (data scala) scala)
(lazy-format (data scheme) scheme)

; Image data
(lazy-format (data image)
  postscript pdf svg
  gif jpeg png ppm tif webp xpm)
