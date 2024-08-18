
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : buffer.scm
;; DESCRIPTION : buffer handling
;; COPYRIGHT   : (C) 2001-2021  Joris van der Hoeven
;;                   2024       Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (app research buffer))

(tm-define (buffer-set-default-style)
  (init-style "generic")
  (with lan (get-preference "language")
    (if (!= lan "english") (set-document-language lan)))
  (with psz (get-printer-paper-type)
    (if (!= psz "a4") (init-page-type psz)))
  (with type (get-preference "page medium")
    (if (!= type "papyrus") (init-env "page-medium" type)))
  (with type (get-preference "page screen margin")
    (if (!= type "true") (init-env "page-screen-margin" type)))
  (when (!= (get-preference "scripting language") "none")
    (lazy-plugin-force)
    (init-env "prog-scripts" (get-preference "scripting language")))
  (buffer-pretend-saved (current-buffer)))
