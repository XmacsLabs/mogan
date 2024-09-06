
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : source-kbd.scm
;; DESCRIPTION : shortcuts for dynamic markup
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source source-kbd)
  (:use (generic generic-kbd)
	(source source-edit)))

(kbd-map
  ("altcmd )" (make-style-with "src-compact" "none"))
  ("altcmd (" (make-style-with "src-compact" "all"))
  ("altcmd @" (make-mod-active 'style-only*))
  ("altcmd -" (make-mod-active 'inactive*))
  ("altcmd +" (make-mod-active 'active*))
  ("altcmd C-@" (make-mod-active 'style-only))
  ("altcmd C--" (make-mod-active 'inactive))
  ("altcmd C-+" (make-mod-active 'active))

  ("inactive a" (make 'arg))
  ("inactive A" (make 'map-args))
  ("inactive c" (make 'compound))
  ("inactive C" (make 'case))
  ("inactive d" (make 'drd-props))
  ("inactive e" (make 'eval-args))
  ("inactive i" (make 'include))
  ("inactive l" (make-latex))
  ("inactive m" (make 'macro))
  ("inactive M" (make 'meaning))
  ("inactive n" (make 'get-arity))
  ("inactive N" (make 'get-label))
  ("inactive p" (make 'provides))
  ("inactive q" (make 'quasi))
  ("inactive s" (make 'surround))
  ("inactive t" (make 'tag))
  ("inactive v" (make 'value))
  ("inactive w" (make 'with 3))
  ("inactive W" (make 'while))
  ("inactive x" (make 'xmacro))
  ("inactive X" (make 'extern))
  ("inactive !" (make 'eval))
  ("inactive =" (make 'assign))
  ("inactive (" (make 'tuple))
  ("inactive <" (make 'tuple))
  ("inactive @" (make 'attr))
  ("inactive C->" (make 'write))
  ("inactive )" (make 'wiki-link))
  ("inactive #" (make 'arg))
  ("inactive $" (noop) (make 'symbol))
  ("inactive '" (make 'quote))
  ("inactive `" (make 'quasiquote))
  ("inactive ," (make 'unquote))
  ("inactive ?" (make 'if))
  ("inactive C-?" (make 'if*))
  ("inactive delete" (remove-unary-document))
  ("inactive backspace" (remove-unary-document)))

(kbd-map
  (:mode in-source?)
  ("special a" (make 'arg))
  ("special A" (make 'map-args))
  ("special c" (make 'compound))
  ("special C" (make 'case))
  ("special d" (make 'drd-props))
  ("special e" (make 'eval-args))
  ("special m" (make 'macro))
  ("special n" (make 'get-arity))
  ("special N" (make 'get-label))
  ("special q" (make 'quasi))
  ("special s" (make 'surround))
  ("special v" (make 'value))
  ("special w" (make 'with 3))
  ("special W" (make 'while))
  ("special x" (make 'xmacro))
  ("special =" (make 'assign))
  ("special (" (make 'tuple))
  ("special #" (make 'arg))
  ("special !" (make 'eval))
  ("special '" (make 'quote))
  ("special `" (make 'quasiquote))
  ("special ," (make 'unquote))
  ("special ?" (make 'if)))
