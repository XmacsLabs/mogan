
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-kbd.scm
;; DESCRIPTION : keystrokes in text mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-kbd)
  (:use (generic generic-kbd)
	(utils edit auto-close)
	(text text-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special symbols in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-text?)
  ("\"" (insert-quote))
  ("\" var" "\"")
  ("- var" (make 'nbhyph))
  ("'" (insert-apostrophe #f))
  ("' var" (insert-apostrophe #t))

  ("< <" "")
  ("< < var" "<less><less>")
  ("> >" "")
  ("> > var" "<gtr><gtr>")
  ("' '" "")
  ("' ' var" "''")
  ("` `" "")
  ("` ` var" "``")
  (", ," "")
  (", , var" ",,")
  ("- -" "")
  ("- - var" "--")
  ("- - var var" (make 'strike-through))
  ("- - -" "")
  ("- - - var" "---")
  ("- - - -" (make 'hrule))
  ("- - - - var" "----")
  ("- - - - -" "-----")
  (". ." "..")
  (". . var" (make 'fill-out))
  (". . var var" (make 'fill-out*))
  (". . ." (make 'text-dots))
  (". . . var" "...")
  (". . . ." (make 'fill-dots))
  (". . . . var" "....")
  (". . . . ." ".....")
  ("_ _" "__")
  ("_ _ var" (make 'underline))
  ("_ _ var var" (make 'overline))
  ("/ /" "//")
  ("/ / var" (make 'deleted))

  ;; Markdown Style Shortcuts
  ("# # var" (make 'section))
  ("# # # var" (make 'subsection))
  ("# # # # var" (make 'subsubsection))
  ("` ` ` var" (make 'verbatim-code))
  ("* * var" (make-with "font-series" "bold"))
  ("* var" (make-with "font-shape" "italic"))
  ("+ var" (make-tmlist 'itemize))
  ("1 . var" (make-tmlist 'enumerate))
  ("[ ] var" (make 'hlink))
  ("[ ] var var" (make 'slink))

  ("space var" (make 'nbsp))
  ("space var var" (make-space "1em"))
  ("_" "_")
  ("_ var" (make-script #f #t))
  ("^" "^")
  ("^ var" (make-script #t #t))
  ("accent:deadhat var" (make-script #t #t))
  ("sz" "�")

  ("text:symbol s" "�")
  ("text:symbol S" "�")
  ("text:symbol a" "�")
  ("text:symbol a e" "�")
  ("text:symbol o" "�")
  ("text:symbol o e" "�")
  ("text:symbol A" "�")
  ("text:symbol A E" "�")
  ("text:symbol O" "�")
  ("text:symbol O E" "�")
  ("text:symbol !" "�")
  ("text:symbol ?" "�")
  ("text:symbol p" "�")
  ("text:symbol P" "�")
  ("text:symbol m" (make 'masculine))
  ("text:symbol M" (make 'varmasculine))
  ("text:symbol f" (make 'ordfeminine))
  ("text:symbol F" (make 'varordfeminine))

  ("accent:tilde" "~")
  ("accent:tilde space" "~")
  ("accent:tilde A" "�")
  ("accent:tilde N" "�")
  ("accent:tilde O" "�")
  ("accent:tilde a" "�")
  ("accent:tilde n" "�")
  ("accent:tilde o" "�")

  ("accent:hat" "^")
  ("accent:hat space" "^")
  ("accent:hat A" "�")
  ("accent:hat E" "�")
  ("accent:hat I" "�")
  ("accent:hat O" "�")
  ("accent:hat U" "�")
  ("accent:hat a" "�")
  ("accent:hat e" "�")
  ("accent:hat i" "�")
  ("accent:hat o" "�")
  ("accent:hat u" "�")
  ("accent:deadhat" "^")
  ("accent:deadhat space" "^")
  ("accent:deadhat A" "�")
  ("accent:deadhat E" "�")
  ("accent:deadhat I" "�")
  ("accent:deadhat O" "�")
  ("accent:deadhat U" "�")
  ("accent:deadhat a" "�")
  ("accent:deadhat e" "�")
  ("accent:deadhat i" "�")
  ("accent:deadhat o" "�")
  ("accent:deadhat u" "�")

  ("accent:umlaut" "")
  ("accent:umlaut space" "")
  ("accent:umlaut A" "�")
  ("accent:umlaut E" "�")
  ("accent:umlaut I" "�")
  ("accent:umlaut O" "�")
  ("accent:umlaut U" "�")
  ("accent:umlaut Y" "�")
  ("accent:umlaut a" "�")
  ("accent:umlaut e" "�")
  ("accent:umlaut i" "�")
  ("accent:umlaut o" "�")
  ("accent:umlaut u" "�")
  ("accent:umlaut y" "�")

  ("accent:acute" "'")
  ("accent:acute space" "'")
  ("accent:acute A" "�")
  ("accent:acute C" "�")
  ("accent:acute E" "�")
  ("accent:acute I" "�")
  ("accent:acute L" "�")
  ("accent:acute N" "�")
  ("accent:acute O" "�")
  ("accent:acute R" "�")
  ("accent:acute S" "�")
  ("accent:acute U" "�")
  ("accent:acute Y" "�")
  ("accent:acute Z" "�")
  ("accent:acute a" "�")
  ("accent:acute c" "�")
  ("accent:acute e" "�")
  ("accent:acute i" "�")
  ("accent:acute l" "�")
  ("accent:acute n" "�")
  ("accent:acute o" "�")
  ("accent:acute r" "�")
  ("accent:acute s" "�")
  ("accent:acute u" "�")
  ("accent:acute y" "�")
  ("accent:acute z" "�")

  ("accent:grave" "`")
  ("accent:grave space" "`")
  ("accent:grave A" "�")
  ("accent:grave E" "�")
  ("accent:grave I" "�")
  ("accent:grave O" "�")
  ("accent:grave U" "�")
  ("accent:grave a" "�")
  ("accent:grave e" "�")
  ("accent:grave i" "�")
  ("accent:grave o" "�")
  ("accent:grave u" "�")

  ("accent:cedilla" "")
  ("accent:cedilla space" "")
  ("accent:cedilla C" "�")
  ("accent:cedilla S" "�")
  ("accent:cedilla T" "�")
  ("accent:cedilla c" "�")
  ("accent:cedilla s" "�")
  ("accent:cedilla t" "�")

  ("accent:breve" "")
  ("accent:breve space" "")
  ("accent:breve A" "�")
  ("accent:breve G" "�")
  ("accent:breve a" "�")
  ("accent:breve g" "�")

  ("accent:check" "")
  ("accent:check space" "")
  ("accent:check C" "�")
  ("accent:check D" "�")
  ("accent:check E" "�")
  ("accent:check L" "�")
  ("accent:check N" "�")
  ("accent:check R" "�")
  ("accent:check S" "�")
  ("accent:check T" "�")
  ("accent:check U" "�")
  ("accent:check Z" "�")
  ("accent:check c" "�")
  ("accent:check d" "�")
  ("accent:check e" "�")
  ("accent:check l" "�")
  ("accent:check n" "�")
  ("accent:check r" "�")
  ("accent:check s" "�")
  ("accent:check t" "�")
  ("accent:check u" "�")
  ("accent:check z" "�")

  ("accent:doubleacute" "")
  ("accent:doubleacute space" "")
  ("accent:doubleacute O" "�")
  ("accent:doubleacute U" "�")
  ("accent:doubleacute o" "�")
  ("accent:doubleacute u" "�")

  ("accent:abovering" "")
  ("accent:abovering space" "")
  ("accent:abovering A" "�")
  ("accent:abovering U" "�")
  ("accent:abovering a" "�")
  ("accent:abovering u" "�")

  ("accent:abovedot" "
")
  ("accent:abovedot space" "
")
  ("accent:abovedot Z" "�")
  ("accent:abovedot I" "�")
  ("accent:abovedot z" "�")

  ("accent:ogonek" "")
  ("accent:ogonek space" "")
  ("accent:ogonek a" "�")
  ("accent:ogonek A" "�")
  ("accent:ogonek e" "�")
  ("accent:ogonek E" "�")

  ("" "")
  ("" "")
  ("" "")
  ("" "")
  ("" "")
  ("" "")
  ("" "")
  ("" "")
  ("" "")

  ("exclamdown" "�")
  ("cent" "<#A2>")
  ("sterling" "�")
  ("currency" "<#A4>")
  ("yen" "<#A5>")
  ("section" "�")
  ("copyright" "<#A9>")
  ("copyright var" (make 'copyleft))
  ("ordfeminine" "<#AA>")
  ("ordfeminine var" (make 'varordfeminine))
  ("guillemotleft" "")
  ("registered" "<#AE>")
  ("circledR" "<#AE>")
  ("degree" "<#B0>")
  ("twosuperior" "<#B2>")
  ("threesuperior" "<#B3>")
  ("paragraph" "<#B6>")
  ("onesuperior" "<#B9>")
  ("masculine" "<#BA>")
  ("masculine var" (make 'varmasculine))
  ("guillemotright" "")
  ("onequarter" "<#BC>")
  ("onehalf" "<#BD>")
  ("threequarters" "<#BE>")
  ("questiondown" "�")

  ("trademark" "<#2122>")
  ("big-sum" (insert '(big "sum")))
  ("dagger" "<dag>")
  ("sqrt" (insert '(sqrt "")))
  ("big-int" (insert '(big "int")))
  ("euro" "<#20AC>")
  ("ddagger" "<ddag>")
  ("centerdot" "<cdot>")
  ("big-prod" (insert '(big "prod")))
  ("varspace" (insert '(nbsp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font dependent shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-text?)
  (:require (== (get-env "font") "roman"))

  ("copyright" (make 'copyright)) ;; "<#A9>"
  ("ordfeminine" (make 'ordfeminine))
  ("masculine" (make 'masculine))
  ("<#20AC>" (make 'euro))
  ("euro" (make 'euro)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language dependent shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-cyrillic?)
  ("modeswitch"
   (make-with "language" "english")
   (make-with "font" "roman")))

; This breaks � for german keyboards on all systems.
; Maybe it was an old fix for bug #2092 ? It now seems incorrect.
;(kbd-map
;  (:mode in-german?)
;  ("�" "�")
;  ("�" "�"))

(kbd-map
  (:mode in-esperanto?)
  ("c var" "<#109>")
  ("g var" "<#11D>")
  ("h var" "<#125>")
  ("j var" "<#135>")
  ("s var" "<#15D>")
  ("u var" "<#16D>")
  ("C var" "<#108>")
  ("G var" "<#11C>")
  ("H var" "<#124>")
  ("J var" "<#134>")
  ("S var" "<#15C>")
  ("U var" "<#16C>"))

(kbd-map
  (:mode in-hungarian?)
  ("text:symbol O" "�")
  ("text:symbol U" "�")
  ("text:symbol o" "�")
  ("text:symbol u" "�")
  ("text:symbol O var" "�")
  ("text:symbol o var" "�"))

(kbd-map
  (:mode in-spanish?)
  ("�" "�")
  ("�" "�")
  ("! var" "�")
  ("? var" "�")
  ("! `" "�")
  ("? `" "�")
  ("! accent:grave" "�")
  ("? accent:grave" "�"))

(kbd-map
  (:mode in-polish?)
  ("text:symbol a" "�")
  ("text:symbol A" "�")
  ("text:symbol c" "�")
  ("text:symbol C" "�")
  ("text:symbol e" "�")
  ("text:symbol E" "�")
  ("text:symbol l" "�")
  ("text:symbol L" "�")
  ("text:symbol n" "�")
  ("text:symbol N" "�")
  ("text:symbol o" "�")
  ("text:symbol O" "�")
  ("text:symbol s" "�")
  ("text:symbol S" "�")
  ("text:symbol x" "�")
  ("text:symbol X" "�")
  ("text:symbol z" "�")
  ("text:symbol Z" "�")
  ("text:symbol a var" "�")
  ("text:symbol A var" "�")
  ("text:symbol o var" "�")
  ("text:symbol O var" "�")
  ("text:symbol s var" "�")
  ("text:symbol S var" "�")
  ("text:symbol z var" "�")
  ("text:symbol Z var" "�"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overwrite shortcuts which are inadequate in certain contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-variants-disabled?)
  ("- var" (begin (insert "-") (kbd-tab)))
  ("space var" (begin (insert " ") (kbd-tab)))
  ("space var var" (begin (insert " ") (kbd-tab) (kbd-tab))))

(kbd-map
  (:mode in-verbatim?)
  ("space var" (insert-tabstop))
  ("space var var" (begin (insert-tabstop) (insert-tabstop)))
  ("$" (insert "$"))
  ("$ var" (make 'math))
  ("\\" "\\")
  ("\\ var" (make 'hybrid))
  ("\"" "\"")
  ("`" "`")
  ("` var" "<#2018>")
  ("'" "'")
  ("' var" "<#2019>")
  ("< <" "<less><less>")
  ("> >" "<gtr><gtr>")
  ("' '" "''")
  ("` `" "``")
  ("- -" "--")
  ("- - -" "---")
  ("< < var" "")
  ("> > var" "")
  ("' ' var" "")
  ("` ` var" "")
  (", , var" "")
  ("- - var" "")
  ("- - - var" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing the text format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-text?)
  ("font s" (make-with "font-family" "ss"))
  ("font t" (make-with "font-family" "tt"))
  ("font b" (make-with "font-series" "bold"))
  ("font m" (make-with "font-series" "medium"))
  ("font r" (make-with "font-shape" "right"))
  ("font i" (make-with "font-shape" "italic"))
  ("font l" (make-with "font-shape" "slanted"))
  ("font p" (make-with "font-shape" "small-caps")))

(kbd-map
  (:profile macos)
  (:mode in-text?)
  ("macos {" (make-line-with "par-mode" "left"))
  ("macos |" (make-line-with "par-mode" "center"))
  ("macos }" (make-line-with "par-mode" "right"))
  ("macos C-{" (make-line-with "par-mode" "justify")))

(kbd-map
  (:profile windows)
  (:mode in-text?)
  ("windows l" (make-line-with "par-mode" "left"))
  ("windows e" (make-line-with "par-mode" "center"))
  ("windows r" (make-line-with "par-mode" "right"))
  ("windows j" (make-line-with "par-mode" "justify")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard markup in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-std-text?)
  ("text $" (make-equation*))
  ("text &" (make-eqnarray*))

  ("text a" (make 'abbr))
  ("text d" (make-tmlist 'description))
  ("text m" (make 'em))
  ("text n" (make 'name))
  ("text p" (make 'samp))
  ("text s" (make 'strong))
  ("text v" (make 'verbatim))
  ("text ;" (make-item))
  ("text 0" (make-section 'chapter))
  ("text 1" (make-section 'section))
  ("text 2" (make-section 'subsection))
  ("text 3" (make-section 'subsubsection))
  ("text 4" (make-section 'paragraph))
  ("text 5" (make-section 'subparagraph))

  ("F5" (make 'em))
  ("F6" (make 'strong))
  ("F7" (make 'verbatim))
  ("F8" (make 'samp))
  ("S-F6" (make 'name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other shortcuts in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-std?)
  ("std @" (go-to-section-title)))
