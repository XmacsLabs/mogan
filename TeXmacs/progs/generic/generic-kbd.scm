
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-kbd.scm
;; DESCRIPTION : general keyboard shortcuts for all modes
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-kbd)
  (:use (texmacs keyboard prefix-kbd)
        (utils edit variants)
        (utils edit auto-close)
        (utils library cursor)
        (generic document-edit)
        (generic generic-edit)
        (generic format-edit)
        (generic format-geometry-edit)
        (source source-edit)
        (texmacs texmacs tm-files)
        (texmacs texmacs tm-print)
        (doc help-funcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General shortcuts for all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("F1" (interactive docgrep-in-doc))
  ("S-F1" (interactive docgrep-in-src)) ;; FIXME: S-F1 should be 'What is This?'
  ;("M-F1" (load-help-buffer "about/welcome/welcome")) ; Conflict with devtools

  ("<" "<less>")
  (">" "<gtr>")
  ("(" (make-bracket-open "(" ")"))
  (")" (make-bracket-close ")" "("))
  ("[" (make-bracket-open "[" "]"))
  ("]" (make-bracket-close "]" "["))
  ("{" (make-bracket-open "{" "}"))
  ("}" (make-bracket-close "}" "{"))
  ("\\" (if (or (inside? 'hybrid) (in-prog?)) (insert "\\") (make-hybrid)))
  ("\\ var" "\\")
  ("\\ var var" "<setminus>")
  ("$" (make 'math))
  ("$ var" "$")

  ("-" "-")
  ("space" (kbd-space))
  ("tab" (kbd-tab))
  ("enter" (kbd-return))
  ("return" (kbd-return))
  ("S-space" (kbd-shift-space))
  ("S-tab" (kbd-shift-tab))
  ("S-return" (kbd-shift-return))
  ("C-return" (kbd-control-return))
  ("C-S-return" (kbd-shift-control-return))
  ("A-return" (kbd-alternate-return))
  ("A-S-return" (kbd-shift-alternate-return))

  ("delete" (kbd-delete))
  ("backspace" (kbd-backspace))
  ("left" (kbd-left))
  ("right" (kbd-right))
  ("up" (kbd-up))
  ("down" (kbd-down))
  ("home" (kbd-start-line))
  ("end" (kbd-end-line))
  ("pageup" (kbd-page-up))
  ("pagedown" (kbd-page-down))
  ("S-delete" (kbd-delete))
  ("S-backspace" (kbd-backspace))
  ("S-left" (kbd-select kbd-left))
  ("S-right" (kbd-select kbd-right))
  ("S-up" (kbd-select kbd-up))
  ("S-down" (kbd-select kbd-down))
  ("S-home" (kbd-select kbd-start-line))
  ("S-end" (kbd-select kbd-end-line))
  ("S-pageup" (kbd-select kbd-page-up))
  ("S-pagedown" (kbd-select kbd-page-down))

  ("structured:cmd delete" (remove-structure-upwards))
  ("structured:cmd backspace" (remove-structure-upwards))
  ("structured:cmd up" (kbd-select-if-active traverse-up))
  ("structured:cmd down" (kbd-select-if-active traverse-down))
  ("structured:cmd home" (kbd-select-if-active traverse-first))
  ("structured:cmd end" (kbd-select-if-active traverse-last))
  ("structured:cmd pageup" (kbd-select-if-active traverse-previous))
  ("structured:cmd pagedown" (kbd-select-if-active traverse-next))
  ("structured:cmd section" (traverse-previous-section-title))
  ("structured:cmd S-left" (kbd-select traverse-left))
  ("structured:cmd S-right" (kbd-select traverse-right))
  ("structured:cmd S-up" (kbd-select traverse-up))
  ("structured:cmd S-down" (kbd-select traverse-down))
  ("structured:cmd S-home" (kbd-select traverse-first))
  ("structured:cmd S-end" (kbd-select traverse-last))
  ("structured:cmd S-pageup" (kbd-select traverse-previous))
  ("structured:cmd S-pagedown" (kbd-select traverse-next))
  ("structured:cmd space" (kbd-select-enlarge))
  ("structured:cmd tab" (variant-circulate (focus-tree) #t))
  ("structured:cmd S-tab" (variant-circulate (focus-tree) #f))
  ("structured:cmd *" (alternate-toggle (focus-tree)))
  ("structured:cmd #" (numbered-toggle (focus-tree)))
  ("A-S-down" (variant-circulate (focus-tree) #t))
  ("A-S-up" (variant-circulate (focus-tree) #f))

  ("structured:move delete" (structured-exit-right))
  ("structured:move backspace" (structured-exit-left))
  ("structured:move left" (structured-left))
  ("structured:move right" (structured-right))
  ("structured:move up" (structured-up))
  ("structured:move down" (structured-down))
  ("structured:move home" (structured-start))
  ("structured:move end" (structured-end))
  ("structured:move pageup" (structured-top))
  ("structured:move pagedown" (structured-bottom))
  ("structured:move S-left" (kbd-select structured-left))
  ("structured:move S-right" (kbd-select structured-right))
  ("structured:move S-up" (kbd-select structured-up))
  ("structured:move S-down" (kbd-select structured-down))
  ("structured:move S-home" (kbd-select structured-start))
  ("structured:move S-end" (kbd-select structured-end))
  ("structured:move S-pageup" (kbd-select structured-top))
  ("structured:move S-pagedown" (kbd-select structured-bottom))

  ("structured:insert delete" (structured-remove-right))
  ("structured:insert backspace" (structured-remove-left))
  ("structured:insert left" (structured-insert-left))
  ("structured:insert right" (structured-insert-right))
  ("structured:insert up" (structured-insert-up))
  ("structured:insert down" (structured-insert-down))
  ("structured:insert home" (structured-insert-start))
  ("structured:insert end" (structured-insert-end))
  ("structured:insert pageup" (structured-insert-top))
  ("structured:insert pagedown" (structured-insert-bottom))

  ("structured:geometry delete" (geometry-reset))
  ("structured:geometry backspace" (geometry-reset))
  ("structured:geometry left" (geometry-left))
  ("structured:geometry right" (geometry-right))
  ("structured:geometry up" (geometry-up))
  ("structured:geometry down" (geometry-down))
  ("structured:geometry home" (geometry-start))
  ("structured:geometry end" (geometry-end))
  ("structured:geometry pageup" (geometry-top))
  ("structured:geometry pagedown" (geometry-bottom))
  ("structured:geometry tab" (geometry-circulate #t))
  ("structured:geometry S-tab" (geometry-circulate #f))
  ("structured:geometry [" (geometry-slower))
  ("structured:geometry ]" (geometry-faster))

  ("special left" (special-left))
  ("special right" (special-right))
  ("special up" (special-up))
  ("special down" (special-down))
  ("special home" (special-first))
  ("special end" (special-last))
  ("special pageup" (special-previous))
  ("special pagedown" (special-next))
  ("special return" (special-return))
  ("special S-return" (special-shift-return))
  ("special [" (special-back))
  ("special ]" (special-forward))
  
  ("altcmd \\" (make-hybrid))
  ("altcmd a" (make-tree))
  ("altcmd R" (make-rigid))
  ("altcmd =" (make 'hgroup))
  ("altcmd |" (make 'vgroup))
  ("altcmd :" (make 'line-break))
  ("altcmd ;" (make 'new-line))
  ("altcmd return" (make 'next-line))
  ("altcmd /" (make 'no-break))

  ("C-!" (make-label))
  ("C-?" (make 'reference))
  ("C-? var" (make 'eqref))
  ("C-? var var" (make 'pageref))

  ("extra e" (edit-focus-macro))
  ("extra r" (edit-previous-macro))
  ("extra m" (edit-focus-macro-source))
  ("extra p" (toggle-preamble-mode))
  ("extra s" (toggle-source-mode))

  ("accent:hat" "^")
  ("accent:deadhat" "^")
  ("accent:tilde" "~")
  ("accent:acute" "'")
  ("accent:grave" "`")

  ("symbol \\" "\\")
  ("symbol \"" "\"")
  ("symbol $" "$")
  ("symbol &" "&")
  ("symbol #" "#")
  ("symbol �" "�")
  ("symbol %" "%")
  ("symbol _" "_")
  ("symbol ^" "^")
  ("symbol (" "(")
  ("symbol )" ")")
  ("symbol [" "[")
  ("symbol ]" "]")
  ("symbol {" "{")
  ("symbol }" "}")
  ("symbol |" "|")

  ("undo" (noop) (undo 0))
  ("redo" (noop) (redo 0))
  ("cancel" (noop) (kbd-cancel))
  ("cut" (noop) (kbd-cut))
  ("paste" (noop) (kbd-paste))
  ("copy" (noop) (kbd-copy))
  ("find" (noop) (interactive-search))
  ("search find" (search-next-match #t))
  ("search again" (search-next-match #t))

  ("table N t" (make 'tabular))
  ("table N T" (make 'tabular*))
  ("table N w" (make 'wide-tabular))
  ("table N b" (make 'block))
  ("table N B" (make 'block*))
  ("table N W" (make 'wide-block)))

(kbd-map
  (:mode in-hybrid?)
  ("space" (hybrid-kbd-space))
  ("{" (hybrid-kbd-curly-left))
  ("}" (hybrid-kbd-curly-right))
  ("\\" (hybrid-kbd-backslash))
  ("_" (hybrid-kbd-sub))
  ("_ var" "_")
  ("^" (hybrid-kbd-sup))
  ("^ var" "^"))

(kbd-map
  (:mode in-smart-ref?)
  ("std ?" (make 'smart-ref))
  ("std ? var" (make 'reference))
  ("std ? var var" (make 'pageref)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile emacs)

  ;; standard Emacs shortcuts
  ("emacs a" (kbd-start-line))
  ("emacs b" (kbd-left))
  ;;("emacs d" (remove-text #t))
  ("emacs d" (kbd-delete))
  ("emacs e" (kbd-end-line))
  ("emacs f" (kbd-right))
  ("emacs g" (selection-cancel))
  ("emacs j" (insert-return))
  ("emacs k" (kill-paragraph))
  ("emacs l" (refresh-window))
  ("emacs m" (insert-return))
  ("emacs n" (kbd-down))
  ("emacs p" (kbd-up))
  ("emacs q" (make 'symbol))
  ("emacs r" (interactive-search))
  ("emacs s" (interactive-search))
  ("emacs v" (kbd-page-down))
  ("emacs w" (kbd-cut))
  ("emacs y" (kbd-paste))
  ("emacs insert" (kbd-copy))
  ("emacs _" (undo 0))
  ("emacs /" (undo 0))

  ("emacs:meta v" (kbd-page-up))
  ("emacs:meta w" (kbd-copy))
  ("emacs:meta x" (interactive exec-interactive-command))
  ("emacs:meta X" (interactive footer-eval))
  ("emacs:meta <" (go-start))
  ("emacs:meta >" (go-end))
  ("emacs:meta $" (interactive-spell))
  ("emacs:meta %" (interactive-replace))
  ("emacs:meta backspace" (backward-kill-word))
  ("emacs:meta delete" (kill-word))

  ("emacs:prefix b" (interactive go-to-buffer))
  ("emacs:prefix h" (select-all))
  ("emacs:prefix k" (close-document))
  ("emacs:prefix K" (close-document*))
  ("emacs:prefix C-c" (safely-quit-TeXmacs))
  ("emacs:prefix C-f" (interactive load-document))
  ("emacs:prefix C-i" (make 'indent))
  ("emacs:prefix C-s" (save-buffer))
  ("emacs:prefix C-w" (interactive save-buffer-as))

  ("search emacs s" (search-next-match #t))
  ("search emacs r" (search-next-match #f))

  ;; not implemented
  ;;("emacs h ..." (help ...))
  ;;("emacs l" (recenter-window))
  ;;("emacs o" (open-line))
  ;;("emacs t" (transpose-chars))
  ;;("emacs u" (universal-argument))
  ;;("emacs z" (suspend-texmacs))
  ;;("emacs \\" (toggle-input-method))
  ;;("emacs ]" (abort-recursive-edit))
  ;;("emacs:meta !" (shell-command))
  ;;("emacs:meta (" (insert-parentheses))
  ;;("emacs:meta )" (move-past-closed-and-reindent))
  ;;("emacs:meta *" (pop-tag-mark))                  ;; conflict altcmd *
  ;;("emacs:meta ," (loops-tag-continue))
  ;;("emacs:meta ." (find-tag))
  ;;("emacs:meta /" (dabbrev-expand))                ;; conflict altcmd /
  ;;("emacs:meta \\" (delete-horizontal-space))      ;; conflict altcmd \
  ;;("emacs:meta :" (interactive footer-eval))       ;; conflict altcmd :
  ;;("emacs:meta ;" (comment-dwim))                  ;; conflict altcmd ;
  ;;("emacs:meta =" (count-lines-region))
  ;;("emacs:meta {" (backward-paragraph))
  ;;("emacs:meta |" (shell-command-on-region))
  ;;("emacs:meta }" (forward-paragraph))
  ;;("emacs:meta @" (mark-word))
  ;;("emacs:meta a" (traverse-up))                   ;; conflict altcmd a
  ;;("emacs:meta b" (traverse-left))
  ;;("emacs:meta c" (capitalize-word))
  ;;("emacs:meta e" (traverse-down))                 ;; conflict altcmd e
  ;;("emacs:meta f" (traverse-right))                ;; conflict altcmd f
  ;;("emacs:meta h" (mark-paragraph))
  ;;("emacs:meta i" (tab-to-tab-stop))               ;; conflict altcmd i
  ;;("emacs:meta j" (indent-new-command-line))
  ;;("emacs:meta l" (downcase-word))                 ;; conflict altcmd l
  ;;("emacs:meta m" (back-to-indentation))
  ;;("emacs:meta q" (fill-paragraph))
  ;;("emacs:meta r" (move-to-window-line))
  ;;("emacs:meta t" (transpose-words))               ;; conflict altcmd t
  ;;("emacs:meta u" (upcase-word))
  ;;("emacs:meta y" (yank-pop))
  ;;("emacs:meta z" (zap-to-char))
  ;;("emacs:prefix delete" (backward-kill-sentence))
  ;;("emacs:prefix `" (next-error))
  ;;("emacs:prefix 0" (delete-window))
  ;;("emacs:prefix 1" (delete-other-windows))
  ;;("emacs:prefix 2" (split-window-vertically))
  ;;("emacs:prefix 3" (split-window-horizontally))
  ;;("emacs:prefix d" (dired))
  ;;("emacs:prefix f" (set-fill-column))
  ;;("emacs:prefix i" (interactive insert-buffer))
  ;;("emacs:prefix l" (count-lines-page))
  ;;("emacs:prefix m" (compose-mail))
  ;;("emacs:prefix o" (other-window))
  ;;("emacs:prefix s" (save-some-buffers))
  ;;("emacs:prefix u" (advertised-undo))
  ;;("emacs:prefix z" (repeat))
  ;;("emacs:prefix C-@" (pop-global-mark))
  ;;("emacs:prefix C-d" (list-directory))
  ;;("emacs:prefix C-e" (eval-last-expression))
  ;;("emacs:prefix C-l" (downcase-region))
  ;;("emacs:prefix C-n" (set-goal-column))
  ;;("emacs:prefix C-o" (delete-blank-lines))
  ;;("emacs:prefix C-p" (mark-page))
  ;;("emacs:prefix C-q" (toggle-read-only))
  ;;("emacs:prefix C-r" (interactive load-readonly-buffer))
  ;;("emacs:prefix C-t" (transpose-lines))
  ;;("emacs:prefix C-u" (upcase-region))
  ;;("emacs:prefix C-v" (interactive load-alternate-buffer))
  ;;("emacs:prefix C-x" (exchange-point-and-mark))
  ;;("emacs:prefix C-z" (suspend-texmacs))

  ;; further shortcuts for the Emacs mode
  ("F2" (open-document))
  ("S-F2" (open-document*))
  ("C-F2" (revert-buffer))
  ("M-F2" (new-document))
  ("M-S-F2" (new-document*))
  ;;("M-C-F2" (clone-window))
  ("F3" (save-buffer))
  ("S-F3" (choose-file save-buffer-as "Save TeXmacs file" "action_save_as"))
  ("F4" (preview-buffer))
  ("S-F4" (print-buffer))
  ("C-F4" (interactive print-to-file))
  ("M-F4" (interactive print-pages))
  ("M-S-F4" (interactive print-pages-to-file))

  ("emacs =" (interactive-replace))
  ("emacs:meta g" (kbd-cancel))
  ("emacs:meta [" (undo 0))
  ("emacs:meta ]" (redo 0))

  ("A-C-tab" (geometry-circulate #t))
  ("A-C-S-tab" (geometry-circulate #f))
  ("A-C-[" (geometry-slower))
  ("A-C-]" (geometry-faster))

  ("C-<" (cursor-history-backward))
  ("C->" (cursor-history-forward))
  ("C-!" (cursor-history-add (cursor-path)))
  ("C-#" (numbered-toggle (focus-tree)))
  ("C-*" (alternate-toggle (focus-tree)))
  ("C-%" (inactive-toggle (focus-tree)))
  ("C-+" (zoom-in (sqrt (sqrt 2.0))))
  ("C--" (zoom-out (sqrt (sqrt 2.0))))
  ("C-0" (change-zoom-factor 1.0))
  
  ("C-7" (fit-all-to-screen))
  ("C-8" (fit-to-screen))
  ("C-9" (fit-to-screen-width)))

(kbd-map
  (:profile emacs)
  (:require (and (not (in-prog?)) (not (in-verbatim?))))
  ("A-tab" (kbd-alternate-tab))
  ("A-S-tab" (kbd-shift-alternate-tab))
  ("A-space" (make-space "0.2spc"))
  ("A-S-space" (make-space "-0.2spc"))
  ("M-space" (make-space "0.2spc"))
  ("M-S-space" (make-space "-0.2spc"))
  ("M-tab" (kbd-alternate-tab))
  ("M-S-tab" (kbd-shift-alternate-tab)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnome keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile gnome)

  ;; standard Gnome shortcuts
  ("gnome d" (remove-text #t))
  ("gnome h" (interactive-replace))
  ("gnome k" (kill-paragraph))
  ("gnome left" (traverse-left))
  ("gnome right" (traverse-right))
  ("gnome home" (go-start))
  ("gnome end" (go-end))
  ("gnome S-left" (kbd-select traverse-left))
  ("gnome S-right" (kbd-select traverse-right))
  ("gnome S-home" (kbd-select go-start))
  ("gnome S-end" (kbd-select go-end))

  ("F14" (undo 0))
  ("F16" (kbd-copy))
  ("F18" (kbd-paste))
  ("F20" (kbd-cut))
  ("C-insert" (kbd-copy))
  ("S-insert" (kbd-paste))
  ("S-delete" (kbd-cut))
  ("gnome c" (kbd-copy))
  ("gnome v" (kbd-paste))
  ("gnome x" (kbd-cut))

  ("search F3" (search-next-match #t))
  ("search S-F3" (search-next-match #f))
  ("search gnome g" (search-next-match #t))
  ("search gnome G" (search-next-match #f))

  ;; not yet implemented
  ;;("gnome delete" (delete-end-word))
  ;;("gnome backspace" (delete-start-word))
  ;;("forward" (next-tab))
  ;;("back" (previous-tab))

  ;; further shortcuts for Gnome look and feel
  ("gnome g" (selection-cancel))
  ("gnome l" (refresh-window))
  ("gnome F" (interactive-search))

  ("cmd q" (make 'symbol))
  ("altcmd g" (kbd-cancel))
  ("altcmd x" (interactive footer-eval))
  ("A-x" (interactive exec-interactive-command))
  ("altcmd $" (interactive-spell))

  ("C-P" (toggle-preamble-mode))
  ("C-O" (toggle-source-mode))

  ("structured:cmd left" (kbd-select-if-active traverse-left))
  ("structured:cmd right" (kbd-select-if-active traverse-right))

  ("M-A-C-home" (traverse-first))
  ("M-A-C-end" (traverse-last))
  ("M-A-C-S-home" (kbd-select traverse-first))
  ("M-A-C-S-end" (kbd-select traverse-last)))

(kbd-map
  (:profile gnome)
  (:require (and (not (in-prog?)) (not (in-verbatim?))))
  ("M-tab" (kbd-alternate-tab))
  ("M-S-tab" (kbd-shift-alternate-tab))
  ("M-space" (make-space "0.2spc"))
  ("M-S-space" (make-space "-0.2spc")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KDE keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile kde)

  ;; standard KDE shortcuts
  ("kde d" (remove-text #t))
  ("kde k" (kill-paragraph))
  ("kde r" (interactive-replace))
  ("kde left" (traverse-left))
  ("kde right" (traverse-right))
  ("kde home" (go-start))
  ("kde end" (go-end))
  ("kde S-left" (kbd-select traverse-left))
  ("kde S-right" (kbd-select traverse-right))
  ("kde S-home" (kbd-select go-start))
  ("kde S-end" (kbd-select go-end))

  ("F14" (undo 0))
  ("F16" (kbd-copy))
  ("F18" (kbd-paste))
  ("F20" (kbd-cut))
  ("C-insert" (kbd-copy))
  ("S-insert" (kbd-paste))
  ("S-delete" (kbd-cut))

  ("search F3" (search-next-match #t))
  ("search S-F3" (search-next-match #f))

  ;; not yet implemented
  ;;("kde N" (add-tab))
  ;;("kde delete" (delete-end-word))
  ;;("kde backspace" (delete-start-word))
  ;;("forward" (next-tab))
  ;;("back" (previous-tab))

  ;; further shortcuts for KDE look and feel
  ("kde g" (selection-cancel))
  ("kde l" (refresh-window))
  ("kde F" (interactive-search))

  ("cmd q" (make 'symbol))
  ("altcmd g" (kbd-cancel))
  ("altcmd x" (interactive footer-eval))
  ("A-x" (interactive exec-interactive-command))
  ("altcmd $" (interactive-spell))

  ("C-P" (toggle-preamble-mode))
  ("C-O" (toggle-source-mode))

  ("structured:cmd left" (kbd-select-if-active traverse-left))
  ("structured:cmd right" (kbd-select-if-active traverse-right))

  ("M-A-C-home" (traverse-first))
  ("M-A-C-end" (traverse-last))
  ("M-A-C-S-home" (kbd-select traverse-first))
  ("M-A-C-S-end" (kbd-select traverse-last)))
  
(kbd-map
  (:profile kde)
  (:require (and (not (in-prog?)) (not (in-verbatim?))))
  ("M-tab" (kbd-alternate-tab))
  ("M-S-tab" (kbd-shift-alternate-tab))
  ("M-space" (make-space "0.2spc"))
  ("M-S-space" (make-space "-0.2spc")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac OS keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile macos)

  ;; standard macOS keyboard shortcuts
  ("macos ;" (interactive-spell))
  ("macos ?" (interactive docgrep-in-doc))
  ("macos [" (cursor-history-backward))
  ("macos ]" (cursor-history-forward))
  ("macos _" (make 'nbhyph))
  ("macos up" (go-start))
  ("macos down" (go-end))
  ("macos left" (kbd-select-if-active traverse-left))
  ("macos right" (kbd-select-if-active traverse-right))
  ("macos S-left" (kbd-select kbd-start-line))
  ("macos S-right" (kbd-select kbd-end-line))
  ("macos S-up" (kbd-select go-start))
  ("macos S-down" (kbd-select go-end))

  ("search macos g" (search-next-match #t))
  ("search macos G" (search-next-match #f))

  ;; not yet supported
  ;;("macos :" (display-spelling-window))
  ;;("macos ," (open-preferences))
  ;;("macos A-/" (toggle-antialising))
  ;;("macos #" (capture-screen-to-file))
  ;;("macos C-#" (capture-screen-to-clipboard))
  ;;("macos $" (capture-selection-to-file))
  ;;("macos C-$" (capture-selection-to-clipboard))
  ;;("macos C" (show-colors-window))
  ;;("macos C-c" (copy-style))
  ;;("macos A-c" (copy-formatting))
  ;;("macos C-d" (show-definition-word))
  ;;("macos A-d" (toggle-doc))
  ;;("macos e" (search-selection))
  ;;("macos h" (hide-window))
  ;;("macos A-h" (hide-other-windows))
  ;;("macos A-i" (show-inspector-window))
  ;;("macos j" (scroll-to-selection))
  ;;("macos m" (minimize-window))
  ;;("macos A-m" (minimize-all-windows))
  ;;("macos P" (printer-setup))
  ;;("macos t" (show-fonts-window))
  ;;("macos A-t" (toggle-toolbar))
  ;;("macos C-v" (paste-style))
  ;;("macos C-V" (paste-match-style))
  ;;("macos A-v" (paste-formatting))
  ;;("macos A-w" (safely-kill-all-windows))
  ;;("macos C-x" (cut-style))       ;; TeXmacs addition
  ;;("macos A-x" (cut-formatting))  ;; TeXmacs addition

  ;; further shortcuts for MacOS look and feel
  ("macos r" (interactive-replace))
  ("macos F" (toggle-full-screen-mode))
  ("macos C-f" (toggle-full-screen-edit-mode))

  ("macos S-=" (zoom-in (sqrt (sqrt 2.0))))
  ("macos S-+" (zoom-in (sqrt (sqrt 2.0))))
  ("macos S--" (zoom-out (sqrt (sqrt 2.0))))
  ("macos S-_" (zoom-out (sqrt (sqrt 2.0))))

  ("altcmd x" (interactive footer-eval))
  ("A-x" (interactive exec-interactive-command))

  ("A-space var" (make 'nbsp))

  ("C-a" (kbd-start-line))
  ("C-e" (kbd-end-line))
  ("C-b" (kbd-left))
  ("C-f" (kbd-right))
  ("C-g" (selection-cancel))
  ("C-k" (kill-paragraph))
  ("C-l" (refresh-window))
  ("C-y" (yank-paragraph))
  ("A-q" (make 'symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile windows)

  ;; standard Windows shortcuts
  ("windows F4" (close-document))
  ("windows S-F4" (close-document*))
  ("windows left" (traverse-left))
  ("windows right" (traverse-right))
  ("windows home" (go-start))
  ("windows end" (go-end))
  ("windows S-left" (kbd-select traverse-left))
  ("windows S-right" (kbd-select traverse-right))
  ("windows S-home" (kbd-select go-start))
  ("windows S-end" (kbd-select go-end))
  ("windows S-space" (make 'nbsp))
  ("windows _" (make 'nbhyph))
  ("windows A-." "<ldots>")
  ("windows A-c" (make 'copyright))
  ("windows e" (make 'footnote))
  ("windows F" (make 'footnote))
  ("windows h" (interactive-replace))
  ("windows K" (toggle-small-caps))
  ("windows A-r" (make 'registered))
  ("windows A-t" (make 'trademark))
  ("windows y" (redo 0))

  ("F2" (interactive-replace))
  ("S-delete" (kbd-cut))
  ("S-insert" (kbd-paste))
  ("C-insert" (kbd-copy))
  ("A-F4" (close-document))
  ("A-S-F4" (close-document*))

  ("search windows g" (search-next-match #t))
  ("search windows G" (search-next-match #f))
  ("search F3" (search-next-match #t))
  ("search S-F3" (search-next-match #f))

  ;; not yet implemented
  ;;("F4" (go-to-different-folder))
  ;;("F5" (refresh-window))
  ;;("F6" (switch-to-next-pane))
  ;;("F8" (kbd-select-enlarge))
  ;;("F9" (refresh-web-page))
  ;;("C-F9" (insert-field))
  ;;("F10" (menu-bar-options))
  ;;("S-F10" (open-contextual-menu))
  ;;("windows F4" (close-mdi-window))
  ;;("windows F6" (next-tab))
  ;;("windows S-F6" (previous-tab))
  ;;("windows delete" (delete-end-word))
  ;;("windows backspace" (delete-start-word))
  ;;("windows tab" (switch-to-next-child))
  ;;("windows escape" (open-start-menu))
  ;;("windows S-escape" (open-task-manager))
  ;;("windows S-return" (insert-section-break))
  ;;("windows C" (copy-formatting))
  ;;("windows D" (insert-endnote))
  ;;("windows E" (review-toggle-track-changes))
  ;;("windows g" (go-to-location))
  ;;("windows A-m" (review-insert-comment))
  ;;("windows V" (paste-formatting))
  ;;("windows A-y" (search-repeat))
  ;;("windows <" (decrease-font-size))
  ;;("windows >" (increase-font-size))
  ;;("windows [" (decrease-font-size-one-point))
  ;;("windows ]" (increase-font-size-one-point))
  ;;("windows ` `" (open-single-quotation))
  ;;("windows ' '" (close-single-quotation))
  ;;("windows ` C-`" (open-double-quotation))
  ;;("windows ' C-'" (close-double-quotation))
  ;;("S-delete" (delete-selection-immediately))
  ;;("A-F6" (switch-to-next-window))
  ;;("A-tab" (switch-to-next-program))
  ;;("A-down" (open-drop-down-list-box))
  ;;("A-space" (open-system-menu))
  ;;("A-return" (open-properties-window))
  ;;("A-I" (insert-citation-entry))
  ;;("A-O" (insert-toc-entry))
  ;;("A-X" (insert-index-entry))
  ;;("A--" (open-child-system-menu))
  ;;("A-_" (open-menu))
  ;;("M-F1" (run-dialog-box))
  ;;("M-tab" (cycle-taskbar-button))
  ;;("M-space" (show-keyboard-shortcuts))
  ;;("M-c" (open-control-panel))
  ;;("M-d" (minimize-all-open-windows))
  ;;("M-e" (open-explorer-window))
  ;;("M-C-f" (find-computer))
  ;;("M-f" (open-finder-window))
  ;;("M-i" (open-mouse-properties-window))
  ;;("M-k" (open-keyboard-properties-window))
  ;;("M-l" (log-off-windows))
  ;;("M-m" (minimize-all-windows))
  ;;("M-M" (unminimize-all-windows))
  ;;("M-p" (start-print-manager))
  ;;("M-r" (run-dialog-box))
  ;;("M-s" (toggle-caps-lock))
  ;;("M-v" (start-clipboard))
  ;;("forward" (next-tab))
  ;;("back" (previous-tab))

  ;; further shortcuts for Windows look and feel
  ("windows g" (selection-cancel))
  ("windows l" (refresh-window))
  ("windows =" (change-zoom-factor 1.0))

  ("cmd q" (make 'symbol))
  ("altcmd g" (kbd-cancel))
  ("altcmd x" (interactive footer-eval))
  ("A-x" (interactive exec-interactive-command))
  ("altcmd $" (interactive-spell))

  ("structured:cmd left" (kbd-select-if-active traverse-left))
  ("structured:cmd right" (kbd-select-if-active traverse-right))

  ("M-A-C-home" (traverse-first))
  ("M-A-C-end" (traverse-last))
  ("M-A-C-S-home" (kbd-select traverse-first))
  ("M-A-C-S-end" (kbd-select traverse-last)))

(kbd-map
  (:profile windows)
  (:require (and (not (in-prog?)) (not (in-verbatim?))))
  ("M-tab" (kbd-alternate-tab))
  ("M-S-tab" (kbd-shift-alternate-tab))
  ("M-space" (make-space "0.2spc"))
  ("M-S-space" (make-space "-0.2spc")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard cross-platform keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile std)

  ;; standard cross-platform shortcuts
  ("std a" (select-all))
  ("std b" (toggle-bold))
  ("std c" (kbd-copy))
  ("std f" (interactive-search))
  ("std i" (toggle-italic))
  ("std t" (new-document))
  ("std n" (new-document))
  ("std N" (new-document*))
  ("std o" (open-document))
  ("std L" (open-document*))
  ("std p" (preview-buffer))
  ("std q" (safely-quit-TeXmacs))
  ("std R" (update-document "all"))
  ("std s" (save-buffer))
  ("std S" (choose-file save-buffer-as "Save TeXmacs file" "action_save_as"))
  ("std u" (toggle-underlined))
  ("std v" (kbd-paste))
  ("std V" (kbd-magic-paste))
  ("std w" (close-document))
  ("std W" (close-document*))
  ("std x" (kbd-cut))
  ("std z" (undo 0))
  ("std Z" (redo 0))
  ("std +" (zoom-in (sqrt (sqrt 2.0))))
  ("std =" (zoom-in (sqrt (sqrt 2.0))))
  ("std -" (zoom-out (sqrt (sqrt 2.0))))
  ("std 0" (change-zoom-factor 1.0))
  ("std 1" (switch-to-view-index 0))
  ("std 2" (switch-to-view-index 1))
  ("std 3" (switch-to-view-index 2)) 
  ("std 4" (switch-to-view-index 3))
  ("std 5" (switch-to-view-index 4))
  ("std 6" (switch-to-view-index 5)) 

  ;; not yet implemented
  ;;("std t" (add-tab))
  ;;("std tab" (next-tab))
  ;;("std S-tab" (previous-tab))

  ;; extras
  ("std 7" (fit-all-to-screen))
  ("std 8" (fit-to-screen))
  ("std 9" (fit-to-screen-width))
  ("search std f" (search-next-match #t))   ;; added for convenience
  ("search std F" (search-next-match #f)))  ;; added for convenience
