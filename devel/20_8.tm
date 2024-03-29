<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese>>

<\body>
  <tmdoc-title|Failed to insert full-width punctuation via input method>

  <section|Bug metadata>

  <\itemize>
    <item>Owner: Darcy

    <item>Gitee: <slink|https://gitee.com/XmacsLabs/mogan/issues/I7AUTV>
  </itemize>

  <section|Description>

  When insert a full-width punctuation via input method, on macOS and
  Windows, it failed with the following error message:

  <\scm-code>
    ;unbound variable keyboard-speech

    ; (== (get-input-mode) 1)

    ; (key-press key) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ;
    key: "speech:\<less\>#FF08\<gtr\>"

    ; ((with after? (in-inactive-reference?) (w... ; before?: #f
  </scm-code>

  <\scm-code>
    ;unbound variable keyboard-speech

    ; (== (get-input-mode) 1)

    ; (key-press key) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ;
    key: "speech:\\x10;"

    ; ((with after? (in-inactive-reference?) (w... ; before?: #f
  </scm-code>

  <subsection|How to debug>

  Enable <verbatim|debug-qt> and <verbatim|debug-keyboard>, here is the debug
  message:

  <\verbatim-code>
    TeXmacs] debug-qt, FOCUSIN: simple_widget \ \ \ \ \ \ \ id: 3

    TeXmacs] debug-keyboard, Got focus at 37388

    TeXmacs] debug-qt, keypressed

    TeXmacs] debug-qt, key \ : 16777248

    TeXmacs] debug-qt, text :

    TeXmacs] debug-qt, count: 0

    TeXmacs] debug-qt, unic : 0

    TeXmacs] debug-qt, nativeScanCode: 42

    TeXmacs] debug-qt, nativeVirtualKey: 16

    TeXmacs] debug-qt, nativeModifiers: 1

    TeXmacs] debug-qt, shift

    TeXmacs] debug-qt, IM committing: \Q

    TeXmacs] debug-qt, keypressed

    TeXmacs] debug-qt, key \ : 0

    TeXmacs] debug-qt, text : speech:?

    TeXmacs] debug-qt, count: 8

    TeXmacs] debug-qt, unic : 115

    TeXmacs] debug-qt, nativeScanCode: 0

    TeXmacs] debug-qt, nativeVirtualKey: 0

    TeXmacs] debug-qt, nativeModifiers: 0

    TeXmacs] debug-qt, key press: speech:

    TeXmacs] debug-qt, IM preediting :

    TeXmacs] debug-keyboard, Pressed speech: at 38950

    TeXmacs] debug-keyboard, \ \ Codes 115 112 101 101 99 104 58 17

    TeXmacs] debug-keyboard, Pressed pre-edit: at 38950

    TeXmacs] debug-keyboard, \ \ Codes 112 114 101 45 101 100 105 116 58

    TeXmacs] debug-qt, keypressed

    TeXmacs] debug-qt, key \ : 16777251

    TeXmacs] debug-qt, text :

    TeXmacs] debug-qt, count: 0

    TeXmacs] debug-qt, unic : 0

    TeXmacs] debug-qt, nativeScanCode: 56

    TeXmacs] debug-qt, nativeVirtualKey: 18

    TeXmacs] debug-qt, nativeModifiers: 4

    TeXmacs] debug-qt, alt

    TeXmacs] debug-qt, FOCUSOUT: simple_widget \ \ \ \ \ \ id: 3

    TeXmacs] debug-keyboard, Lost focus at 39363
  </verbatim-code>

  <subsection|Git Blame>

  Here is the commit which introduced the bug:

  <slink|https://gitee.com/XmacsLabs/mogan/commit/86aa58981ab538ac80fe86f74041121e1f541502>

  <tmdoc-copyright|2023|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>