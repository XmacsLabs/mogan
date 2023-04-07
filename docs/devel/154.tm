<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Failed to insert <menu|Insert|Format|Whitespace|Stretchable<text-dots>>>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Yiqi Xu on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I6R51R>

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|How to reproduce it>

  <\enumerate>
    <item>Launch Mogan via <shell|<code|<code*|/usr/bin/mogan>>> in terminal
    on GNU/Linux

    <item>Click <menu|Insert|Format|Whitespace|Stretchable>
  </enumerate>

  Here is the error message:

  <\scm-code>
    ;procedure-source argument, #f, is boolean but should be a procedure or a
    macro

    ; \ \ \ (procedure-source fun)

    ; \ \ \ /share/Xmacs/progs/kernel/texmacs/tm-dialogue.scm, line 226,
    position: 7448

    ; compute-interactive-args-try-hard: (proce... ; fun: #f

    ; ((with fun-args (build-interactive-args f... ; args: ()

    ; make-menu-entry-button: (protected-call (... ; style: 0
  </scm-code>

  <section|Description>

  It is related to the migration from GNU Guile to S7 Scheme.

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