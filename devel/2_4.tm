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

    <item>Click <menu|Format|Whitespace|Stretchable>
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

  <subsection|The root cause>

  It is related to the migration from GNU Guile to S7 Scheme.

  The root cause of this bug is that the result of <scm|(procedure-name
  make-space)> is <scm|#f>.

  Here is the expected result

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (procedure-name make-space)
    <|unfolded-io>
      make-space
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <subsection|How to fix it>

  Based on the fact that, for all glued symbols, it must starts with alpha:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (filter\ 

      \ (lambda (x) (not x))

      \ (map

      \ \ (lambda (x) (string-alpha? (string-take x 1)))

      \ \ (all-glued-symbols)))
    <|unfolded-io>
      ()
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  That's the reason why I made this fix.

  <subsection|How to test it>

  <\session|scheme|default>
    <\input|Scheme] >
      (regtest-tm-define)
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

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