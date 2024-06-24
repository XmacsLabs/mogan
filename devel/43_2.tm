<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|messy encoding when <menu|Edit|Copy to|LaTeX>>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: animerqi on <hlink|gitee|https://gitee.com/XmacsLabs/mogan/issues/I83Y8F>

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|How to reproduce it>

  Select the following two lines, and then click <menu|Edit|Copy to|LaTeX>,
  and then paste, you will find that the encoding of the first line is not
  correct.

  \<#8FD9\>\<#662F\>\<#4E00\>\<#4E2A\>\<#57FA\>\<#672C\>\<#7684\>\<#5E42\>\<#7EA7\>\<#6570\>

  <\equation*>
    <big|sum><rsub|n=0><rsup|\<infty\>>x<rsup|n>=<frac|1|1-x>
  </equation*>

  \;

  <section|Description>

  The behavior is controlled by two converters:

  <\itemize>
    <item><scm|texmacs-stree> \<rightarrow\> <scm|latex-stree>

    <item><scm|latex-\<gtr\>stree> \<rightarrow\> <scm|latex-snippet>
  </itemize>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (stree-\<gtr\>tree `(frac "\<#5206\>\<#5B50\>" "\<#5206\>\<#6BCD\>"))
    <|unfolded-io>
      <text|<frac|\<#5206\>\<#5B50\>|\<#5206\>\<#6BCD\>>>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (texmacs-\<gtr\>latex `(frac "\<#5206\>\<#5B50\>" "\<#5206\>\<#6BCD\>")
      `())
    <|unfolded-io>
      (!group (frac (!concat (!widechar (symbol "å\\x88;\\x86;")) (!widechar
      (symbol "å­\\x90;"))) (!concat (!widechar (symbol "å\\x88;\\x86;"))
      (!widechar (symbol "æ¯\\x8d;")))))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (serialize-latex (texmacs-\<gtr\>latex `(frac "\<#5206\>\<#5B50\>"
      "\<#5206\>\<#6BCD\>") `()))
    <|unfolded-io>
      "{\\\\frac{å\\x88;\\x86;å­\\x90;}{å\\x88;\\x86;æ¯\\x8d;}}"
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  It seems the bugs should lie in the converter from <scm|texmacs-stree> to
  <scm|latex-stree>.

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