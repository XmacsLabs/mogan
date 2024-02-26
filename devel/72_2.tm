<TeXmacs|2.1.2>

<style|<tuple|tmdoc|devel|chinese>>

<\body>
  <tmdoc-title|Failed to render CJK characters in scheme session>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: <value|da> on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I90SOL>

    <item>Owner: <value|da>

    <item>Tester: <value|da>
  </itemize>

  <section|How to reproduce it>

  <\session|scheme|default>
    <\input|Scheme] >
      (define zh "\<#4E2D\>\<#6587\>")
    </input>

    <\input|Scheme] >
      zh
    </input>

    <\input|Scheme] >
      (define \<#8FB9\>\<#957F\> 4)
    </input>

    <\input|Scheme] >
      \<#8FB9\>\<#957F\>
    </input>

    <\input|Scheme] >
      (define (\<#6C42\>\<#5E73\>\<#65B9\> x) (* x x))
    </input>

    <\input|Scheme] >
      (\<#6C42\>\<#5E73\>\<#65B9\>\<#6839\> 2)
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Solution>

  <subsection|The root cause>

  The routine <scm|scheme-eval> failed to render CJK characters.

  <subsection|How to fix it>

  Fix it via the special cases.

  <subsection|How to test it>

  see How to reproduce it.

  <tmdoc-copyright|2024|<value|da>>

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