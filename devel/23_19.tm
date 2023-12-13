<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Focus toolbar disappears on <menu|Insert|Long text>>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Darcy on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I8NIYS>

    <item>Owner: Darcy

    <item>Tester: Darcy
  </itemize>

  <section|How to reproduce it>

  <\enumerate>
    <item><menu|Insert|Image|Draw image>

    <item><menu|Insert|Long text> to enter the long text mode
  </enumerate>

  Click in the graphics mode, you will find that the icons in the graphics
  toolbar disappear.

  <section|Solution>

  <subsection|The root cause>

  Here is the error message:

  <\scm-code>
    ;equal?: not enough arguments: (equal? "mixed")

    ; \ \ \ (== "mixed")

    ; \ \ \ /Users/da/git/mogan/TeXmacs/progs/graphics/graphics-menu.scm,
    line 1130, position: 46158

    ; (== "mixed")

    ; (col "none") \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ;
    col: "none"

    ; (($list ($menu-link doc-at-fill-color-men...

    ; (list-values 'gui-normalize (list-values ...
  </scm-code>

  <subsection|How to fix it>

  <subsection|How to test it>

  <tmdoc-copyright|2023|<value|da>>

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