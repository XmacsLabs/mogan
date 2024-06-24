<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Code: fix copy/paste unicode in scheme code block>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Darcy on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I928PB>

    <item>Owner: <value|da>

    <item>Tester: <value|da>
  </itemize>

  <section|How to reproduce it>

  Copy the first line and paste the to the second line:

  <\scm-code>
    \<#4E2D\>\<#6587\>

    \;
  </scm-code>

  <section|Solution>

  <subsection|The root cause>

  <scm|kbd-copy> and <scm|kbd-paste> in scheme prog mode is wrong.

  <subsection|How to fix it>

  Remove <scm|kbd-copy> and <scm|kbd-paste> in scheme prog mode.

  <subsection|How to test it>

  See how to reproduce it.

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