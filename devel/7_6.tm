<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|<key|S-return> freezes the structured search window>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: <value|jk> on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I7CBUC>

    <item>Owner: <value|da>

    <item>Tester: <value|da>
  </itemize>

  <section|How to reproduce it>

  <menu|Edit|Search<text-dots>> and then activate the structured search
  window, and then press <key|S-return>, Mogan will freeze.

  <section|Solution>

  <subsection|The root cause>

  Inifinite loop.

  <subsection|How to fix it>

  Fix the inifinite loop.

  <subsection|How to test it>

  just follow the reproduce step.

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