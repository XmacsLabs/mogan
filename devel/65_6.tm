<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Qt 6: Failed to recognize <key|altcmd> and <key|extra>>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Darcy Shen on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I8E9O9>

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|How to reproduce it>

  <\big-table|<tabular|<tformat|<table|<row|<cell|Key>|<cell|Menu>>|<row|<cell|<key|extra
  s>>|<cell|<menu|Document|Source|Edit source tree>>>|<row|<cell|<key|extra
  p>>|<cell|<menu|Document|Part|Create preamble>>>|<row|<cell|<key|altcmd
  f>>|<cell|<menu|View|Full screen mode>>>>>>>
    Several cases to reproduce the problem
  </big-table>

  Using Mogan Research v1.2.0-rc1, it does not work. Using Mogan v1.1.6, it
  works fine.

  <section|Solution>

  <subsection|The root cause>

  In <cpp|QTMWidget::keyPressEvent> of src/Plugins/Qt/QTMWidget.cpp, the key
  combination failed to recognize the combinations: <key|altcmd> and
  <key|extra>.

  <subsection|How to fix it>

  <subsection|How to test it>

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