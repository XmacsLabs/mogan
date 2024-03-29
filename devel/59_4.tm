<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Failed to show <key|C-&> in the Qt menu>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Darcy on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I5NBFH>

    <item>Owner: Darcy

    <item>Tester: Darcy
  </itemize>

  <section|How to reproduce it>

  See <menu|Insert|Mathematics|Several equations>.

  <section|Solution>

  <subsection|The root cause>

  <verbatim|&> is a special symbol in Qt.

  <subsection|How to fix it>

  Just port the fix from branch-1.1:

  <slink|https://gitee.com/XmacsLabs/mogan/commit/744c08f6cc180223b2bb3013cd473fdfd01d9aa6>

  <tmdoc-copyright|2023|Who>

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