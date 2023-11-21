<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Inconsist encoding to include a file with unicode filename>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Darcy

    <item>Owner: Darcy

    <item>Tester: Darcy
  </itemize>

  <section|How to reproduce it>

  See <slink|../TeXmacs/tests/69_5.tm>. The filename to include contains
  Chinese, it should be in cork encoding like the normal Chinese text once
  included. When we change the filename in the focus toolbar, its encoding
  will be changed into cork encoding. But the inclusion failed to work.

  <tmdoc-copyright|2023|Darcy>

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