<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Mogan crashes after updating pages>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter:

    <\itemize>
      <item>pdiazs on <hlink|Github|https://github.com/XmacsLabs/mogan/issues/1142>

      <item>bimalgaudel on <hlink|Github|https://github.com/XmacsLabs/mogan/issues/1487>

      <item>Darcy Shen on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I8ZES8>
    </itemize>

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|How to reproduce it>

  Generate a big document, and click <menu|Document|Update|All> to reproduce
  it.

  <section|Solution>

  <subsection|The root cause>

  It is using the list container for the change_log, for large doc, big list
  will crash because of the limitation of the stack memory size.

  <subsection|How to fix it>

  <\itemize>
    <item>2023/10/13: Use array instead of list
    <slink|https://github.com/XmacsLabs/mogan/pull/1296>

    <item>2023/12/19: Revert \PUse array instead of list\Q
    \ <slink|https://github.com/XmacsLabs/mogan/pull/1493>

    <item>2023/12/19: Second attempt to use array instead of list
    <slink|https://github.com/XmacsLabs/mogan/pull/1494>

    <item>2023/12/23: Third attempt to fix the issue
    <slink|https://github.com/XmacsLabs/mogan/pull/1504>

    <item>2024/02/09: Revert again <slink|https://github.com/XmacsLabs/mogan/pull/1646>

    <item>2024/02/09: Final fix <slink|https://github.com/XmacsLabs/mogan/pull/1647>
  </itemize>

  <subsection|How to test it>

  See <slink|../TeXmacs/tests/tm/7_2_1.tm>, just delete the first line, the
  bug of the third attempt will appear.

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