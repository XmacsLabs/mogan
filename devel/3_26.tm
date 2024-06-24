<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|comment>>

<\body>
  <tmdoc-title|error while loading shared library libmogan.so>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: woutersj on <hlink|Github|https://github.com/XmacsLabs/mogan/issues/531>

    <item>Owner: woutersj

    <item>Tester: woutersj
  </itemize>

  <section|How to reproduce it>

  If we build and install Mogan without:

  <\shell-code>
    xmake config -m debug --yes
  </shell-code>

  And launch it we may receive the following error message:

  <\shell-code>
    <code*|build/package/bin/mogan: error while loading shared libraries:
    libmogan.so: cannot open shared object file: No such file or directory>
  </shell-code>

  <section|Description>

  <subsection|The root cause>

  The root cause is that during link time, xmake will choose the
  system-installed <shell|libmogan.so> rather than the one built by the user.

  <subsection|How to fix it>

  <\unfolded-comment|+11fbTSb226sSEve4|+11fbTSb226sSEve5|comment|woutersj|1683900241|>
    \;

    The <hlink|target:add_rpathdirs option|https://xmake.io/#/manual/project_target?id=targetadd_rpathdirs>
    seems useful.

    Adding <code*|add_rpathdirs("@executable_path/../lib")> to
    <code*|target("mogan")> fixes it for me.
  </unfolded-comment>

  <subsection|How to test it>

  Just follow the guide in how to reproduce it.

  <tmdoc-copyright|2023|Darcy Shen|wourtersj>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|global-title|error while loading shared library libmogan.so>
    <associate|page-medium|papyrus>
  </collection>
</initial>