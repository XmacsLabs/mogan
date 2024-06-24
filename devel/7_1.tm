<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Disable case sensitivity only accepts lower case letters>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: LoETR9 on <hlink|GNU TeXmacs
    forum|http://forum.texmacs.cn/t/version-2-crashes-on-opensuse-kde/1345>

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|How to reproduce it>

  <\enumerate>
    <item>Launch Mogan via <shell|<code|<code*|LC_PAPER=A4 /usr/bin/mogan>>>
    on GNU/Linux

    <item>Click <menu|Help|Welcome>
  </enumerate>

  Here is the error message:

  <\verbatim-code>
    terminate called after throwing an instance of 'std::runtime_error'

    \ \ what(): \ locale::facet::_S_create_c_locale name not valid

    [1] \ \ \ 1117551 IOT instruction (core dumped) \ LC_PAPER=A4
    /usr/bin/mogan
  </verbatim-code>

  <section|Description>

  Here is the code snippet to crash Mogan:

  <\cpp-code>
    std::locale previous= std::locale::global (std::locale(""));

    string charset= string (nl_langinfo (CODESET));

    std::locale::global (previous);
  </cpp-code>

  Just return <cpp|UTF-8> to fix it.

  <tmdoc-copyright|2022|Darcy Shen>

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