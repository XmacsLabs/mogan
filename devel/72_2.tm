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
    <\unfolded-io|Scheme] >
      (define zh "\<#4E2D\>\<#6587\>")
    <|unfolded-io>
      "ф╕нц\\x96;\\x87;"
    </unfolded-io>

    <\unfolded-io|Scheme] >
      zh
    <|unfolded-io>
      "ф╕нц\\x96;\\x87;"
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (define \<#8FB9\>\<#957F\> 4)
    <|unfolded-io>
      4
    </unfolded-io>

    <\unfolded-io|Scheme] >
      \<#8FB9\>\<#957F\>
    <|unfolded-io>
      4
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Solution>

  <subsection|The root cause>

  <subsection|How to fix it>

  <subsection|How to test it>

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