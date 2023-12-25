<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|<inactive|<range|1|3|123>> crashes Mogan>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: <value|da> on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I8DT0J>

    <item>Owner: <value|da>

    <item>Tester: <value|da>
  </itemize>

  <section|How to reproduce it>

  As the title describes.

  <section|Solution>

  <subsection|The root cause>

  Error handling of <cpp|typeset_range> is insufficient.

  <subsection|How to fix it>

  Improve error handling of <cpp|typeset_range>.

  <subsection|How to test it>

  <\description>
    <item*|case 1>normal cases

    <\itemize>
      <item><inactive|<range|123456|0|1>> \<rightarrow\> <range|123456|0|1>

      <item><inactive|<range|123456|0|2>> \<rightarrow\> <range|123456|0|2>

      <item><inactive|<range|123456|1|4>> \<rightarrow\> <range|123456|1|4>
    </itemize>

    <item*|case 2>out of range

    <\itemize>
      <item>bigger right <inactive|<range|123456|1|10>> \<rightarrow\>
      <range|123456|1|10>

      <item>smaller right <inactive|<range|123456|1|-1>> \<rightarrow\>
      <range|123456|1|-1>

      <item>bigger left <inactive|<range|123456|10|3>> \<rightarrow\>
      <range|123456|10|3>

      <item>smaller left <inactive|<range|123456|-1|3>> \<rightarrow\>
      <range|123456|-1|3>
    </itemize>

    <item*|case 3>invalid index

    <\itemize>
      <item><inactive|<range|1234|a|3>> \<rightarrow\> <range|1234|a|3>

      <item><inactive|<range|1234|1|b>> \<rightarrow\> <range|1234|1|b>

      <item><inactive|<range|1234|a|b>> \<rightarrow\> <range|1234|a|b>
    </itemize>
  </description>

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