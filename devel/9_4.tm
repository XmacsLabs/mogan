<TeXmacs|2.1.2>

<style|tmdoc>

<\body>
  <tmdoc-title|corrupted label display for Chinese>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Darcy Shen

    <item>Mogan Editor Version: v1.1.1
  </itemize>

  <section|Description>

  <paragraph|All of the characters are CJK>

  <label|\<#4E2D\>\<#6587\>\<#6807\>\<#7B7E\>>

  <paragraph|Not all of the characters are CJK>

  <label|bib-\<#8BD7\>\<#7ECF\>>

  <section|How to fix?>

  We need a predicate to test if a label contains Chinese characters and then
  use the proper font to render it.

  <section|Limitation>

  This fix only works for Chinese. Here is a possible improvements for
  Japanese:

  <slink|https://zhuanlan.zhihu.com/p/613487071>

  <\tmdoc-copyright|2022>
    Darcy Shen
  </tmdoc-copyright>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|info-flag|detailed>
  </collection>
</initial>