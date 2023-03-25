<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|comment>>

<\body>
  <tmdoc-title|Upgrade pdf hummus to avoid export failure>

  <section|Feature metadata>

  <\itemize>
    <item>Reporter: <hlink|斩夜神|https://gitee.com/dhuzf> on
    <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I6B6AH>

    <item>Owner: jingkaimori
  </itemize>

  <section|Description>

  本次提交更改了xmake.lua文件，使用xmake构建时，将安装版本较新的PDFHummus，代替内嵌的版本。本次提交不会更改cmake的行为，使用cmake构建的版本仍然存在bug。

  更新PDFHummus后，Hummus内部解码文件名的部分改变，因此该bug消失。

  \;

  <tmdoc-copyright|2023|jingkaimori>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|font-base-size|10>
    <associate|page-medium|papyrus>
    <associate|preamble|false>
  </collection>
</initial>