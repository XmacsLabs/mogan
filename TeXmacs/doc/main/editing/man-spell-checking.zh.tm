<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|拼写检查>

  在您的系统上已安装<shell|ispell>的前提下，使用<shortcut|(interactive-spell)>
  或者<menu|Edit|Spell>可以检查您的文本中是否有拼写错误。使用前，还请您先检查一下您所用语言的字典是否已经安装。

  对全文或者选中文本启用拼写后，您将会在底端收到错词的修正提示，有以下选项供您选择：

  <\description>
    <item*|a)>接受错词，这样，以后的检查中将忽略该词

    <item*|r)>使用您输入的单词替换该词

    <item*|i)>申明该词其实是正确的，并将该词添加到您的个人词典

    <item*|1-9)>对错词的推荐修正选项
  </description>

  请注意，<shell|ispell>只管拼写对错，并不检查句子的语法正确与否。

  启动拼写检查后，设定语言是当前光标所在处的语言或者是选中区域的起始处的语言。拼写检查只会检查设定语言的拼写问题。这意味着，如果文档中有多种语言，您需要对每一个部分分别启用拼写检查。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>