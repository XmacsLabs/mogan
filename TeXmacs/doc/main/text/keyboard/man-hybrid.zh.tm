<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|混合命令和<LaTeX>模拟>

  <TeXmacs>允许你直接从键盘输入<LaTeX>命令。具体步骤如下：你先按下<key|\\>以进入<LaTeX>/<TeXmacs>混合命令模式，再输入你需要执行的命令。当你输完命令时，底部的状态栏左边会显示如下信息：

  <\verbatim>
    \ \ \ \ \<less\>return\<gtr\>: action to be undertaken
  </verbatim>

  此时当你按下回车键，你的命令就会被执行。比如在数学模式中，你可以通过输入<key|\\
  f r a c return>创建分数。

  如果你输入的命令不是一个（可识别的）<LaTeX>命令，我们首先看该命令是否是一个存在的<TeXmacs>宏，函数或者环境（由样式文件提供）。如果是这样，相应的宏展开，函数或者环境将被应用。否则，我们认为你的命令对应于一个环境变量且我们提取其值。<key|\\>键总是等价于这些命令<key|inactive
  l>，<key|inactive e>，<key|inactive a>，<key|inactive
  #>或者<key|inactive v>之一。

  使用<key|symbol \\>可以输入字面意义上的\\(backslash)。

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>