<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|搜索和替换>

  使用<key|C-s>或者<menu|Edit|Search>开始搜索文本。在搜索中，搜索字符在底端状态栏的左侧显示。你输入的每一个字符都将附加到搜索字符的末尾，文本中匹配的字符以细线红框围绕显示。在搜索过程再次按下<key|C-s>，搜索将继续进行。如果在下文中没有找到匹配项，<TeXmacs>什么也不做，底端的状态栏会显示相关信息，此时再次按下<key|C-s>，搜索将从正文起始处重新进行。按下<key|Backspace>可在搜索文本中撤销上一个字符输入。

  通常情况下，搜索方向为自光标处往下。使用<key|C-r>可以自光标处往上搜索。在搜索中，只匹配相同模式和相同语言下的文本。模式和语言在您开始搜索时有光标所在处决定。这意味着，如果你在数学模式中搜索<math|x>，那么文本中普通的x是不会被匹配到的。当前的局限之处在于，所搜索的文本只能是普通文本而不是数学符号或者更加复杂的结构化文本。

  使用<key|C-=>或者<menu|Edit|Replace>可查询并替换文本。开始替换时，在底端先请您输入被替换文本，再输入替换文本。每一次匹配到文本时，底端状态栏询问您选择(y)替换文本(n)不替换文本(a)替换此处及所有匹配处三者之一。和搜索一样，查询-替换命令对模式和语言敏感。

  当前的搜索和替换系统还十分基础，仅仅是对普通文本的简单处理。未来，我们计划对结构化文本实现一种更加强大的搜索和替换机制。

  当前，您还可以用以下方式搜索和替换文档中的文本片段：在您的文档中某处或者是其他的窗口中选中你想搜索的文本，点击菜单项<menu|Edit|Copy
  to|Search>，接下来按<key|C-s>便可用选中的文本搜索。类似地，选中文本，点击菜单项<menu|Edit|Copy
  to|Replace>，再次选中另一个文本，接下来按<key|C-=>就能以前后两次选中的文本为参数执行替换操作。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>