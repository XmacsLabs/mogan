<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|结构化光标移动>

  <TeXmacs>实现了三种主要的结构化光标移动的机制

  <\enumerate>
    <item>整个文档结构的遍历

    <item>相似标记的遍历

    <item>最深处标记的移动
  </enumerate>

  大部分结构化光标移动的快捷键支持与<prefix|S->组合适用，以在移动的同时选中文本。

  <todo|customizing the behaviour>

  <paragraph*|结构化遍历文档>

  <shortcut|(traverse-left)>， <shortcut|(traverse-right)>，
  <shortcut|(traverse-up)> and <shortcut|(traverse-down)>用于结构化遍历整个文档。在普通的文本中，<shortcut|(traverse-left)>和<shortcut|(traverse-right)>用于按词移动，
  而<shortcut|(traverse-up)>和<shortcut|(traverse-down)>用于按段落移动。

  在其它标记中，<shortcut|(traverse-left)>和<shortcut|(traverse-right)>用于移动到文档中光标可访问的位置，在您持续在文本间按词移动时，则没有这个效果。<shortcut|(traverse-up)>和<shortcut|(traverse-down)>行为更显上下文相关性，在矩阵中，可用于将某行向上或者向下移动。

  <paragraph*|结构化遍历相似标记>

  此类光标移动是为了方便在文档中访问与当前最深处的标签相似的标签。<shortcut|(traverse-previous)>，<shortcut|(traverse-next)>对应与上一个和下一个，而<shortcut|(traverse-first)>和<shortcut|(traverse-last)>对应于第一个和最后一个。

  比如，当光标处于某节的标题中，用<shortcut|(traverse-previous)>可以跳转到前一节的标题中。用<key|C->则可以跳回先前的标签处。

  <paragraph*|最深处标签的移动>

  也可以在最深处的标签内移动而不必退出。<shortcut|(structured-left)>，
  <shortcut|(structured-right)>， <shortcut|(structured-start)>和<shortcut|(structured-end)>对应于跳转到光标的前一个，后一个，第一个，最后一个参数处。而<shortcut|(structured-exit-left)>或<shortcut|(structured-exit-right)>用于从左边或者右边跳出标签。

  在特定场景下，这些默认行为可能无法生效。比如，在表格或树中，这些快捷键对应于按单元格或者按节点的光标移动。除此之外，垂直方向上则使用<shortcut|(structured-up)>，<shortcut|(structured-down)>，<shortcut|(structured-top)>和<shortcut|(structured-bottom)>移动光标。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>