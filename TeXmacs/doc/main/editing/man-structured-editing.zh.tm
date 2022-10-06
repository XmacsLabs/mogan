<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|结构化编辑>

  作为普适法则，几乎所有的结构化编辑命令是上下文相关的，从文档源代码的角度上讲，结构化编辑的效果由光标所在结构最深处的标记所决定。当您选中某些文本时，焦点会定位于选中结构最深处的标记。在诸如相似标记间导航的结构化操作中，当前的焦点会临时设置为其它某处。当前的焦点由光标所在结构中最深处的青盒子显式指示。

  比如，结构化插入命令<shortcut|(structured-insert-left)>，
  <shortcut|(structured-insert-right)>， <shortcut|(structured-insert-up)>
  和 <shortcut|(structured-insert-down)>
  在表格和树中含义不同。在表格中是插入行与列(见图
  <reference|matrix-insert-fig>)。在树中是插入新节点(见图
  <reference|tree-insert-fig>)。倘若您的光标在表格中的树中，则从文档的源代码上说，最里面的标记实际上是树，所以使用结构化插入将优先插入节点，而不是行与列。

  在大多数情况下，结构化操作下标记的默认行为已预定义。比如<shortcut|(structured-insert-left)>
  或<shortcut|(structured-insert-right)>是在标记的左边或者右边插入一个新的参数。

  <\big-figure>
    <\equation*>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b<cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|5em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|<cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|<cursor>>|<cell|b>|<cell|c>>|<row|<cell|d>|<cell|>|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|c>>|<row|<cell|>|<cell|<cursor>>|<cell|>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|>|<cell|<cursor>>|<cell|>>|<row|<cell|a>|<cell|b>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>>
    </equation*>
  <|big-figure>
    <label|matrix-insert-fig>假定光标在最左矩阵的<cursor>处。那个右边的四个矩阵按顺序分别对应往左边(<shortcut|(structured-insert-left)>)插入一新列，往右边(<shortcut|(structured-insert-right)>
    )插入一新列，往下边(<shortcut|(structured-insert-down)>)插入一新行，往上边(<shortcut|(structured-insert-up)>)插入一新行。
  </big-figure>

  <\big-figure|<tree|a|b|c<cursor>|d><space|3em><space|3em><tree|a|b|<cursor>|c|d><space|3em><tree|a|b|c|<cursor>|d><space|3em><tree|a|b|<tree|<cursor>|c>|d><space|3em><tree|a|b|<tree|c|<cursor>>|d>>
    <label|tree-insert-fig>假定光标位于最左边树的 <cursor>
    处。那么右边的四棵树分别对应<shortcut|(structured-insert-left)>，<shortcut|(structured-insert-right)>，<shortcut|(structured-insert-up)>，<shortcut|(structured-insert-down)>后的效果
  </big-figure>

  类似地，在矩阵中，<shortcut|(structured-insert-start)>，
  <shortcut|(structured-insert-end)>， <shortcut|(structured-insert-top)>
  和 <shortcut|(structured-insert-bottom)>可用于插入新的首列，末列，首行，末行。<shortcut|(structured-remove-left)>
  和 <shortcut|(structured-remove-right)>可用于结构化后向或前向删除。在矩阵中，这将删除光标前或者后的一列(见图<reference|matrix-remove-fig>)。使用<shortcut|(remove-structure-upwards)>或者<shortcut|(remove-structure-upwards)>可删除光标所在标记外的环境。

  <\big-figure>
    <\equation*>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b<cursor>>|<cell|c>>|<row|<cell|d>|<cell|e>|<cell|f>>>>><space|5em><matrix|<tformat|<table|<row|<cell|b<cursor>>|<cell|c>>|<row|<cell|e>|<cell|f>>>>><space|2em><matrix|<tformat|<table|<row|<cell|a>|<cell|<cursor>c>>|<row|<cell|d>|<cell|f>>>>><space|2em>b<cursor>
    </equation*>
  <|big-figure>
    <label|matrix-remove-fig>假定光标在最左矩阵的<cursor>处。中间两个矩阵对应按下<shortcut|(structured-remove-left)>和<shortcut|(structured-remove-right)>后的结果。最右边的矩阵是按下<shortcut|(remove-structure-upwards)>或者<shortcut|(remove-structure-upwards)>后的效果，其作用是用单元格内的内容替换掉整个矩阵结构。
  </big-figure>

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>