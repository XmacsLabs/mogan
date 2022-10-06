<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|在文档中输入数学公式>

  <TeXmacs>对数学公式有着非常强大的支持。
  <TeXmacs>下有三种输入数学公式的方式：

  <\description>
    <item*|<with|font|fireflysung|<menu|插入|数学|公式>>或者<key|$>>

    这个菜单条目输入的是像<math|a<rsup|2>+b<rsup|2>=c<rsup|2>>那样嵌在文本段落中的简短公式。注意公式是一种特殊的排版方式，不会占用过多的垂直方向上的空间。比如求和符号的上下界限往往处于右边（<math|<big|sum><rsup|n><rsub|i=1>>）。此时选中整个数学公式，再在<menu|Format|Mathematics|Display
    style>中选择打开，求和符号的界限就会显示在其上下两侧（<with|math-display|true|<math|<big|sum><rsup|n><rsub|i=1>>>）。缺省情况下，展示风格是关闭的。

    <item*|<menu|插入|数学|单行公式>或者<shortcut|(make-equation*)>>

    这个条目适合输入较长的公式，比如：

    <\equation*>
      x<rsup|n>+y<rsup|n>=z<rsup|n>,
    </equation*>

    公式的排版会占据整个段落。使用快捷键<shortcut|(numbered-toggle
    (focus-tree))>可以给公式加上编号（或者删去编号）。快捷键<shortcut|(variant-circulate
    (focus-tree) #t)>能够让公式在行内模式和单行模式间切换。

    <item*|<menu|插入|数学|多行公式>或者<shortcut|(make-eqnarray*)>>

    这个条目创建了一个<markup|eqnarray<math|<rsup|\<ast\>>>>，一个三列宽的类似表格的环境（详见<hlink|创建表格|../../table/man-create-table.zh.tm>）。在这个环境中可以方便地输入多行公式，比如：

    <\eqnarray*>
      <tformat|<table|<row|<cell|x+0>|<cell|=>|<cell|x>>|<row|<cell|x+<around*|(|-x|)>>|<cell|=>|<cell|0>>|<row|<cell|x+y
      >|<cell|=>|<cell|y+x>>|<row|<cell|<around*|(|x+y|)>+z>|<cell|=>|<cell|x+<around*|(|y+z|)>>>>>
    </eqnarray*>

    第一列右对齐，第二列居中，第三列左对齐。
    <markup|eqnarray<math|<rsup|\<ast\>>>>环境的典型用途就是显示多步计算：

    <\eqnarray*>
      <tformat|<table|<row|<cell|<around*|(|e<rsup|sin x>+sin<rsup|>
      e<rsup|x>|)><rprime|'>>|<cell|=>|<cell|<around*|(|e<rsup|sin
      x>|)><rprime|'>+<around*|(|sin e<rsup|x>|)><rprime|'>>>|<row|<cell|>|<cell|=>|<cell|<around*|(|sin
      x|)><rprime|'>e<rsup|sin x>+<around*|(|e<rsup|x>|)><rprime|'>cos
      e<rsup|x>>>|<row|<cell|>|<cell|=>|<cell|e<rsup|sin x> cos
      x\<noplus\>+e<rsup|x> cos e<rsup|x>,>>>>
    </eqnarray*>

    在这个例子中，大多数行的第一列是空着的。
  </description>

  <tmdoc-copyright|1998\U2022|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>