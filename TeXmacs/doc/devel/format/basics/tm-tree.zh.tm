<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|<TeXmacs>树>

  所有的<with|font|roman|<TeXmacs>>文档或者文档片段都能看作是树。例如这棵树

  <\equation*>
    <tree|<text|<markup|with>>|mode|math|<tree|<text|<markup|concat>>|x+y+|<tree|<text|<markup|frac>>|1|2>|+|<tree|<text|<markup|sqrt>>|y+z>>>
  </equation*>

  就是下面这个公式的一种典型的表示。

  <\equation>
    <label|tm-tree-ex>x+y+<frac|1|2>+<sqrt|y+z>
  </equation>

  <paragraph|<TeXmacs>树的内部节点>

  <TeXmacs>树的内部节点都是字符串符号，而其叶节点都是普通字符串。字符串符号和字符串的区别可以从这点来辨别：<TeXmacs>内部将每一个符号对应于一个唯一的编号，以便快速判断两个符号是否相同。

  <paragraph|<TeXmacs>树的叶节点>

  当前，所有的字符串都用<em|通用<TeXmacs>编码>来表示。除了\P<verbatim|\<less\>>\Q和\P<verbatim|\<gtr\>>\Q，这种编码和Cork字体的编码一致。特殊扩展字符由起始于\P<verbatim|\<less\>>\Q终止于\P<verbatim|\<gtr\>>\Q字符序列表示。例如，<verbatim|\<less\>alpha\<gtr\>>表示字母\<alpha\>。<em|通用<TeXmacs>编码>中字符的语义不依赖于上下文（目前，西里尔字符是一个例外）。换句话说,通用<TeXmacs>编码可以看作是Unicode的类似物。我们以后可能要把编码转换为Unicode。

  字符串叶节点上可能是普通文本也可能是特殊数据。<TeXmacs>支持下列数据类型：

  <\description>
    <item*|布尔型><verbatim|true>或<verbatim|false>。

    <item*|整型>纯数字序列或者前面带一个负号。

    <item*|浮点型>使用科学计数法表示。

    <item*|长度>浮点型数后加一个<hlink|长度单位|lengths.zh.tm>，比如<verbatim|29.7cm>和<verbatim|2fn>。
  </description>

  <paragraph*|序列化和适用于编辑的语法>

  将文档保存为硬盘上的文件或者拷贝文档片段到剪贴板时，<TeXmacs>树必须表示为字符串。抽象<TeXmacs>树到字符串的无损转换我们称之为<em|序列化>，逆向过程则称之为<em|解析>。<TeXmacs>提供了三种序列化的方法，分别对应于标准<hlink|<TeXmacs>
  格式|tm-tm.zh.tm>，<hlink|XML格式|tm-tmml.zh.tm>和<hlink|<scheme>格式|tm-scm.zh.tm>。

  然而，需要强调的是，修改<TeXmacs>文档的首选语法(syntax)是在编辑器的内部显示的。如果你感到不解，不妨认为语法就是一种使得信息更易于理解和修改的表现形式。一种具体的语法对应着文档在的屏幕上排版展示以及交互式编辑的行为。另外，在菜单项<menu|Document|Source>中，你可以选择显示文档的不同方式，比如不同程度的紧化和编辑样式文件的<hlink|\P源码树\Q模式|../../style/presentation/src-present.en.tm>。

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
    <associate|src-compact|all>
  </collection>
</initial>