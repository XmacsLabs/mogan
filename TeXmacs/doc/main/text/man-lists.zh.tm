<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|列表>

  <menu|Insert|Itemize>即插入无序列表。可选择 <math|\<bullet\>>
  (圆点)， <math|<op|->> (短线) 或者 <math|<op|\<rightarrow\>>>
  (箭头) 之一作为条目的标记。如下所示，列表中还可嵌入子列表：

  <\itemize>
    <item>第一项。

    <item>下面是子列表：

    <\itemize>
      <item>一个子项。

      <item>另一个子项。
    </itemize>

    <item>最后一项。
  </itemize>

  默认的标记根据其层次自动以不同样式显示。在最外面一层，我们使用<math|\<bullet\>>，第二层则用<math|<op|\<circ\>>>。光标在列表中时，按下<key|return>键将自动新建一个新项目。如果您的项目内容由多个段落构成，则可以用<shortcut|(kbd-shift-return)>新起一段。

  有序列表可用菜单项<menu|Insert|Enumerate>得到，与无序列表类似，只是各个项目是顺序显示的。下面是一个按罗马数字顺序显示的例子(<menu|Insert|Enumerate|Roman>)：

  <\enumerate-Roman>
    <item>第一项

    <item>第二项

    <item>最后一项
  </enumerate-Roman>

  最后一种列表是描述列表。相应的菜单项是
  <menu|Insert|Description> ，功能是演示一串概念：

  <\description>
    <\with|font|roman>
      <item*|Gnu>A hairy but gentle beast.

      <item*|Gnat>Only lives in a zoo.
    </with>
  </description>

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>