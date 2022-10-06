<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|移动和缩放对象>

  <prefix|structured:geometry>键前缀用于移动和缩放对象。比如说，在表格中的单元格内部，使用<key|structured:geometry
  right>可使单元格更靠右。在使用<menu|Format|Space>插入的空格中，使用该键可增加空格的宽度。下面是结构化移动和缩放快捷键的通用功能描述:

  <\description>
    <item*|<shortcut|(geometry-left)>>减小对象的水平宽度，或者更向左对齐。

    <item*|<shortcut|(geometry-right)>>增加对象的水平宽度，或者更向右对齐。

    <item*|<shortcut|(geometry-down)>>减小/增加对象的竖直高度，或者更向底部对齐。

    <item*|<shortcut|(geometry-up)>>减小/增加对象的竖直高度，或者更向顶部对齐。

    <item*|<shortcut|(geometry-start)>>减小对象的水平位移或者对齐。

    <item*|<shortcut|(geometry-end)>>增加对象的水平位移或者对齐。

    <item*|<shortcut|(geometry-bottom)>>减小对象的垂直位移或者底部对齐。

    <item*|<shortcut|(geometry-top)>>增加对象的垂直位移或者顶部对齐。

    <item*|<shortcut|(geometry-reset)>>将几何性质(大小，位置，对齐)重设为默认值。

    <item*|<shortcut|(geometry-circulate #t)>，
    <shortcut|(geometry-circulate #f)>>在可用长度单元间循环以指定其几何结构。

    <item*|<shortcut|(geometry-slower)>，
    <shortcut|(geometry-faster)>>减小或者增加移动或者缩放的步长。
  </description>

  下面是一些特定标记下的特定说明:

  <\description>
    <item*|空格>空格指的是<menu|Format|Space>中的水平或者竖直空格。若要使用快捷键，必须把光标置于空格标记之后。

    <item*|盒修饰器><menu|Format|Transform>菜单中的<markup|move>，
    <markup|shift>， <markup|resize>和<markup|clipped>标记

    <item*|动画>动画的长度可用<shortcut|(geometry-left)>和<shortcut|(geometry-right)>更改。

    <item*|图片>其大小和对齐可供更改。
  </description>

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>