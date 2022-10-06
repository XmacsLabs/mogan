<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|开始作图>

  点击<menu|Insert|Image|Draw image>开始作图。在某些情况下，你也许想在已有的图片（或者其他内容）上绘图。此时，你可以先选中该区域，再点击<menu|Insert|Image|Draw
  over selection>。

  插入图片的默认宽度是整个段落宽度。使用快捷键<shortcut|(graphics-decrease-hsize)>，
  <shortcut|(graphics-increase-hsize)>，
  <shortcut|(graphics-decrease-vsize)>，
  <shortcut|(graphics-increase-vsize)>可以调整图片的尺寸。使用<shortcut|(graphics-decrease-hsize-fast)>，
  <shortcut|(graphics-increase-hsize-fast)>，
  <shortcut|(graphics-decrease-vsize-fast)>，
  <shortcut|(graphics-increase-vsize-fast)>调整得更快。你也可以通过<menu|Insert|Geometry|Size>精确指定图片的长和宽。绘图完毕，你可以使用<menu|Insert|Geometry|Crop>自动剪裁掉绘图内容上下左右的空白（当然，会留下一部分作为补白）。

  对于技术类的图，用坐标纸来绘制是再好不过的了。点击<menu|Insert|Grid|Type|Cartesian>即可。在菜单<menu|Insert|Grid>中，还可以调整坐标和网格线的颜色以及网格的单位长度。打印时，网格默认会被打印出来。如果不希望打印网格，那么打印前需要先移除网格。

  <TeXmacs>默认将网格的原点置于屏幕的中央并且使用1cm作为单位长度。使用<shortcut|(graphics-move-origin-left)>，
  <shortcut|(graphics-move-origin-right)>，
  <shortcut|(graphics-move-origin-up)>，
  <shortcut|(graphics-move-origin-down)>可以控制图片在网格上的移动，使用<shortcut|(graphics-move-origin-left-fast)>，
  <shortcut|(graphics-move-origin-right-fast)>，
  <shortcut|(graphics-move-origin-up-fast)>，
  <shortcut|(graphics-move-origin-down-fast)>则可以加大移动的步伐。通过<menu|Insert|Geometry|Unit>可以改变网格的单位长度。使用<shortcut|(graphics-zoom-in)>和<shortcut|(graphics-zoom-out)>可以放缩图片，对应的菜单项是<menu|Insert|Geometry|Zoom>。

  <tmdoc-copyright|2012|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>