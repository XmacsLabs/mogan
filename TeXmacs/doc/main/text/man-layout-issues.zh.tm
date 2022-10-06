<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|布局问题>

  一般情况下，<TeXmacs>会自己处理文档的布局。所以，尽管我们不想禁止，但是我们也不鼓励你手动排版文档。比如，你不应该插入空格或者空行作为水平方向上字与字之间的空白或者竖直方向上行与行之间的空白。额外的空白应该用<menu|Insert|Space>明确指定。这可以让你的文档更加鲁棒，在对文档做一些影响到断行、分页的细小改动或者做一些诸如改变文档样式的重大改动时，你就不需要重新考虑布局了。

  <TeXmacs>实现了多种精确插入空白的命令。首先，你可以精确地插入指定高度和宽度的空白。水平方向上的空白无需高度这一属性，而且可以是可伸缩的。可伸缩的空白的长度由段落的断行方式决定。更进一步，你还可以插入制表符类型的空白。竖直方向上的空白可以在段落中任意位置插入，根据类型的不同会插入到段落的上面或者下面，我们建议将插入点放在段落起始处或结束处。两个段落间额外的竖直空白高度是其间的竖直空白高度的最大值（和<TeX>相反，这可以防止两个连续定理之间多余的空白）。

  至于段落的布局，用户可以指定段落的对齐方式（自调整、左对齐、中央对齐、右对齐）、边白和段落首行（末行）的向左（右）缩进。用户还可以控制行间以及段落间的间距。

  页面布局的菜单在<menu|Document|Page>可以找到。首先，你可以指定页面在屏幕上的展示方式：在<menu|Document|Page>的格式选项卡下页面渲染选项中选中「paper」作为页面类型，你就可以看到明确的分页。默认页面类型「papyrus」在你撰写文档时不会分页。页面类型「automatic」假定你的页面大小正好是你的窗口大小。页面的边白和文本宽度在<menu|Document|Page|Margins>中指定。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>