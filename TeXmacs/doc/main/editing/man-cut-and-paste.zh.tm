<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|剪切与粘贴>

  使用鼠标左键，您可以选中文本。选中后，点击<menu|Edit|Cut>或者<shortcut|(kbd-cut)>，将会删除选中文本；而点击<menu|Edit|Copy>或者<shortcut|(kbd-copy)>则会留下选中文本。此时，再点击<menu|Edit|Paste>或者<shortcut|(kbd-paste)>，您可以将此前选中的文本粘贴至光标所在位置。注意，您也可以使用鼠标中键快速复制选中区域，只需选中文本，在将光标置于某处，按下鼠标中键，此时此前选中文本被粘贴至此处。

  选中文本后，您还可以改变选中区域的属性。比如，使用鼠标左键选中一些黑色文字，点击<render-menu|Format|<rigid|<render-menu|Color|<tabular*|<tformat|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-lsep|1spc>|<cwith|1|1|1|1|cell-rsep|1spc>|<cwith|1|1|1|1|cell-bsep|1spc>|<cwith|1|1|1|1|cell-tsep|1spc>|<cwith|1|1|1|1|cell-width|1.5em>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-background|red>|<table|<row|<cell|>>>>>>>>，将选中文字变成红色。又如，选中公式后点击<menu|Insert|Fraction>，则此公式将成为新插入分式的分子。

  与其它应用程序之间通过复制和粘贴机制通信时，文本是以<TeXmacs>的数据格式传输的。这意味直接从外部粘贴有格式的文本至<TeXmacs>，容易导致格式混乱。您可以通过<menu|File|Import>和<menu|File|Export>规避这种风险。默认地，复制和粘贴使用文本缓冲区。使用<menu|Edit|Paste
  from>和<menu|Edit|Copy to>，您可以指定选中文本的来源和去向。

  使用键盘选中文本有一下两种方法：其一是按中<prefix|S->不放，使用<key|left>，
  <key|right>在文本上移动。其二是使用<shortcut|(kbd-select-enlarge)>固定一个起始点，再移动光标，移动过程中，起始点和光标所在点之间的区域即为选中区域。按下<shortcut|(selection-cancel)>，您可以退出这种选中的状态。中文用户在GNU/Linux环境下使用的输入法切换和<shortcut|(kbd-select-enlarge)>产生冲突，建议自定义其快捷键。

  注意使用<shortcut|(kbd-select-enlarge)>还可以用于结构化选中。按下<shortcut|(kbd-select-enlarge)>两次，当前光标所在处的单词将被选中。再次按下<shortcut|(kbd-select-enlarge)>，选中的范围将依照结构逐层扩大，比如一开始选中的是单词，下一步可能是段落。最终，将选中全文，此时，再次按下<shortcut|(kbd-select-enlarge)>，选中区域被清空。这里的<shortcut|(kbd-select-enlarge)>，也可以用鼠标左键替代。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>