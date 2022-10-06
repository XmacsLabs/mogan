<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <\tmdoc-title>
    键盘配置
  </tmdoc-title>

  <TeXmacs>键盘的行为模式由<menu|Edit|Preferences>中的一些用户选项指定：

  <\itemize>
    <item><hlink|<menu|Look and feel>|man-preferences.en.tm#preferences:look-and-feel>旨在提供和其他平台类似的快捷键与动作绑定的规则。

    <item>细微的调整可在<hlink|<menu|Edit|Preferences|Keyboard>|man-preferences.en.tm#preferences:keyboard>中解决。
  </itemize>

  下面要讨论的是在不同系统上的键盘配置问题。

  关于快捷键在文档中的显示，请阅读<hlink|惯例|../start/man-conventions.zh.tm>。另外，键盘快捷键的相关细节，请参考<hlink|精通键盘|../text/keyboard/man-keyboard.zh.tm>这一节。

  <paragraph*|一致的标准键位>

  <TeXmacs>已经尽量使快捷键和其它平台一致。然而为了方便用户，<TeXmacs>保留一些通用的键盘快捷键：

  <\itemize>
    <item><key|F5>\U<key|F12> 保留用作特殊用途

    <item><prefix|std>、<key|Win+>和<prefix|A->用作修饰键

    <item><TeXmacs>自带大量一个或多个修饰键辅以<key|left>,
    <key|right>, <key|up>, <key|down>, <key|home>, <key|end>, <key|pageup>,
    <key|pagedown>, <key|backspace>, <key|delete>, <key|space>,
    <key|tab>或<key|return>的快捷键。
  </itemize>

  <paragraph*|潜在的冲突>

  <TeXmacs>专用的快捷键基本不会和惯例冲突。然而，在表格<nbsp><reference|kbd-conflict-table>中，我们还是可以列出一些标准的快捷键。这些快捷键在其他应用程序中可以工作，但是在<TeXmacs>中不奏效。（注：表格中列出的快捷键并不完全符合。）

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|24|24|1|4|cell-halign|l>|<cwith|27|27|1|4|cell-halign|l>|<cwith|6|6|1|4|cell-halign|l>|<cwith|8|8|1|4|cell-halign|l>|<cwith|7|7|3|4|cell-halign|l>|<cwith|7|7|3|4|cell-halign|l>|<cwith|8|8|3|4|cell-halign|l>|<cwith|8|8|3|4|cell-halign|l>|<cwith|11|11|1|4|cell-halign|l>|<table|<row|<cell|快捷键风格>|<cell|快捷键>|<cell|备选>|<cell|作用>>|<row|<cell|Emacs>|<cell|<key|F10>>|<cell|>|<cell|在窗口中显示菜单栏>>|<row|<cell|Emacs>|<cell|<key|M-!>>|<cell|>|<cell|Shell命令>>|<row|<cell|Emacs>|<cell|<key|M-'>/<key|M-`>/<key|M-^>>|<cell|>|<cell|Needed
  for <TeXmacs> accents>>|<row|<cell|Emacs>|<cell|<key|M-/>/<key|M-\\>/<key|M-:>/<key|M-;>>|<cell|>|<cell|>>|<row|<cell|Emacs>|<cell|<key|C-left>/<key|C-right>>|<cell|<key|M-left>/<key|M-right>>|<cell|以单词为单位前进/后退>>|<row|<cell|Emacs>|<cell|<key|C-up>/<key|C-down>>|<cell|>|<cell|以段落为单位前进/后退>>|<row|<cell|Emacs>|<cell|<key|M-b>/<key|M-f>>|<cell|>|<cell|以字符为单位前进/后退>>|<row|<cell|Emacs>|<cell|<key|M-l>/<key|M-t>>|<cell|>|<cell|Locase/transpose
  words (not impl.)>>|<row|<cell|Windows>|<cell|<key|F5>>|<cell|>|<cell|Refresh/Switch
  to next pane>>|<row|<cell|Windows>|<cell|<key|F6>/<key|C-F6>/<key|C-S-F6>>|<cell|>|<cell|Switch
  to next/previous pane/tab>>|<row|<cell|Windows>|<cell|<key|C-space>>|<cell|>|<cell|Remove
  formatting>>|<row|<cell|Windows>|<cell|<key|C-tab>>|<cell|>|<cell|Switch to
  next child window>>|<row|<cell|Windows>|<cell|<key|C-backspace>/<key|C-delete>>|<cell|>|<cell|Delete
  word>>|<row|<cell|Mac OS>|<cell|<key|C-F5>/<key|C-F6>/<key|C-S-F6>>|<cell|>|<cell|Move
  focus to toolbar/panels>>|<row|<cell|Mac
  OS>|<cell|<key|C-F7>>|<cell|>|<cell|Override keyboard access
  mode>>|<row|<cell|Mac OS>|<cell|<key|F9>/<key|F10>>|<cell|>|<cell|Tile or
  untile windows>>|<row|<cell|Mac OS>|<cell|<key|F11>/<key|F12>>|<cell|>|<cell|Hide
  or show windows/dashboard>>|<row|<cell|Mac
  OS>|<cell|<key|tab>/<key|S-tab>>|<cell|>|<cell|Navigate through
  controls>>|<row|<cell|Mac OS>|<cell|<key|C-tab>,
  <key|C-S-tab>>|<cell|>|<cell|Move focus within control
  groups>>|<row|<cell|Mac OS>|<cell|<key|C-space>/<key|M-C-space>>|<cell|>|<cell|Toggle
  between input sources>>|<row|<cell|Mac OS>|<cell|<key|C-left>/<key|C-right>>|<cell|<key|structured:move
  left>/<key|structured:move right>>|<cell|Move one cell left/right in
  table>>|<row|<cell|Mac OS>|<cell|<key|C-up>/<key|C-down>>|<cell|<key|structured:move
  up>/<key|structured:move down>>|<cell|Move one cell up/down in
  table>>|<row|<cell|Mac OS>|<cell|<key|home>/<key|end>>|<cell|<key|M-up>/<key|M-down>>|<cell|Move
  to start/end of document>>|<row|<cell|Mac OS>|<cell|<key|A-pageup>,
  <key|C-up>, <key|C-pageup>>|<cell|<key|pageup>>|<cell|Page
  up>>|<row|<cell|Mac OS>|<cell|<key|A-pagedown>, <key|C-down>,
  <key|C-pagedown>>|<cell|<key|pagedown>>|<cell|Page down>>|<row|<cell|Mac
  OS>|<cell|<key|C-a>/<key|C-e>>|<cell|<key|A-up>/<key|A-down>>|<cell|Move to
  start/end of block>>>>>>
    <label|kbd-conflict-table>一些在其他软件中可用、在<TeXmacs>不可用的快捷键
  </big-table>

  <paragraph*|优先的系统全局快捷键>

  除了上述标准快捷键，系统的全局应用会定义一些额外的全局快捷键，这些快捷键会覆盖掉<TeXmacs>的快捷键。比如，<name|Mac
  OS X>中<name|Spaces>这个应用会用<key|C-left>, <key|C-right>,
  <key|C-up>, <key|C-down>, <key|C-1>, <key|C-2>,
  <key|C-3>和<key|C-4>在多个屏幕中切换。

  这个问题的一种解决方案是在相应的应用中改变出问题的全局快捷键。比如，<name|Spaces>可以把前缀<prefix|C->改为<prefix|M-A-C->。另外，<TeXmacs>没有使用<prefix|fn>这个键。

  如果你不能或不想改变全局的快捷键，你可以用<rigid|<key*|escape>>来产生和<prefix|M->,
  <prefix|A->与<prefix|C->等价的修饰键。所以，你可以输入<key*|escape
  escape right>以在<TeXmacs>产生<key|C-right>的效果。表格<nbsp><reference|kbd-escape-table>所示即可以用<key*|escape>得到的修饰键及组合。

  <\big-table|<descriptive-table|<tformat|<table|<row|<cell|快捷键>|<cell|修饰键>>|<row|<cell|<key*|escape>>|<cell|<key|escape>>>|<row|<cell|<key*|escape
  escape>>|<cell|<key|escape escape>>>|<row|<cell|<key*|escape escape
  escape>>|<cell|<key|escape escape escape>>>|<row|<cell|<key*|S-escape>>|<cell|<key|S-escape>>>|<row|<cell|<key*|S-escape
  S-escape><space|1em>>|<cell|<key|S-escape
  S-escape>>>|<row|<cell|<key*|S-escape S-escape
  S-escape>>|<cell|<key|S-escape S-escape S-escape>>>>>>>
    <label|kbd-escape-table>快捷键和对应修饰键或修饰键组合
  </big-table>

  <paragraph*|用户自定义快捷键>

  <TeXmacs>默认的快捷键不合你的心意时，你还可以<hlink|配置你自己的快捷键|../scheme/man-keyboard.zh.tm>.

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>