<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|基本前缀规则>

  由于快捷键多而杂，将它们分类将便于记忆。这里按照键前缀来分类。当前激活的键前缀依赖于<menu|Edit|Preferences>中的快捷键风格。主要的键前缀如以下所示:

  <\description>
    <item*|<prefix|std>>标准快捷键。比如，在Emacs快捷键风格下，<shortcut|(kbd-paste)>可以用来粘贴文本。

    <item*|<prefix|cmd>><TeXmacs>快捷键，往往依赖于当前的编辑模式。比如，
    <key|text s> 在文本模式下产生strong文本，在数学模式下则是<math|<sqrt|>>（平方根）。

    <item*|<prefix|altcmd>>组合式<TeXmacs>快捷键。
    通常，这些快捷键是用于先指示命令将要应用的标记的类别再指定具体的命令。比如<key|executable>键前缀用于插入可执行的标记以用于<hlink|撰写样式文件|../../../devel/style/style.en.tm>。一个例子是快捷键<key|executable
    +>用于插入加法。

    <item*|<prefix|structured:geometry>>此键前缀用于和方向键及其它特殊键组合，用于<hlink|移动和缩放对象|../../editing/man-structured-geometry.zh.tm>。

    <item*|<prefix|structured:move>>此键前缀用于和方向键及其它特殊键组合，用于<hlink|结构化光标移动|../../editing/man-structured-move.zh.tm>。

    <item*|<prefix|special>>此键前缀偶用于和字母和标点符号组合以创建一些额外的简单易记的快捷键。

    <item*|<prefix|symbol>>此键前缀可和常用字符组合使用以插入特殊符号。比如，使用<key|text:symbol
    s>得到<with|font|roman|>以及<key|math:symbol
    a>得到<math|<op|\<amalg\>>>。<prefix|symbol>键前缀还用于插入字面意义上的字符。比如，<key|symbol
    \\>总是产生\\，而 <key|\\>用于进入<hlink|混合命令|man-hybrid.zh.tm>。
  </description>

  不巧的是， 基于<prefix|altcmd>快捷键在一些系统上面被系统的快捷键所覆盖。比如，在Mac
  OS上重音字符和常见的特殊字符是使用该快捷键输入的。此时，你可以使用<key*|escape>作为替代。更多信息，参见<hlink|配置键盘|../../config/man-config-keyboard.en.tm>。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>