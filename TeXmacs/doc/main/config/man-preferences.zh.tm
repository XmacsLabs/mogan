<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|用户首选项>

  你或许需要将<TeXmacs>调整为符合自己需求的样子，以寻求极致的输入体验。这一切都可以在<menu|Edit|Preferences>中搞定。首先，在<menu|Edit|Preferences|Look
  and feel>中选择适合自己的快捷键方案。这些方案和你在其他平台上已经适应的方案所差无几。

  有如下用户首选项以供配置:

  <\description>
    <item*|<menu|Look and feel>><label|preferences:look-and-feel>这个选项主要影响<TeXmacs>的快捷键风格。默认风格依据系统而定（<name|Linux>下的Gnome、KDE或Emacs，<name|Mac
    OS>下的Mac OS，<name|Windows>下的Windows）。Emacs快捷键风格可以作为任何系统下的可选项。

    更多细节请参考<hlink|不同系统上的快捷键配置|man-config-keyboard.zh.tm>一文。

    <item*|<menu|Interactive questions>>这个选项指定了需要用户输入时界面的弹出方式，或者是单独的弹出式窗口，或者是显示在<TeXmacs>的状态栏上。

    <item*|<menu|Details in menus>>这个选项指定了菜单的复杂度，如果选中<menu|Simplified
    menus>，不常用的选项会自动隐藏。

    <item*|<menu|Language>>指定了界面和<TeXmacs>帮助文档的首选语言。

    <item*|<menu|Keyboard>><label|preferences:keyboard>除了快捷键风格，还有一些其他的设置决定了键盘输入的行为模式：

    <\itemize>
      <item><menu|Cyrillic input method>指定了<hlink|输入西里尔字母的方式|man-russian.en.tm>

      <item><menu|Automatic quotes>功能

      <item><menu|Automatically close brackets>功能
    </itemize>

    <item*|<menu|Printer>>打印相关可在此子菜单设置。

    <item*|<menu|Security>>理论上，<TeXmacs>可以被嵌入宏和超链接，造成可以执行任意命令的险况。实际情况下，这个特性可能带来安全风险。所以，<menu|Security>这个选项允许用户指定对不受信任的可执行代码的处理方式。

    <item*|<menu|Converters>><TeXmacs>与其他格式的转换可在此菜单下配置。
    更多细节参见<hlink|和其他格式的兼容性|../convert/man-convert.en.tm>这一章。

    <item*|<menu|Scripts>>指定外部脚本的默认脚本语言。

    <item*|<menu|Autosave>>指定自动保存的时间间隔。
  </description>

  <tmdoc-copyright|1998\U2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>