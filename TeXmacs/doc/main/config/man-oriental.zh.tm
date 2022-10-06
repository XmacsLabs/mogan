<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|东方语言用户使用须知>

  首先，选择合适的中文输入法，对于macOS或者Windows，使用系统默认输入法即可；对于Linux，推荐使用<name|Fcitx>。

  其次，检查<menu|Document|Language>，如果下拉菜单中的<menu|简体中文>菜单项处于灰色不可用状态，那么说明<TeXmacs>在当前系统没有检测到合适的默认中文字体。此时，下载Fandol字体，并将解压后的字体文件夹拷贝到该目录：

  <verbatim| \ \ \ $TEXMACS_HOME_PATH/fonts/truetype>

  然后点击<menu|Tools|Font|Clear font
  cache>，最后重启<TeXmacs>就可以解决这个问题。

  最后，设置<menu|Chinese>环境。其一，首选项中选择<menu|Chinese>作为界面语言，注意该选项只影响菜单项和帮助文档的显示。其二，在菜单项<menu|Document|Language>中选择<menu|Chinese>，否则中文可能无法显示，尤其会影响到段落的换行。当文档语言非简体中文时，还可以通过<menu|Format|Language>为选中区域设置语言。

  <tmdoc-copyright|1998\U2021|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>