<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|撤销和恢复>

  <TeXmacs>只在运行时支持撤销和恢复操作。您可以使用菜单项<menu|Edit|Undo>和<menu|Edit|Redo>以及相应快捷键<shortcut|(undo
  0)>和<shortcut|(redo 0)>来控制您对文档的改变。

  为了节约内存空间，默认的撤销和恢复操作限于百步之内。在您的配置文件中添加

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  这行代码，可以深度改为1000。若是将深度赋值为负值，则任何数量的操作都能被撤销。

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>